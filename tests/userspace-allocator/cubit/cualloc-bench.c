/* CuAlloc on CuBit (headless --test bench-cualloc --accel kvm): the libc
 * malloc family over CuAlloc, single-threaded and with POSIX threads
 * (musl over kernel threads). The same traces as the Linux-hosted
 * benchmarks (tests/userspace-allocator), reduced to what CuBit can report:
 * throughput, sampled latency and live rounding waste. No retained-memory
 * figure: CuBit has no per-process resident count yet.
 *
 *   single WORKLOAD: 1024 live slots, free + malloc + first/last byte touch
 *     per pair; every 128th pair timed alone.
 *   threads N: N workers, each its own 1024 slots of 1..4096 bytes, local
 *     free + malloc; wall time / all pairs, plus every 64th pair timed alone.
 *   remote N: as threads, but each worker frees the blocks of the worker
 *     before it (a ring), so frees cross threads.
 *
 * First two checks (a FAIL line ends the run as failed):
 *   reserve: RESERVE_OWNED_MEMORY's alignment argument, the raw syscall:
 *     32 MiB at 32 MiB lands on a 32 MiB boundary, 0 still means a page,
 *     a non-power-of-two alignment is refused.
 *   stress: generations of threads that allocate mixed sizes, fill them
 *     with a pattern, pass every other block through a shared queue to
 *     whichever thread takes it (remote frees), check patterns before
 *     freeing, and exit (their heaps are left; the next generation adopts
 *     them). Reports blocks moved and the arena count's growth.
 */
#define _GNU_SOURCE
#include <malloc.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <cubit/debug.h>

enum { LIVE = 1024, SINGLE_PAIRS = 1000000, THREAD_PAIRS = 400000, MAX_THREADS = 4,
       SINGLE_SAMPLE = 128, THREAD_SAMPLE = 64 };

static void say(const char *text) { cubit_debug_write(text, strlen(text)); }

static uint64_t kernel_call(uint64_t number, uint64_t a0, uint64_t a1) {
    uint64_t result;
    __asm__ volatile("syscall" : "=a"(result) : "a"(number), "D"(a0), "S"(a1)
                     : "rcx", "r11", "memory");
    return result;
}
enum { RESERVE_OWNED_MEMORY = 123, RELEASE_OWNED_RESERVATION = 125 };

static int reserve_check(void) {
    const uint64_t span = 32u << 20;
    char line[160];
    uint64_t a = kernel_call(RESERVE_OWNED_MEMORY, span, span);
    uint64_t b = kernel_call(RESERVE_OWNED_MEMORY, 4096, 0);
    uint64_t c = kernel_call(RESERVE_OWNED_MEMORY, span, 3 * 4096);
    uint64_t d = kernel_call(RESERVE_OWNED_MEMORY, 4096, 1ull << 31);
    int ok = a != 0 && a % span == 0 && b != 0 && b % 4096 == 0 && c == 0 && d == 0;
    if (a) kernel_call(RELEASE_OWNED_RESERVATION, a, span);
    if (b) kernel_call(RELEASE_OWNED_RESERVATION, b, 4096);
    snprintf(line, sizeof line, "cualloc-bench: reserve %s aligned=%#llx page=%#llx odd=%#llx huge=%#llx\n",
             ok ? "ok" : "FAIL", (unsigned long long)a, (unsigned long long)b, (unsigned long long)c,
             (unsigned long long)d);
    say(line);
    return ok;
}

static uint64_t now_ns(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (uint64_t)t.tv_sec * 1000000000u + (uint64_t)t.tv_nsec;
}

static uint64_t next(uint64_t *seed) {
    *seed ^= *seed << 13;
    *seed ^= *seed >> 7;
    *seed ^= *seed << 17;
    return *seed;
}

static size_t size_for(const char *workload, uint64_t *seed) {
    if (!strcmp(workload, "fixed64")) return 64;
    if (!strcmp(workload, "mixed")) return 1 + next(seed) % 4096;
    if (!strcmp(workload, "medium")) return 4097 + next(seed) % (262144 - 4096);
    uint64_t n = next(seed);  /* bimodal */
    return n % 8 == 0 ? 2048 + (n >> 3) % 2049 : 1 + (n >> 3) % 64;
}

static int compare(const void *a, const void *b) {
    uint64_t x = *(const uint64_t *)a, y = *(const uint64_t *)b;
    return x < y ? -1 : x > y;
}

static void report_samples(const char *what, uint64_t *samples, size_t count, double per_pair) {
    char line[256];
    qsort(samples, count, sizeof *samples, compare);
    snprintf(line, sizeof line,
             "cualloc-bench: %s ns_per_pair=%.1f p50=%llu p99=%llu p999=%llu max=%llu\n", what, per_pair,
             (unsigned long long)samples[count / 2], (unsigned long long)samples[count * 99 / 100],
             (unsigned long long)samples[count * 999 / 1000], (unsigned long long)samples[count - 1]);
    say(line);
}

static void single(const char *workload) {
    static unsigned char *slot[LIVE];
    static size_t sizes[LIVE];
    static uint64_t samples[SINGLE_PAIRS / SINGLE_SAMPLE + 1];
    uint64_t seed = 0x123456789abcdefull, start, elapsed;
    size_t count = 0, requested = 0, usable = 0;
    for (int i = 0; i < LIVE; i++) {
        slot[i] = malloc(64);
        sizes[i] = 64;
        if (!slot[i]) { say("cualloc-bench: FAIL out of memory\n"); return; }
    }
    start = now_ns();
    for (int i = 0; i < SINGLE_PAIRS; i++) {
        unsigned k = next(&seed) % LIVE;
        size_t size = size_for(workload, &seed);
        uint64_t t = i % SINGLE_SAMPLE == 0 ? now_ns() : 0;
        free(slot[k]);
        slot[k] = malloc(size);
        if (!slot[k]) { say("cualloc-bench: FAIL out of memory\n"); return; }
        ((volatile unsigned char *)slot[k])[0] = 1;
        ((volatile unsigned char *)slot[k])[size - 1] = 2;
        sizes[k] = size;
        if (i % SINGLE_SAMPLE == 0) samples[count++] = now_ns() - t;
    }
    elapsed = now_ns() - start;
    for (int i = 0; i < LIVE; i++) {
        requested += sizes[i];
        usable += malloc_usable_size(slot[i]);
        free(slot[i]);
    }
    char what[96];
    snprintf(what, sizeof what, "single %s waste=%.1f%%", workload, 100.0 * ((double)usable / requested - 1));
    report_samples(what, samples, count, (double)elapsed / SINGLE_PAIRS);
}

/* The stress: a shared queue of parcels (lock: a pthread mutex). */
enum { STRESS_THREADS = 4, STRESS_GENERATIONS = 8, STRESS_ROUNDS = 20000, QUEUE = 4096, OWN = 64 };
struct parcel { unsigned char *item; size_t size; unsigned char value; };
static struct parcel queue[QUEUE];
static int queue_head, queue_count;
static pthread_mutex_t queue_lock = PTHREAD_MUTEX_INITIALIZER;
static int stress_corrupt, stress_refused;
static long stress_moved;

static int holds(const struct parcel *p) {
    for (size_t i = 0; i < p->size; i++) if (p->item[i] != p->value) return 0;
    return 1;
}
static void check_free(const struct parcel *p, int *corrupt) {
    if (!holds(p)) *corrupt = 1;
    free(p->item);
}
static void *stress_worker(void *arg) {
    int id = (int)(intptr_t)arg, corrupt = 0, refused = 0;
    long moved = 0;
    uint64_t seed = 0x9e3779b97f4a7c15ull * (uint64_t)(id + 1);
    struct parcel own[OWN] = {0};
    for (int round = 1; round <= STRESS_ROUNDS; round++) {
        unsigned k = next(&seed) % OWN;
        uint64_t r = next(&seed);
        if (own[k].item) { check_free(&own[k], &corrupt); own[k].item = 0; }
        struct parcel p = {.size = r % 61 == 0 ? r % 300000 + 1 : r % 3000 + 1,
                           .value = (unsigned char)((id * 31 + round) % 251)};
        p.item = malloc(p.size);
        if (!p.item) refused = 1;
        else {
            memset(p.item, p.value, p.size);
            if (round % 2 == 0) own[k] = p;
            else {
                int put = 0;
                pthread_mutex_lock(&queue_lock);
                if (queue_count < QUEUE) { queue[(queue_head + queue_count++) % QUEUE] = p; put = 1; }
                pthread_mutex_unlock(&queue_lock);
                if (!put) check_free(&p, &corrupt);
            }
        }
        struct parcel q; int found = 0;
        pthread_mutex_lock(&queue_lock);
        if (queue_count > 0) { q = queue[queue_head]; queue_head = (queue_head + 1) % QUEUE; queue_count--; found = 1; }
        pthread_mutex_unlock(&queue_lock);
        if (found) { check_free(&q, &corrupt); moved++; }
    }
    for (int k = 0; k < OWN; k++) if (own[k].item) check_free(&own[k], &corrupt);
    pthread_mutex_lock(&queue_lock);
    stress_corrupt |= corrupt; stress_refused |= refused; stress_moved += moved;
    pthread_mutex_unlock(&queue_lock);
    return 0;
}
static int stress(void) {
    char line[200];
    uint64_t start = now_ns();
    for (int g = 0; g < STRESS_GENERATIONS; g++) {
        pthread_t t[STRESS_THREADS];
        for (int i = 0; i < STRESS_THREADS; i++)
            if (pthread_create(&t[i], 0, stress_worker, (void *)(intptr_t)(g * STRESS_THREADS + i))) {
                say("cualloc-bench: FAIL stress pthread_create\n");
                return 0;
            }
        for (int i = 0; i < STRESS_THREADS; i++) pthread_join(t[i], 0);
    }
    int corrupt = 0;
    while (queue_count > 0) { check_free(&queue[queue_head], &corrupt); queue_head = (queue_head + 1) % QUEUE; queue_count--; }
    stress_corrupt |= corrupt;
    int ok = !stress_corrupt && !stress_refused && stress_moved > 100000;
    snprintf(line, sizeof line, "cualloc-bench: stress %s threads=%d generations=%d moved=%ld ms=%llu\n",
             ok ? "ok" : "FAIL", STRESS_THREADS, STRESS_GENERATIONS, stress_moved,
             (unsigned long long)((now_ns() - start) / 1000000));
    say(line);
    return ok;
}

struct worker {
    pthread_t thread;
    int id, threads, remote;
    uint64_t *samples;
    size_t count;
    unsigned char **batch;          /* handed to the next worker (remote) */
    volatile int ready;              /* batch full */
};
static struct worker workers[MAX_THREADS];
static pthread_barrier_t gate, done;

/* Remote handoff: a one-slot mailbox per worker, spun on (no allocation). */
static void hand_over(struct worker *to, unsigned char **batch) {
    while (__atomic_load_n(&to->ready, __ATOMIC_ACQUIRE)) {}
    to->batch = batch;
    __atomic_store_n(&to->ready, 1, __ATOMIC_RELEASE);
}
static unsigned char **take(struct worker *self) {
    while (!__atomic_load_n(&self->ready, __ATOMIC_ACQUIRE)) {}
    unsigned char **batch = self->batch;
    __atomic_store_n(&self->ready, 0, __ATOMIC_RELEASE);
    return batch;
}

static void *work(void *arg) {
    struct worker *self = arg;
    uint64_t seed = 0x9e3779b97f4a7c15ull + (uint64_t)self->id;
    unsigned char **mine = malloc(LIVE * sizeof *mine);
    if (!self->remote) for (int i = 0; i < LIVE; i++) mine[i] = malloc(1 + next(&seed) % 4096);
    pthread_barrier_wait(&gate);
    int rounds = THREAD_PAIRS / LIVE;
    for (int r = 0; r < rounds; r++) {
        for (int i = 0; i < LIVE; i++) {
            size_t size = 1 + next(&seed) % 4096;
            if ((r * LIVE + i) % THREAD_SAMPLE == 0) {
                uint64_t t = now_ns();
                void *q = malloc(size);
                free(q);
                self->samples[self->count++] = now_ns() - t;
            }
            if (!self->remote) free(mine[i]);
            mine[i] = malloc(size);
            ((volatile unsigned char *)mine[i])[size - 1] = 1;
        }
        if (self->remote) {
            /* Pass this round's blocks on; free the ones handed to us. */
            /* The pointer arrays circulate with the blocks: one out, one in. */
            hand_over(&workers[(self->id + 1) % self->threads], mine);
            unsigned char **theirs = take(self);
            for (int i = 0; i < LIVE; i++) free(theirs[i]);
            mine = theirs;
        }
    }
    pthread_barrier_wait(&done);
    if (!self->remote) for (int i = 0; i < LIVE; i++) free(mine[i]);
    free(mine);
    return 0;
}

static void threads(int count, int remote) {
    static uint64_t samples[MAX_THREADS][THREAD_PAIRS / THREAD_SAMPLE + 2];
    static uint64_t pooled[MAX_THREADS * (THREAD_PAIRS / THREAD_SAMPLE + 2)];
    pthread_barrier_init(&gate, 0, count + 1);
    pthread_barrier_init(&done, 0, count + 1);
    for (int i = 0; i < count; i++) {
        workers[i] = (struct worker){.id = i, .threads = count, .remote = remote, .samples = samples[i]};
        if (pthread_create(&workers[i].thread, 0, work, &workers[i])) {
            say("cualloc-bench: FAIL pthread_create\n");
            return;
        }
    }
    pthread_barrier_wait(&gate);
    uint64_t start = now_ns();
    pthread_barrier_wait(&done);
    uint64_t elapsed = now_ns() - start;
    size_t n = 0;
    for (int i = 0; i < count; i++) {
        pthread_join(workers[i].thread, 0);
        memcpy(pooled + n, workers[i].samples, workers[i].count * sizeof *pooled);
        n += workers[i].count;
    }
    pthread_barrier_destroy(&gate);
    pthread_barrier_destroy(&done);
    char what[64];
    snprintf(what, sizeof what, "%s %d", remote ? "remote" : "threads", count);
    report_samples(what, pooled, n, (double)elapsed / ((double)count * (THREAD_PAIRS / LIVE) * LIVE));
}

int main(void) {
    uint64_t t[4096];
    say("cualloc-bench: start\n");
    for (int i = 0; i < 4096; i++) { uint64_t a = now_ns(); t[i] = now_ns() - a; }
    qsort(t, 4096, sizeof *t, compare);
    char line[96];
    snprintf(line, sizeof line, "cualloc-bench: timer p50=%llu ns\n", (unsigned long long)t[2048]);
    say(line);
    reserve_check();
    stress();
    const char *workloads[] = {"fixed64", "mixed", "bimodal", "medium"};
    for (int w = 0; w < 4; w++) single(workloads[w]);
    for (int n = 1; n <= MAX_THREADS; n *= 2) threads(n, 0);
    for (int n = 2; n <= MAX_THREADS; n *= 2) threads(n, 1);
    say("cualloc-bench: done\n");
    return 0;
}
