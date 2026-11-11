/* Linux-hosted allocator comparison, one C harness for every allocator
 * (ALLOC-001 item 11). Compiled once per allocator with the same compiler
 * and flags, each allocator linked statically into a fully static binary
 * (build.sh); CuAlloc through its C entry points (cualloc_allocate with
 * malloc's 16-byte alignment, cualloc_free), the references through
 * malloc/free. Nothing here is CuBit integration.
 *
 *   single <trace> <pairs>     the traces of bench.rs (xorshift, 1024 live
 *                              slots, free + malloc + first/last byte touch
 *                              per pair, every 128th pair timed); medium-full
 *                              is medium writing every page of each block
 *   hot <bytes> <pairs>        one block allocated and freed, repeatedly
 *   split <rounds>             1024 x 64 B allocated, then freed: alloc and
 *                              free timed apart
 *   threads <local|remote> <threads> <rounds>
 *                              thread_bench.rs in C: batches of 512 blocks of
 *                              1..4096 B, freed by the allocating thread or
 *                              handed to the next thread to free
 *   replay <file>              a recorded allocation trace (record.c)
 *
 * Output: one JSON object per run. */
#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>

#if defined(ENGINE_CUALLOC)
uint64_t cualloc_allocate(uint64_t, uint64_t);
void cualloc_free(uint64_t);
uint64_t cualloc_usable_size(uint64_t);
uint64_t cualloc_reallocate(uint64_t, uint64_t);
uint64_t cualloc_allocate_zeroed(uint64_t, uint64_t);
enum { FUNDAMENTAL = 16 };
#define ENGINE "cualloc"
static inline void *A(size_t n) { return (void *)cualloc_allocate(n, FUNDAMENTAL); }
static inline void F(void *p) { cualloc_free((uint64_t)p); }
static inline void *R(void *p, size_t n) { return (void *)cualloc_reallocate((uint64_t)p, n); }
static inline void *C(size_t k, size_t n) { return (void *)cualloc_allocate_zeroed(k * n, FUNDAMENTAL); }
static inline size_t U(void *p) { return cualloc_usable_size((uint64_t)p); }
void cualloc_free_sized(uint64_t, uint64_t, uint64_t);
#if defined(SIZED_FREE)
static inline void FS(void *p, size_t n) { cualloc_free_sized((uint64_t)p, n, FUNDAMENTAL); }
#endif
#else
#include <malloc.h>
#if defined(ENGINE_JEMALLOC)
#define ENGINE "jemalloc"
#elif defined(ENGINE_TCMALLOC)
#define ENGINE "tcmalloc"
#elif defined(ENGINE_MIMALLOC)
#define ENGINE "mimalloc"
#else
#define ENGINE "glibc"
#endif
static inline void *A(size_t n) { return malloc(n); }
static inline void F(void *p) { free(p); }
static inline void *R(void *p, size_t n) { return realloc(p, n); }
static inline void *C(size_t k, size_t n) { return calloc(k, n); }
static inline size_t U(void *p) { return malloc_usable_size(p); }
#if defined(SIZED_FREE)
/* C23 free_sized (jemalloc, tcmalloc). */
void free_sized(void *, size_t);
static inline void FS(void *p, size_t n) { free_sized(p, n); }
#endif
#endif
/* SIZED_FREE: frees that know the block's size (Rust GlobalAlloc dealloc,
 * Ada storage pools, C23 free_sized) where the harness knows it (single,
 * replay, strings); otherwise the plain free. */
#if !defined(SIZED_FREE)
static inline void FS(void *p, size_t n) { (void)n; F(p); }
#define SIZED_NAME ""
#else
#define SIZED_NAME "-sized"
#endif

enum { LIVE = 1024, SAMPLE_EVERY = 128, BATCH = 512, THREAD_SAMPLE_EVERY = 64,
       MAX_THREADS = 64, SMALL_LIMIT = 4096, MEDIUM_LIMIT = 262144 };

static uint64_t next(uint64_t *s) { *s ^= *s << 13; *s ^= *s >> 7; *s ^= *s << 17; return *s; }
static uint64_t now(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (uint64_t)t.tv_sec * 1000000000ull + (uint64_t)t.tv_nsec;
}
static long status_kib(const char *field) {
    char line[256];
    long v = -1;
    FILE *f = fopen("/proc/self/status", "r");
    if (!f) return -1;
    while (fgets(line, sizeof line, f))
        if (!strncmp(line, field, strlen(field))) { v = atol(line + strlen(field)); break; }
    fclose(f);
    return v;
}
static int cmp32(const void *a, const void *b) {
    uint32_t x = *(const uint32_t *)a, y = *(const uint32_t *)b;
    return x < y ? -1 : x > y;
}
static void die(const char *m) { fprintf(stderr, "%s\n", m); exit(1); }

static size_t trace_size(const char *w, uint64_t *seed) {
    static const size_t boundary[] = {1, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256,
                                      257, 511, 512, 513, 1023, 1024, 1025, 2047, 2049, 4096};
    if (!strcmp(w, "fixed64")) return 64;
    if (!strcmp(w, "fixed256")) return 256;
    if (!strcmp(w, "mixed")) return 1 + next(seed) % SMALL_LIMIT;
    if (!strcmp(w, "small")) return 1 + next(seed) % 128;
    if (!strcmp(w, "boundary")) return boundary[next(seed) % (sizeof boundary / sizeof *boundary)];
    if (!strcmp(w, "medium") || !strcmp(w, "medium-full"))
        return SMALL_LIMIT + 1 + next(seed) % (MEDIUM_LIMIT - SMALL_LIMIT);
    if (!strcmp(w, "bimodal")) {
        uint64_t n = next(seed);
        return n % 8 == 0 ? 2048 + (n >> 3) % 2049 : 1 + (n >> 3) % 64;
    }
    die("unknown trace");
    return 0;
}

static int single(const char *w, long n) {
    uint32_t *slots = mmap(0, n * 4, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    uint32_t *sizes = mmap(0, n * 4, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    uint32_t *samples = mmap(0, (n / SAMPLE_EVERY + 1) * 4, PROT_READ | PROT_WRITE,
                             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (slots == MAP_FAILED || sizes == MAP_FAILED || samples == MAP_FAILED) die("mmap");
    uint64_t seed = 0x123456789abcdefull;
    for (long i = 0; i < n; i++) {
        slots[i] = next(&seed) % LIVE;
        sizes[i] = trace_size(w, &seed);
    }
    static volatile unsigned char *p[LIVE];
    static size_t held[LIVE];
    for (int i = 0; i < LIVE; i++) {
        p[i] = A(64);
        if (!p[i]) die("OOM");
        p[i][0] = 1; p[i][63] = 2; held[i] = 64;
    }
    /* "-full" traces write every page of each block, as a program that
     * uses its memory does: peak RSS is then the allocator's footprint.
     * The others write the first and last byte only, so a page in the
     * middle of a block counts only if an earlier block touched it. */
    size_t len = strlen(w);
    int full = len > 5 && !strcmp(w + len - 5, "-full");
    long taken = 0;
    uint64_t start = now();
    for (long i = 0; i < n; i++) {
        uint64_t t = i % SAMPLE_EVERY == 0 ? now() : 0;
        FS((void *)p[slots[i]], held[slots[i]]);
        volatile unsigned char *q = A(sizes[i]);
        if (!q) die("OOM");
        if (full)
            for (size_t o = 4096; o < sizes[i]; o += 4096) q[o] = 1;
        q[0] = 1; q[sizes[i] - 1] = 2;
        p[slots[i]] = q;
        held[slots[i]] = sizes[i];
        if (t) samples[taken++] = (uint32_t)(now() - t);
    }
    uint64_t elapsed = now() - start;
    uint64_t requested = 0, usable = 0;
    for (int i = 0; i < LIVE; i++) { requested += held[i]; usable += U((void *)p[i]); }
    long peak = status_kib("VmHWM:");
    for (int i = 0; i < LIVE; i++) F((void *)p[i]);
    long after = status_kib("VmRSS:");
    qsort(samples, taken, 4, cmp32);
    printf("{\"engine\":\"%s\",\"mode\":\"single\",\"workload\":\"%s\",\"pairs\":%ld,\"ns_per_pair\":%.3f,"
           "\"p50_ns\":%u,\"p99_ns\":%u,\"p999_ns\":%u,\"peak_rss_kib\":%ld,\"rss_after_free_kib\":%ld,"
           "\"live_requested\":%lu,\"live_usable\":%lu}\n",
           ENGINE, w, n, (double)elapsed / n, samples[taken / 2], samples[taken * 99 / 100],
           samples[taken * 999 / 1000], peak, after, (unsigned long)requested, (unsigned long)usable);
    return 0;
}

static int split(long rounds) {
    enum { COUNT = 1024, SIZE = 64 };
    static void *p[COUNT];
    uint64_t a = 0, f = 0;
    for (long r = 0; r < rounds; r++) {
        uint64_t t0 = now();
        for (int i = 0; i < COUNT; i++) { p[i] = A(SIZE); *(volatile char *)p[i] = 1; }
        uint64_t t1 = now();
        for (int i = 0; i < COUNT; i++) F(p[i]);
        uint64_t t2 = now();
        a += t1 - t0; f += t2 - t1;
    }
    printf("{\"engine\":\"%s\",\"mode\":\"split\",\"alloc_ns\":%.3f,\"free_ns\":%.3f}\n", ENGINE,
           (double)a / (rounds * COUNT), (double)f / (rounds * COUNT));
    return 0;
}

/* Hot loop: one block allocated and freed again, everything in L1: the
 * bare path length of a malloc/free pair. */
static int hot(size_t bytes, long n) {
    /* A second live block, so the loop never empties its slab or page. */
    volatile char *anchor = A(bytes);
    anchor[0] = 1;
    uint64_t t = now();
    for (long i = 0; i < n; i++) {
        volatile char *p = A(bytes);
        p[0] = 1;
        F((void *)p);
    }
    printf("{\"engine\":\"%s\",\"mode\":\"hot\",\"bytes\":%zu,\"ns_per_pair\":%.3f}\n", ENGINE, bytes,
           (double)(now() - t) / n);
    return 0;
}

/* Threads: a one-slot mailbox per thread (the previous thread's batch). */
struct mailbox { _Atomic(void **) batch; char pad[56]; };
static struct mailbox boxes[MAX_THREADS];
static int thread_count, thread_rounds, remote;
static pthread_barrier_t ready, go, done;
static uint32_t *thread_samples[MAX_THREADS];
static long thread_taken[MAX_THREADS];

/* Waiting for a mailbox: spin briefly, then give the CPU away, so a
 * preempted neighbour in the ring is not starved by our spinning. */
enum { SPIN_LIMIT = 256 };
static void relax(unsigned *spins) {
    if (++*spins < SPIN_LIMIT) __builtin_ia32_pause();
    else sched_yield();
}

static void *worker(void *arg) {
    int id = (int)(intptr_t)arg;
    uint64_t seed = 0x123456789abcdefull + id;
    long total = (long)thread_rounds * BATCH;
    uint32_t *sizes = A(total * 4);
    uint32_t *samples = A((total / THREAD_SAMPLE_EVERY + 1) * 4);
    void **mine = A(BATCH * sizeof(void *)), **spare = A(BATCH * sizeof(void *));
    if (!sizes || !samples || !mine || !spare) die("OOM");
    for (long i = 0; i < total; i++) sizes[i] = 1 + next(&seed) % SMALL_LIMIT;
    for (int i = 0; i < BATCH; i++) { mine[i] = A(sizes[i]); if (!mine[i]) die("OOM"); }
    for (int i = 0; i < BATCH; i++) F(mine[i]);
    long taken = 0, counter = 0;
    pthread_barrier_wait(&ready);
    pthread_barrier_wait(&go);
    for (int r = 0; r < thread_rounds; r++) {
        uint32_t *s = sizes + (long)r * BATCH;
        for (int i = 0; i < BATCH; i++) {
            if (++counter % THREAD_SAMPLE_EVERY == 0) {
                uint64_t t = now();
                F(A(s[i]));
                samples[taken++] = (uint32_t)(now() - t);
            }
            volatile unsigned char *p = A(s[i]);
            if (!p) die("OOM");
            p[0] = 0x5a; p[s[i] - 1] = 0x5a;
            mine[i] = (void *)p;
        }
        void **to_free = mine;
        if (remote) {
            struct mailbox *out = &boxes[(id + 1) % thread_count], *in = &boxes[id];
            void **expected = NULL;
            unsigned spins = 0;
            while (!atomic_compare_exchange_weak(&out->batch, &expected, mine)) {
                expected = NULL;
                relax(&spins);
            }
            void **got;
            spins = 0;
            while (!(got = atomic_exchange(&in->batch, NULL))) relax(&spins);
            to_free = got;
            mine = spare;
            spare = got;
        }
        for (int i = 0; i < BATCH; i++) {
            volatile unsigned char *p = to_free[i];
            if (p[0] != 0x5a) die("corrupt");
            F((void *)p);
        }
    }
    pthread_barrier_wait(&done);
    thread_samples[id] = samples;
    thread_taken[id] = taken;
    return 0;
}

static int threads(const char *w, int n, int rounds) {
    if (n < 1 || n > MAX_THREADS || rounds < 1) die("threads");
    remote = !strcmp(w, "remote");
    if (remote && n < 2) die("remote needs two threads");
    thread_count = n; thread_rounds = rounds;
    pthread_barrier_init(&ready, 0, n + 1);
    pthread_barrier_init(&go, 0, n + 1);
    pthread_barrier_init(&done, 0, n + 1);
    pthread_t t[MAX_THREADS];
    for (int i = 0; i < n; i++) pthread_create(&t[i], 0, worker, (void *)(intptr_t)i);
    pthread_barrier_wait(&ready);
    uint64_t begin = now();
    pthread_barrier_wait(&go);
    pthread_barrier_wait(&done);
    uint64_t elapsed = now() - begin;
    for (int i = 0; i < n; i++) pthread_join(t[i], 0);
    long all = 0;
    for (int i = 0; i < n; i++) all += thread_taken[i];
    uint32_t *s = mmap(0, all * 4 + 4, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    long k = 0;
    for (int i = 0; i < n; i++)
        for (long j = 0; j < thread_taken[i]; j++) s[k++] = thread_samples[i][j];
    qsort(s, all, 4, cmp32);
    long pairs = (long)n * rounds * BATCH;
    printf("{\"engine\":\"%s\",\"mode\":\"threads\",\"workload\":\"%s\",\"threads\":%d,\"ns_per_pair\":%.3f,"
           "\"p50_ns\":%u,\"p99_ns\":%u,\"p999_ns\":%u,\"peak_rss_kib\":%ld}\n",
           ENGINE, w, n, (double)elapsed / pairs, s[all / 2], s[all * 99 / 100], s[all * 999 / 1000],
           status_kib("VmHWM:"));
    return 0;
}

/* Replay: records of 24 bytes {op, id, a, b} (record.c): op 1 malloc(a),
 * 2 free, 3 realloc(a), 4 calloc(a, b), 5 aligned (b alignment, a bytes):
 * id names the block. Every 64th operation is timed alone. */
struct op { uint32_t op, id; uint64_t a, b; };
static int replay(const char *file) {
    int fd = open(file, O_RDONLY);
    struct stat st;
    if (fd < 0 || fstat(fd, &st)) die("trace");
    long n = st.st_size / sizeof(struct op);
    const struct op *ops = mmap(0, st.st_size, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0);
    if (ops == MAP_FAILED) die("trace map");
    uint32_t ids = 0;
    for (long i = 0; i < n; i++) if (ops[i].id >= ids) ids = ops[i].id + 1;
    void **live = mmap(0, (size_t)ids * 8 + 8, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    uint64_t *bytes = mmap(0, (size_t)ids * 8 + 8, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    uint32_t *samples = mmap(0, (n / THREAD_SAMPLE_EVERY + 1) * 4, PROT_READ | PROT_WRITE,
                             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    long taken = 0;
    uint64_t requested = 0, peak_requested = 0;
    uint64_t start = now();
    for (long i = 0; i < n; i++) {
        const struct op *o = &ops[i];
        uint64_t t = i % THREAD_SAMPLE_EVERY == 0 ? now() : 0;
        unsigned char *p;
        switch (o->op) {
        case 1: p = A(o->a); if (!p) die("OOM"); p[0] = 1; live[o->id] = p; bytes[o->id] = o->a; break;
        case 2: FS(live[o->id], bytes[o->id]); live[o->id] = 0; break;
        case 3: p = R(live[o->id], o->a); if (!p) die("OOM"); p[0] = 1; live[o->id] = p; bytes[o->id] = o->a; break;
        case 4: p = C(o->a, o->b); if (!p) die("OOM"); live[o->id] = p; bytes[o->id] = o->a * o->b; break;
        default: die("bad op");
        }
        if (t) samples[taken++] = (uint32_t)(now() - t);
    }
    uint64_t elapsed = now() - start;
    (void)requested; (void)peak_requested;
    long peak = status_kib("VmHWM:");
    qsort(samples, taken, 4, cmp32);
    printf("{\"engine\":\"%s\",\"mode\":\"replay\",\"workload\":\"%s\",\"ops\":%ld,\"ns_per_op\":%.3f,"
           "\"p50_ns\":%u,\"p99_ns\":%u,\"p999_ns\":%u,\"peak_rss_kib\":%ld}\n",
           ENGINE, strrchr(file, '/') ? strrchr(file, '/') + 1 : file, n, (double)elapsed / n,
           samples[taken / 2], samples[taken * 99 / 100], samples[taken * 999 / 1000], peak);
    return 0;
}

/* String building: LIVE strings, each grown by appending 1..64 bytes
 * (realloc to the new length) until it passes a random length up to 64
 * KiB, then freed and started again; one op is one append. */
static int strings(long n) {
    enum { STRINGS = 64, LONGEST = 65536 };
    static unsigned char *s[STRINGS];
    static size_t len[STRINGS], goal[STRINGS];
    uint64_t seed = 0x2545f4914f6cdd1dull;
    for (int i = 0; i < STRINGS; i++) { s[i] = 0; len[i] = 0; goal[i] = 1 + next(&seed) % LONGEST; }
    uint64_t start = now();
    for (long i = 0; i < n; i++) {
        uint64_t r = next(&seed);
        int k = r % STRINGS;
        size_t add = 1 + (r >> 8) % 64;
        unsigned char *q = R(s[k], len[k] + add);
        if (!q) die("OOM");
        q[len[k]] = (unsigned char)i; q[len[k] + add - 1] = 1;
        s[k] = q; len[k] += add;
        if (len[k] >= goal[k]) {
            FS(s[k], len[k]); s[k] = 0; len[k] = 0; goal[k] = 1 + (r >> 20) % LONGEST;
        }
    }
    uint64_t elapsed = now() - start;
    printf("{\"engine\":\"%s%s\",\"mode\":\"strings\",\"workload\":\"strings\",\"pairs\":%ld,\"ns_per_pair\":%.3f,"
           "\"peak_rss_kib\":%ld}\n", ENGINE, SIZED_NAME, n, (double)elapsed / n, status_kib("VmHWM:"));
    for (int i = 0; i < STRINGS; i++) if (s[i]) FS(s[i], len[i]);
    return 0;
}

int main(int argc, char **argv) {
    if (argc >= 3 && !strcmp(argv[1], "strings")) return strings(atol(argv[2]));
    if (argc >= 4 && !strcmp(argv[1], "single")) return single(argv[2], atol(argv[3]));
    if (argc >= 3 && !strcmp(argv[1], "split")) return split(atol(argv[2]));
    if (argc >= 4 && !strcmp(argv[1], "hot")) return hot((size_t)atol(argv[2]), atol(argv[3]));
    if (argc >= 5 && !strcmp(argv[1], "threads")) return threads(argv[2], atoi(argv[3]), atoi(argv[4]));
    if (argc >= 3 && !strcmp(argv[1], "replay")) return replay(argv[2]);
    fprintf(stderr, "usage: %s single <trace> <pairs> | hot <bytes> <pairs> | split <rounds> | threads <local|remote> <n> <rounds>"
                    " | replay <file>\n", argv[0]);
    return 2;
}
