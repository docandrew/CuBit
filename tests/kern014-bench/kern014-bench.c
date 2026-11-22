/* KERN-014 A/B benchmark: frame commit/decommit through the owned-
 * memory API (alloc + zero + map, unmap + TLB round + free), the TLB round's
 * fixed cost (1-page decommit), user-level 4 KiB zero/copy as the reference
 * for in-kernel zeroing, and posix_spawn + waitpid of a trivial child.
 * KERN-014's A/B benchmark (docs/kernel-memory-map.md, "Baseline"):
 * tests/headless/run.sh --test kern014-bench --accel kvm [--cpus N]. */
#define _GNU_SOURCE
#include <spawn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <cubit/debug.h>

enum { RESERVE = 146, CHANGE = 147, RELEASE = 148, COMMIT = 0, DECOMMIT = 1 };
#define FAILED UINT64_MAX

static void say(const char *t) { cubit_debug_write(t, strlen(t)); }
static uint64_t call3(uint64_t n, uint64_t a, uint64_t b, uint64_t c) {
    uint64_t r;
    __asm__ volatile("syscall" : "=a"(r) : "a"(n), "D"(a), "S"(b), "d"(c) : "rcx", "r11", "memory");
    return r;
}
static uint64_t now_ns(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (uint64_t)t.tv_sec * 1000000000u + (uint64_t)t.tv_nsec;
}
static int cmp(const void *a, const void *b) {
    uint64_t x = *(const uint64_t *)a, y = *(const uint64_t *)b;
    return x < y ? -1 : x > y;
}
static void report(const char *what, uint64_t *s, int n, double per) {
    char line[256];
    qsort(s, n, sizeof *s, cmp);
    snprintf(line, sizeof line, "kern014: %s n=%d p50_ns=%llu min_ns=%llu p90_ns=%llu per_unit_p50_ns=%.1f\n",
             what, n, (unsigned long long)s[n / 2], (unsigned long long)s[0],
             (unsigned long long)s[n * 9 / 10], (double)s[n / 2] / per);
    say(line);
}

#define ROUNDS 60
static uint64_t a[ROUNDS], b[ROUNDS], c[ROUNDS];

static int owned(uint64_t pages, const char *tag) {
    uint64_t bytes = pages * 4096, base = call3(RESERVE, 16u << 20, 0, 0);
    char what[96];
    if (base == 0 || base == FAILED) { say("kern014: FAIL reserve\n"); return 0; }
    for (int r = 0; r < ROUNDS; r++) {
        uint64_t t0 = now_ns();
        if (call3(CHANGE, base, bytes, COMMIT) != 0) { say("kern014: FAIL commit\n"); return 0; }
        uint64_t t1 = now_ns();
        for (uint64_t p = 0; p < pages; p++) ((volatile unsigned char *)base)[p * 4096] = 1;
        uint64_t t2 = now_ns();
        if (call3(CHANGE, base, bytes, DECOMMIT) != 0) { say("kern014: FAIL decommit\n"); return 0; }
        uint64_t t3 = now_ns();
        a[r] = t1 - t0; b[r] = t2 - t1; c[r] = t3 - t2;
    }
    snprintf(what, sizeof what, "commit %s pages=%llu", tag, (unsigned long long)pages);
    report(what, a, ROUNDS, (double)pages);
    snprintf(what, sizeof what, "touch %s pages=%llu", tag, (unsigned long long)pages);
    report(what, b, ROUNDS, (double)pages);
    snprintf(what, sizeof what, "decommit %s pages=%llu", tag, (unsigned long long)pages);
    report(what, c, ROUNDS, (double)pages);
    call3(RELEASE, base, 16u << 20, 0);
    return 1;
}

static unsigned char buf[8u << 20] __attribute__((aligned(4096)));
static unsigned char src[4096] __attribute__((aligned(4096)));

static void zeroing(void) {
    /* Cold: stream over 8 MiB (larger than most L2s), 4 KiB at a time. */
    memset(buf, 1, sizeof buf);
    for (int r = 0; r < ROUNDS; r++) {
        uint64_t t0 = now_ns();
        for (size_t o = 0; o < sizeof buf; o += 4096) memset(buf + o, 0, 4096);
        a[r] = now_ns() - t0;
        __asm__ volatile("" ::: "memory");
    }
    report("user-zero-4k streaming(8MiB)", a, ROUNDS, sizeof buf / 4096.0);
    for (int r = 0; r < ROUNDS; r++) {
        uint64_t t0 = now_ns();
        for (int i = 0; i < 2048; i++) { memset(buf, 0, 4096); __asm__ volatile("" ::: "memory"); }
        a[r] = now_ns() - t0;
    }
    report("user-zero-4k hot", a, ROUNDS, 2048.0);
    for (int r = 0; r < ROUNDS; r++) {
        uint64_t t0 = now_ns();
        for (size_t o = 0; o < sizeof buf; o += 4096) memcpy(buf + o, src, 4096);
        a[r] = now_ns() - t0;
        __asm__ volatile("" ::: "memory");
    }
    report("user-copy-4k streaming(8MiB)", a, ROUNDS, sizeof buf / 4096.0);
    /* The kernel's Util.memset today: one volatile byte store at a time. */
    for (int r = 0; r < ROUNDS; r++) {
        uint64_t t0 = now_ns();
        for (int i = 0; i < 256; i++) {
            volatile unsigned char *p = buf + (size_t)(i % 2048) * 4096;
            for (int k = 0; k < 4096; k++) p[k] = 0;
        }
        a[r] = now_ns() - t0;
    }
    report("user-zero-4k bytewise-volatile(kernel memset shape)", a, ROUNDS, 256.0);
    for (int r = 0; r < ROUNDS; r++) {
        uint64_t t0 = now_ns();
        for (int i = 0; i < 256; i++) {
            unsigned char *p = buf + (size_t)(i % 2048) * 4096;
            unsigned long n = 4096;
            __asm__ volatile("rep stosb" : "+D"(p), "+c"(n) : "a"(0) : "memory");
        }
        a[r] = now_ns() - t0;
    }
    report("user-zero-4k rep-stosb", a, ROUNDS, 256.0);
}

static void spawning(void) {
    enum { N = 40 };
    static uint64_t s[N];
    char *argv[] = { "kern014-child.app", 0 };
    char *envp[] = { 0 };
    int ok = 0;
    for (int i = 0; i < N; i++) {
        pid_t pid = 0;
        int status = 0;
        uint64_t t0 = now_ns();
        int e = posix_spawn(&pid, "kern014-child.app", 0, 0, argv, envp);
        if (e) { char l[96]; snprintf(l, sizeof l, "kern014: FAIL spawn error=%d\n", e); say(l); return; }
        if (waitpid(pid, &status, 0) != pid || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            say("kern014: FAIL wait\n"); return;
        }
        s[ok++] = now_ns() - t0;
    }
    report("spawn+exit+wait", s, ok, 1.0);
}

int main(void) {
    say("kern014: start\n");
    uint64_t t0 = now_ns(), t1 = now_ns();
    { char l[96]; snprintf(l, sizeof l, "kern014: clock-step ns=%llu\n", (unsigned long long)(t1 - t0)); say(l); }
    zeroing();
    if (!owned(1, "small")) goto done;
    if (!owned(16, "16")) goto done;
    if (!owned(512, "2MiB")) goto done;
    if (!owned(4096, "16MiB")) goto done;
    spawning();
done:
    say("kern014: done\n");
    return 0;
}
