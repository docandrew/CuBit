/* Record a Linux program's allocation trace for alloc-bench's replay
 * (LD_PRELOAD=record.so CUBIT_ALLOC_TRACE=<file> program ...). Wraps
 * glibc's own entry points (__libc_malloc and friends), names each live
 * block by a sequence id and writes {op, id, a, b} records (alloc-bench.c,
 * replay). Aligned requests are recorded as plain mallocs of their size.
 * Linux-hosted tooling only. */
#define _GNU_SOURCE
#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

extern void *__libc_malloc(size_t);
extern void __libc_free(void *);
extern void *__libc_realloc(void *, size_t);
extern void *__libc_calloc(size_t, size_t);
extern void *__libc_memalign(size_t, size_t);

enum { TABLE_BITS = 24, TABLE = 1 << TABLE_BITS, BUFFER = 1 << 16 };
struct op { uint32_t op, id; uint64_t a, b; };
struct slot { uintptr_t key; uint32_t id; };
static struct slot *table;
static struct op buffer[BUFFER];
static int used, fd = -2;
static uint32_t ids, *spare_ids, spare_count;
enum { SPARE_IDS = 1 << 24 };
static volatile int lock_word;
static __thread int inside;

static void lock(void) { while (__sync_lock_test_and_set(&lock_word, 1)) ; }
static void unlock(void) { __sync_lock_release(&lock_word); }
static void flush(void) {
    if (fd >= 0 && used) { ssize_t w = write(fd, buffer, used * sizeof *buffer); (void)w; }
    used = 0;
}
static __attribute__((destructor)) void finish(void) { lock(); flush(); unlock(); }
static int start(void) {
    if (fd == -2) {
        /* One file per process: <CUBIT_ALLOC_TRACE>.<pid> (a compiler
         * driver's children inherit the preload). */
        const char *path = getenv("CUBIT_ALLOC_TRACE");
        char name[4096];
        if (path && snprintf(name, sizeof name, "%s.%d", path, (int)getpid()) < (int)sizeof name)
            fd = open(name, O_WRONLY | O_CREAT | O_TRUNC | O_APPEND, 0644);
        else
            fd = -1;
        table = __libc_calloc(TABLE, sizeof *table);
        spare_ids = __libc_malloc(SPARE_IDS * sizeof *spare_ids);
        if (!table || !spare_ids) fd = -1;
    }
    return fd >= 0;
}
static size_t hash(uintptr_t k) { return (size_t)((k >> 4) * 0x9E3779B97F4A7C15ull >> (64 - TABLE_BITS)); }
static void put(uintptr_t k, uint32_t id) {
    size_t h = hash(k);
    while (table[h].key && table[h].key != k) h = (h + 1) & (TABLE - 1);
    table[h].key = k; table[h].id = id;
}
/* Backward-shift deletion keeps probe chains intact. */
static int take(uintptr_t k, uint32_t *id) {
    size_t h = hash(k);
    while (table[h].key != k) { if (!table[h].key) return 0; h = (h + 1) & (TABLE - 1); }
    *id = table[h].id;
    size_t gap = h, j = h;
    for (;;) {
        j = (j + 1) & (TABLE - 1);
        if (!table[j].key) break;
        size_t home = hash(table[j].key);
        if (((j - home) & (TABLE - 1)) >= ((j - gap) & (TABLE - 1))) { table[gap] = table[j]; gap = j; }
    }
    table[gap].key = 0;
    return 1;
}
/* Ids of freed blocks are reused, so the replay's table stays small. */
static uint32_t fresh_id(void) { return spare_count ? spare_ids[--spare_count] : ++ids; }
static void retire(uint32_t id) { if (spare_count < SPARE_IDS) spare_ids[spare_count++] = id; }
static void emit(uint32_t op, uint32_t id, uint64_t a, uint64_t b) {
    buffer[used++] = (struct op){op, id, a, b};
    if (used == BUFFER) flush();
}
static void *record(uint32_t op, void *old, void *fresh, uint64_t a, uint64_t b) {
    if (inside) return fresh;
    inside = 1;
    lock();
    if (start()) {
        uint32_t id = 0;
        if (op == 2) { if (old && take((uintptr_t)old, &id)) { emit(2, id, 0, 0); retire(id); } }
        else if (op == 3) {
            if (old && take((uintptr_t)old, &id)) {
                if (fresh) { emit(3, id, a, 0); put((uintptr_t)fresh, id); }
                else if (!a) { emit(2, id, 0, 0); retire(id); }
                else put((uintptr_t)old, id);
            } else if (fresh) { id = fresh_id(); emit(1, id, a, 0); put((uintptr_t)fresh, id); }
        } else if (fresh) { id = fresh_id(); emit(op, id, a, b); put((uintptr_t)fresh, id); }
    }
    unlock();
    inside = 0;
    return fresh;
}
void *malloc(size_t n) { return record(1, 0, __libc_malloc(n), n, 0); }
void free(void *p) { record(2, p, 0, 0, 0); __libc_free(p); }
void *calloc(size_t k, size_t n) { return record(4, 0, __libc_calloc(k, n), k, n); }
void *realloc(void *p, size_t n) {
    /* Remove the old name before the block can be reused by another thread. */
    void *q = __libc_realloc(p, n);
    return record(3, p, q, n, 0);
}
void *memalign(size_t a, size_t n) { return record(1, 0, __libc_memalign(a, n), n, 0); }
void *aligned_alloc(size_t a, size_t n) { return memalign(a, n); }
int posix_memalign(void **r, size_t a, size_t n) {
    void *p = memalign(a, n);
    if (!p) return ENOMEM;
    *r = p;
    return 0;
}
