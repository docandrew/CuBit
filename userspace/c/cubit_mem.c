/*
 * CuBit OS - Memory Management (sbrk + malloc/free)
 * Copyright (C) 2026 Jon Andrew
 *
 * Boundary-tag allocator with size-segregated free lists.
 *
 * Every block starts with a 16-byte header {prev_size, size|flags}; payloads
 * are 16-byte aligned (the x86-64 ABI's malloc alignment). A free block keeps
 * its free-list links in its payload and records its size in the following
 * block's prev_size, so free() coalesces with both neighbours in O(1).
 * Free blocks live in 128 bins: exact 16-byte classes below 1 KiB, then one
 * bin per power of two. A bitmap of non-empty bins finds the next fitting
 * bin without walking empty lists, so malloc and free cost O(1) apart from a
 * first-fit scan inside a single large bin.
 *
 * The heap grows with sbrk in regions. Other code in the process (the Ada
 * runtime, Rust libraries, surface buffers) also moves the break, so regions
 * need not be contiguous: each ends in an in-use fence header that stops
 * coalescing, and a region is extended in place only when the break still
 * sits at its fence.
 */
#include "cubit.h"

/*---------------------------------------------------------------------------
 * sbrk - Extend the process heap
 *
 * Returns pointer to the old break (start of new memory), or (void*)-1
 * on failure.
 *---------------------------------------------------------------------------*/
void *cubit_sbrk(intptr_t increment)
{
    return (void *)syscall1(SYSCALL_SBRK, increment);
}

struct block {
    size_t prev_size;          /* size of the previous block, if it is free */
    size_t head;               /* size | INUSE | PREV_INUSE */
    struct block *next_free;   /* free blocks only: payload holds the links */
    struct block *prev_free;
};

#define ALIGNMENT   16
#define HEADER      16
#define MIN_BLOCK   32
#define INUSE       ((size_t)1)
#define PREV_INUSE  ((size_t)2)
#define FLAGS       ((size_t)15)
#define GROW_MIN    ((size_t)64 * 1024)
#define NBINS       128
#define SMALL_LIMIT 1024

#define ALIGN_UP(x) (((x) + (ALIGNMENT - 1)) & ~(size_t)(ALIGNMENT - 1))

static struct block *bins[NBINS];
static unsigned long bin_map[NBINS / 64];
static struct block *top_fence;    /* fence of the most recent region */

#ifdef CUBIT_MEM_CHECK
static struct block *check_regions[65536];
static int check_region_count;
#endif

static size_t block_size(const struct block *b) { return b->head & ~FLAGS; }

static struct block *next_block(struct block *b)
{
    return (struct block *)((char *)b + block_size(b));
}

static unsigned bin_index(size_t size)
{
    unsigned index;
    if (size < SMALL_LIMIT) return (unsigned)(size >> 4);
    /* 64 + floor(log2(size)) - 10: [1 KiB, 2 KiB) -> 64, ... */
    index = 64;
    size >>= 11;
    while (size != 0 && index < NBINS - 1) { index++; size >>= 1; }
    return index;
}

static void insert_free(struct block *b)
{
    unsigned index = bin_index(block_size(b));
    b->prev_free = NULL;
    b->next_free = bins[index];
    if (bins[index] != NULL) bins[index]->prev_free = b;
    bins[index] = b;
    bin_map[index / 64] |= 1UL << (index % 64);
}

static void unlink_free(struct block *b)
{
    unsigned index = bin_index(block_size(b));
    if (b->prev_free != NULL) b->prev_free->next_free = b->next_free;
    else bins[index] = b->next_free;
    if (b->next_free != NULL) b->next_free->prev_free = b->prev_free;
    if (bins[index] == NULL) bin_map[index / 64] &= ~(1UL << (index % 64));
}

/* Mark b free, merge it with free neighbours and file it in its bin. */
static void release(struct block *b)
{
    size_t size = block_size(b);
    struct block *next = next_block(b);

    if (!(next->head & INUSE)) {
        unlink_free(next);
        size += block_size(next);
    }
    if (!(b->head & PREV_INUSE)) {
        struct block *prev = (struct block *)((char *)b - b->prev_size);
        unlink_free(prev);
        size += block_size(prev);
        b = prev;
    }
    b->head = size | (b->head & PREV_INUSE);
    next = next_block(b);
    next->prev_size = size;
    next->head &= ~PREV_INUSE;
    insert_free(b);
}

/* Take need bytes from the front of free block b (already unlinked). */
static void *carve(struct block *b, size_t need)
{
    size_t size = block_size(b);
    if (size - need >= MIN_BLOCK) {
        struct block *rest = (struct block *)((char *)b + need);
        struct block *after = (struct block *)((char *)b + size);
        rest->head = (size - need) | PREV_INUSE;
        after->prev_size = size - need;
        b->head = need | (b->head & PREV_INUSE) | INUSE;
        insert_free(rest);
    } else {
        b->head |= INUSE;
        next_block(b)->head |= PREV_INUSE;
    }
    return (char *)b + HEADER;
}

static struct block *find_fit(size_t need)
{
    unsigned index = bin_index(need);
    struct block *b;
    unsigned word;

    /* Large bins hold a range of sizes: first fit within the starting bin. */
    for (b = bins[index]; b != NULL; b = b->next_free)
        if (block_size(b) >= need) return b;
    /* Any block in a higher bin fits. */
    for (index++; index < NBINS; ) {
        unsigned long bits = bin_map[index / 64] >> (index % 64);
        if (bits != 0) {
            index += (unsigned)__builtin_ctzl(bits);
            return bins[index];
        }
        word = index / 64 + 1;
        index = word * 64;
    }
    return NULL;
}

/* Add at least need bytes of free space to the heap. */
static int grow(size_t need)
{
    size_t amount = need > GROW_MIN ? ALIGN_UP(need) : GROW_MIN;
    char *brk = cubit_sbrk(0);
    struct block *b, *fence;

    if (brk == (void *)-1) return 0;
    if (top_fence != NULL && brk == (char *)top_fence + HEADER) {
        /* The break is still at our fence: extend the region in place. The
         * old fence becomes the header of the new free block. */
        if (cubit_sbrk((intptr_t)amount) == (void *)-1) return 0;
        b = top_fence;
        b->head = amount | (b->head & PREV_INUSE);
    } else {
        size_t pad = (ALIGNMENT - ((uintptr_t)brk & (ALIGNMENT - 1))) & (ALIGNMENT - 1);
        if (cubit_sbrk((intptr_t)(pad + amount + HEADER)) == (void *)-1) return 0;
        b = (struct block *)(brk + pad);
        b->head = amount | PREV_INUSE;    /* nothing to merge with before */
#ifdef CUBIT_MEM_CHECK
        check_regions[check_region_count++] = b;
#endif
    }
    fence = (struct block *)((char *)b + amount);
    fence->prev_size = amount;
    fence->head = INUSE;                  /* size 0; previous block is free */
    top_fence = fence;
    b->head |= INUSE;                     /* release() expects an in-use block */
    release(b);
    return 1;
}

static size_t request_size(size_t size)
{
    size_t need;
    if (size > ((size_t)-1) / 2) return 0;
    need = ALIGN_UP(size + HEADER);
    return need < MIN_BLOCK ? MIN_BLOCK : need;
}

/*---------------------------------------------------------------------------
 * malloc
 *---------------------------------------------------------------------------*/
void *malloc(size_t size)
{
    size_t need;
    struct block *b;

    if (size == 0) return NULL;
    need = request_size(size);
    if (need == 0) return NULL;
    b = find_fit(need);
    if (b == NULL) {
        if (!grow(need)) return NULL;
        b = find_fit(need);
        if (b == NULL) return NULL;
    }
    unlink_free(b);
    return carve(b, need);
}

/*---------------------------------------------------------------------------
 * free
 *---------------------------------------------------------------------------*/
void free(void *ptr)
{
    struct block *b;

    if (!ptr) return;
    b = (struct block *)((char *)ptr - HEADER);
    if (!(b->head & INUSE)) return;       /* double free: ignore */
    b->head &= ~INUSE;
    release(b);
}

/*---------------------------------------------------------------------------
 * calloc
 *---------------------------------------------------------------------------*/
void *calloc(size_t nmemb, size_t size)
{
    size_t total;
    void *ptr;

    if (size != 0 && nmemb > ((size_t)-1) / size) return NULL;
    total = nmemb * size;
    ptr = malloc(total);
    if (ptr) memset(ptr, 0, total);
    return ptr;
}

/*---------------------------------------------------------------------------
 * realloc
 *---------------------------------------------------------------------------*/
void *realloc(void *ptr, size_t size)
{
    struct block *b, *next;
    size_t need, have;
    void *new_ptr;

    if (!ptr) return malloc(size);
    if (size == 0) { free(ptr); return NULL; }

    need = request_size(size);
    if (need == 0) return NULL;
    b = (struct block *)((char *)ptr - HEADER);
    have = block_size(b);
    if (have >= need) return ptr;

    /* Grow in place into a free successor. */
    next = next_block(b);
    if (!(next->head & INUSE) && have + block_size(next) >= need) {
        unlink_free(next);
        b->head = (have + block_size(next)) | (b->head & PREV_INUSE);
        return carve(b, need);            /* b is marked in use again */
    }

    new_ptr = malloc(size);
    if (!new_ptr) return NULL;
    memcpy(new_ptr, ptr, have - HEADER);
    free(ptr);
    return new_ptr;
}

#ifdef CUBIT_MEM_CHECK
/* Hosted-test consistency check: walks every region and every bin. */
int cubit_mem_check(void)
{
    struct block *const *regions = check_regions;
    int count = check_region_count;
    int r, free_blocks = 0, binned = 0;
    unsigned i;
    for (r = 0; r < count; r++) {
        struct block *b = regions[r];
        int prev_free = 0;
        size_t prev_size = 0;
        if (!(b->head & PREV_INUSE)) return -1;
        while (block_size(b) != 0) {
            size_t size = block_size(b);
            if (size < MIN_BLOCK || (size & (ALIGNMENT - 1))) return -2;
            if (((b->head & PREV_INUSE) != 0) == prev_free) return -3;
            if (prev_free && b->prev_size != prev_size) return -4;
            if (!(b->head & INUSE)) {
                if (prev_free) return -5;     /* uncoalesced neighbours */
                free_blocks++;
            }
            prev_free = !(b->head & INUSE);
            prev_size = size;
            b = next_block(b);
        }
        if (!(b->head & INUSE)) return -6;    /* fence */
        if (((b->head & PREV_INUSE) != 0) == prev_free) return -7;
    }
    for (i = 0; i < NBINS; i++) {
        struct block *b;
        if ((bins[i] != NULL) != ((bin_map[i / 64] >> (i % 64)) & 1)) return -8;
        for (b = bins[i]; b != NULL; b = b->next_free) {
            if (b->head & INUSE) return -9;
            if (bin_index(block_size(b)) != i) return -10;
            binned++;
        }
    }
    return free_blocks == binned ? 0 : -11;
}
#endif
