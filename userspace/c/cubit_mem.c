/*
 * CuBit OS - Memory Management (sbrk + malloc/free)
 * Copyright (C) 2026 Jon Andrew
 *
 * Simple first-fit free-list allocator with coalescing.
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

/*---------------------------------------------------------------------------
 * Simple free-list allocator
 *
 * Each block has a header with size and free flag. Free blocks form an
 * implicit list (we walk all blocks sequentially). Adjacent free blocks
 * are coalesced on free().
 *---------------------------------------------------------------------------*/

/* Block header: sits just before the returned pointer */
struct block_header {
    size_t size;            /* size of usable data (not including header) */
    int    free;            /* 1 if free, 0 if allocated */
    struct block_header *next;  /* next block in list (sequential) */
};

#define HEADER_SIZE (sizeof(struct block_header))
#define ALIGN16(x)  (((x) + 15) & ~(size_t)15)

static struct block_header *heap_start = NULL;

/*---------------------------------------------------------------------------
 * find_free_block - First-fit search
 *---------------------------------------------------------------------------*/
static struct block_header *find_free_block(size_t size)
{
    struct block_header *curr = heap_start;
    while (curr) {
        if (curr->free && curr->size >= size) {
            return curr;
        }
        curr = curr->next;
    }
    return NULL;
}

/*---------------------------------------------------------------------------
 * request_space - Extend heap via sbrk
 *---------------------------------------------------------------------------*/
static struct block_header *request_space(size_t size)
{
    struct block_header *block;
    block = cubit_sbrk(HEADER_SIZE + size);
    if (block == (void *)-1) {
        return NULL;
    }
    block->size = size;
    block->free = 0;
    block->next = NULL;
    return block;
}

/*---------------------------------------------------------------------------
 * malloc
 *---------------------------------------------------------------------------*/
void *malloc(size_t size)
{
    struct block_header *block;

    if (size == 0) return NULL;

    size = ALIGN16(size);

    block = find_free_block(size);
    if (block) {
        /* Split block if significantly larger than needed */
        if (block->size >= size + HEADER_SIZE + 16) {
            struct block_header *new_block =
                (struct block_header *)((char *)block + HEADER_SIZE + size);
            new_block->size = block->size - size - HEADER_SIZE;
            new_block->free = 1;
            new_block->next = block->next;
            block->size = size;
            block->next = new_block;
        }
        block->free = 0;
        return (void *)((char *)block + HEADER_SIZE);
    }

    /* No free block found, request more space */
    block = request_space(size);
    if (!block) return NULL;

    /* Link into list */
    if (!heap_start) {
        heap_start = block;
    } else {
        struct block_header *curr = heap_start;
        while (curr->next) curr = curr->next;
        curr->next = block;
    }

    return (void *)((char *)block + HEADER_SIZE);
}

/*---------------------------------------------------------------------------
 * free
 *---------------------------------------------------------------------------*/
void free(void *ptr)
{
    struct block_header *block;

    if (!ptr) return;

    block = (struct block_header *)((char *)ptr - HEADER_SIZE);
    block->free = 1;

    /* Coalesce with next block if it's free */
    if (block->next && block->next->free) {
        block->size += HEADER_SIZE + block->next->size;
        block->next = block->next->next;
    }
}

/*---------------------------------------------------------------------------
 * calloc
 *---------------------------------------------------------------------------*/
void *calloc(size_t nmemb, size_t size)
{
    size_t total = nmemb * size;
    void *ptr = malloc(total);
    if (ptr) {
        char *p = (char *)ptr;
        for (size_t i = 0; i < total; i++) {
            p[i] = 0;
        }
    }
    return ptr;
}

/*---------------------------------------------------------------------------
 * realloc
 *---------------------------------------------------------------------------*/
void *realloc(void *ptr, size_t size)
{
    if (!ptr) return malloc(size);
    if (size == 0) { free(ptr); return NULL; }

    struct block_header *block =
        (struct block_header *)((char *)ptr - HEADER_SIZE);

    if (block->size >= size) return ptr;

    void *new_ptr = malloc(size);
    if (!new_ptr) return NULL;

    /* Copy old data */
    char *src = (char *)ptr;
    char *dst = (char *)new_ptr;
    for (size_t i = 0; i < block->size; i++) {
        dst[i] = src[i];
    }

    free(ptr);
    return new_ptr;
}
