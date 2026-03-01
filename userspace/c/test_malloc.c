/*
 * CuBit OS - Malloc Test Program
 * Copyright (C) 2026 Jon Andrew
 *
 * Tests sbrk/malloc/free by allocating memory, writing patterns, and
 * verifying them.
 */
#include "cubit.h"

/* Forward declarations for malloc/free */
void *malloc(size_t size);
void free(void *ptr);
void *cubit_sbrk(intptr_t increment);

/* Simple number-to-string for printing */
static void print_num(unsigned long n)
{
    char buf[20];
    int i = 0;

    if (n == 0) {
        cubit_puts("0");
        return;
    }

    while (n > 0) {
        buf[i++] = '0' + (n % 10);
        n /= 10;
    }

    /* Reverse and print */
    char out[20];
    for (int j = 0; j < i; j++) {
        out[j] = buf[i - 1 - j];
    }
    cubit_write(STDOUT, out, i);
}

int main(void)
{
    cubit_puts("test_malloc: Starting memory tests...\n");

    /* Test 1: sbrk directly */
    cubit_puts("test_malloc: Testing sbrk... ");
    void *brk = cubit_sbrk(4096);
    if (brk == (void *)-1) {
        cubit_puts("FAIL (sbrk returned -1)\n");
        return 1;
    }
    cubit_puts("OK (brk=0x");
    /* Simple hex print */
    {
        unsigned long v = (unsigned long)brk;
        char hex[17];
        for (int i = 15; i >= 0; i--) {
            int d = v & 0xF;
            hex[i] = d < 10 ? '0' + d : 'a' + d - 10;
            v >>= 4;
        }
        cubit_write(STDOUT, hex, 16);
    }
    cubit_puts(")\n");

    /* Test 2: malloc small */
    cubit_puts("test_malloc: Testing malloc(64)... ");
    char *p1 = (char *)malloc(64);
    if (!p1) {
        cubit_puts("FAIL\n");
        return 1;
    }
    for (int i = 0; i < 64; i++) p1[i] = (char)(i & 0xFF);
    for (int i = 0; i < 64; i++) {
        if (p1[i] != (char)(i & 0xFF)) {
            cubit_puts("FAIL (verify)\n");
            return 1;
        }
    }
    cubit_puts("OK\n");

    /* Test 3: malloc larger block */
    cubit_puts("test_malloc: Testing malloc(1MB)... ");
    char *p2 = (char *)malloc(1024 * 1024);
    if (!p2) {
        cubit_puts("FAIL\n");
        return 1;
    }
    /* Write pattern */
    for (size_t i = 0; i < 1024 * 1024; i++) {
        p2[i] = (char)(i & 0xFF);
    }
    /* Verify */
    for (size_t i = 0; i < 1024 * 1024; i++) {
        if (p2[i] != (char)(i & 0xFF)) {
            cubit_puts("FAIL at offset ");
            print_num(i);
            cubit_puts("\n");
            return 1;
        }
    }
    cubit_puts("OK\n");

    /* Test 4: free and reuse */
    cubit_puts("test_malloc: Testing free+realloc... ");
    free(p1);
    char *p3 = (char *)malloc(32);
    if (!p3) {
        cubit_puts("FAIL\n");
        return 1;
    }
    cubit_puts("OK\n");

    free(p2);
    free(p3);

    cubit_puts("test_malloc: All tests passed!\n");
    return 0;
}
