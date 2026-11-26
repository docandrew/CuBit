/*
 * SPDX-License-Identifier: GPL-3.0-or-later
 * KERN-006 layer 1 (tests/image-shares/native): one copy of an image with
 * an 8 MiB read-only blob. It holds the blob mapped for a while (the
 * launcher measures free memory with every copy alive), then checks every
 * byte is its own image's fill and exits 0, or 1 if any is not.
 * IMAGE_SHARE_TWIN builds the twin: the same layout, another fill.
 */
#include <stddef.h>
#include <time.h>

#ifdef IMAGE_SHARE_TWIN
#define FILL 0xa5
#else
#define FILL 0x5a
#endif

enum { BLOB_BYTES = 8 << 20, HOLD_SECONDS = 5 };

/* In .rodata: a read-only PT_LOAD segment the kernel shares. */
__attribute__((used)) static const unsigned char blob[BLOB_BYTES] = {[0 ... BLOB_BYTES - 1] = FILL};

int main(void)
{
    struct timespec hold = {HOLD_SECONDS, 0};
    nanosleep(&hold, NULL);
    /* Read through a volatile view: the compiler knows the blob's bytes. */
    const volatile unsigned char *bytes = blob;
    for (size_t i = 0; i < BLOB_BYTES; i++)
        if (bytes[i] != FILL)
            return 1;
    return 0;
}
