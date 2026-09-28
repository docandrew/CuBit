/* Host check of the CuBit runtime's memmove/memcpy/memset/memcmp
 * (userspace/runtime/gnat/cubit-string.adb): random sizes, offsets and
 * overlaps in both directions against byte-at-a-time references. The
 * runtime's object is linked first, so its symbols replace the C
 * library's. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned seed = 12345;
static unsigned rnd(void) { seed = seed * 1103515245u + 12345u; return seed >> 8; }

int main(void)
{
	enum { SIZE = 8192 };
	static unsigned char a[SIZE], ref[SIZE];
	int failures = 0;
	for (int round = 0; round < 200000; round++) {
		for (int i = 0; i < SIZE; i++) a[i] = ref[i] = (unsigned char)rnd();
		size_t len = rnd() % (round % 7 == 0 ? 4096 : 64);
		size_t s = rnd() % (SIZE - len), d = rnd() % (SIZE - len);
		if (round % 3 == 0) {   /* overlapping: d within 8 bytes of s */
			long near = (long)s + (long)(rnd() % 17) - 8;
			if (near >= 0 && (size_t)near <= SIZE - len - 1) d = (size_t)near;
		}
		/* reference: through a temporary */
		unsigned char tmp[4096];
		for (size_t i = 0; i < len; i++) tmp[i] = ref[s + i];
		for (size_t i = 0; i < len; i++) ref[d + i] = tmp[i];
		if (memmove(a + d, a + s, len) != a + d || memcmp(a, ref, SIZE) != 0) {
			if (failures++ < 5) printf("FAIL memmove len=%zu s=%zu d=%zu\n", len, s, d);
		}
	}
	/* The direction flag must be clear afterwards: a forward copy works. */
	unsigned char x[16] = "0123456789abcdef", y[16];
	memmove(x + 1, x, 8);
	memcpy(y, x, 16);
	if (memcmp(y, "0012345679abcdef", 16) != 0) { failures++; printf("FAIL after backward move\n"); }
	memset(y, 'z', 5);
	if (memcmp(y, "zzzzz45679abcdef", 16) != 0) { failures++; printf("FAIL memset\n"); }
	printf("runtime-string: %s\n", failures ? "FAIL" : "PASS");
	return failures != 0;
}
