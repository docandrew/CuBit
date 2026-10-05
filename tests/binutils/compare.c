/*
 * binutils-compare A B (tests/binutils): exits 0 when the two files are
 * byte-identical, 1 otherwise. binutils-check starts it after each tool.
 */
#include <stdio.h>

int main(int argc, char **argv)
{
	FILE *x, *y;
	int ok, cx, cy;
	if (argc != 3) return 2;
	x = fopen(argv[1], "rb");
	y = fopen(argv[2], "rb");
	ok = x && y;
	while (ok) {
		cx = fgetc(x); cy = fgetc(y);
		if (cx != cy) ok = 0;
		if (cx == EOF || cy == EOF) break;
	}
	if (x) fclose(x);
	if (y) fclose(y);
	return !ok;
}
