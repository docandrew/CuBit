/* The first C compiled on CuBit (tests/gcc): no headers, so cc1 needs only
   this file. Built and linked on CuBit, it exits with 42. */
static int square(int x) { return x * x; }

int answer(void) { return square(6) + 6; }

int main(void) { return answer(); }
