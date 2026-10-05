/* Native CuBit fixture: launch with no filesystem endpoint capability.
 * Every denied open, including initial queue setup, must release its frames. */
#include <stdint.h>
#include <errno.h>
#include <cubit/debug.h>
extern long __cubit_file_open(const char *, int, unsigned long, uint64_t *, uint64_t *);
static unsigned long owned(void) { unsigned long r; __asm__ volatile("syscall":"=a"(r):"a"(15UL),"D"(1602UL):"rcx","r11","memory"); return r; }
int main(void) {
 unsigned long before=owned();
 for(int i=0;i<32;i++) {
  uint64_t handle=0, size=0;
  if(__cubit_file_open("/denied",0,0,&handle,&size)!=-EACCES || owned()!=before) {
   cubit_debug_write("TEST: FAIL denied grant leak\n",sizeof("TEST: FAIL denied grant leak\n")-1); return 1;
  }
 }
 cubit_debug_write("TEST: PASS denied grants reclaim\n",sizeof("TEST: PASS denied grants reclaim\n")-1);return 0;
}
