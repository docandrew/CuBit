#include <stddef.h>
#include <stdint.h>
static uint64_t calls, bytes;
void *__real_memcpy(void *,const void *,size_t);
void *__wrap_memcpy(void *d,const void *s,size_t n){++calls;bytes+=n;return __real_memcpy(d,s,n);}
void reset_copies(void){calls=0;bytes=0;}
uint64_t copy_calls(void){return calls;}

uint64_t copy_bytes(void){return bytes;}
