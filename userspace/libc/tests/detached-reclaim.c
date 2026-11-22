#define _GNU_SOURCE
#include <pthread.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <sched.h>
#include <unistd.h>
#include <cubit/debug.h>
static atomic_int done;
static _Thread_local unsigned tls;
static unsigned long owned(void) {
 unsigned long result;
 __asm__ volatile("syscall" : "=a"(result) : "a"(15UL), "D"(1602UL) : "rcx","r11","memory");
 return result;
}
static void *worker(void *p) {
 volatile unsigned char stack[65536];
 tls=123;
 for(unsigned i=0;i<sizeof stack;i+=4096) stack[i]=(unsigned char)i;
 if(tls!=123 || stack[0]!=0) __builtin_trap();
 atomic_store_explicit(&done,1,memory_order_release);
 return p;
}
static void *barrier(void *p) {return p;}
static int cycle(void) {
 pthread_attr_t a; pthread_t t,b;
 if(pthread_attr_init(&a) || pthread_attr_setstacksize(&a,2*1024*1024) || pthread_attr_setdetachstate(&a,PTHREAD_CREATE_DETACHED)) return 1;
 atomic_store(&done,0);
 if(pthread_create(&t,&a,worker,0)) return 2;
 pthread_attr_destroy(&a);
 while(!atomic_load_explicit(&done,memory_order_acquire)) sched_yield();
 /* pthread_create serializes on musl's thread-list lock, which the exiting
    detached thread holds until the kernel clears its exit word. */
 if(pthread_create(&b,0,barrier,0) || pthread_join(b,0)) return 3;
 return 0;
}
int main(void) {
 char out[200];
 for(int i=0;i<4;i++) if(cycle()) goto fail;
 unsigned long before=owned();
 for(int i=0;i<32;i++) if(cycle()) goto fail;
 unsigned long after=owned();
 int n=snprintf(out,sizeof out,"DETACHED: before=%lu after=%lu delta=%ld cycles=32\nDETACHED: %s\n",before,after,(long)(after-before),after<=before+1048576UL?"PASS":"LEAK");
 cubit_debug_write(out,n); return 0;
fail:
 cubit_debug_write("DETACHED: FAIL thread lifecycle\n",31);return 1;
}
