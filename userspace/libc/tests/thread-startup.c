#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <cubit/debug.h>
static void *worker(void *arg) {
 if(syscall(SYS_gettid)==0) {
  cubit_debug_write("STARTUP: zero unpublished tid\n",29);
  for(;;) sched_yield();
 }
 return arg;
}
int main(void) {
 for(unsigned i=0;i<128;i++) {
  pthread_t t;void *result;
  if(pthread_create(&t,0,worker,(void*)1)||pthread_join(t,&result)||result!=(void*)1) {
   cubit_debug_write("STARTUP: FAIL\n",14);for(;;) sched_yield();
  }
 }
 cubit_debug_write("STARTUP: PASS\n",14);
 for(;;) sched_yield();
}
