#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/wait.h>
static unsigned logs;
static char captured[200];
static void report(const char *format, ...)
{
   va_list args;
   va_start(args,format);
   vsnprintf(captured,sizeof(captured),format,args);
   va_end(args);
   logs++;
}
#include "native-transport-failure.h"
static void *race(void *arg)
{
   (void)arg;
   for (unsigned i=0;i<1000;i++)
      cubit_test_mesa_transport_failure("later",99,100);
   return NULL;
}
static unsigned start;
static void *first_race(void *arg)
{
   const uint32_t id=(uint32_t)(uintptr_t)arg;
   while (!__atomic_load_n(&start,__ATOMIC_ACQUIRE)) { }
   cubit_test_mesa_transport_failure("concurrent-first",id,1000+id);
   return NULL;
}
static void concurrent_first(void)
{
   pthread_t threads[8];
   for (uintptr_t i=0;i<8;i++)
      assert(!pthread_create(&threads[i],NULL,first_race,(void *)i));
   __atomic_store_n(&start,1,__ATOMIC_RELEASE);
   /* A drain during capture may see empty/writing; subsequent drain must
    * still deliver the complete winning tuple exactly once. */
   report_transport_failure();
   for (unsigned i=0;i<8;i++) assert(!pthread_join(threads[i],NULL));
   report_transport_failure();
   report_transport_failure();
   unsigned status=99,handle=0;
   assert(logs==1);
   assert(sscanf(captured,"MESA-TRANSPORT first-failure operation=concurrent-first status=%u handle=%u",
                 &status,&handle)==2);
   assert(status<8 && handle==1000+status);
}
int main(void)
{
   /* Fresh process state per race: never reset the production one-shot
    * recorder or carry a thread across fork. This fixture is hosted only. */
   for (unsigned run=0;run<32;run++) {
      pid_t child=fork();
      assert(child>=0);
      if (!child) { concurrent_first(); _exit(0); }
      int status;
      assert(waitpid(child,&status,0)==child);
      assert(WIFEXITED(status) && WEXITSTATUS(status)==0);
   }
   report_transport_failure();
   assert(logs==0);
   cubit_test_mesa_transport_failure("close-buffer",3,42);
   assert(logs==0); /* Capture must never perform logging under adapter lock. */
   pthread_t threads[8];
   for (unsigned i=0;i<8;i++) assert(!pthread_create(&threads[i],NULL,race,NULL));
   report_transport_failure();
   for (unsigned i=0;i<8;i++) assert(!pthread_join(threads[i],NULL));
   report_transport_failure();
   assert(logs==1);
   assert(strstr(captured,"operation=close-buffer status=3 handle=42"));
   puts("Transport capture PASS: first reason retained, 8000 later events ignored, no logging in callback, one drain");
   puts("Concurrent first capture PASS: 32 fresh-process races, eight producers, coherent winning tuple and one drain");
}
