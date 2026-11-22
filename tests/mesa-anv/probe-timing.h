/* CPU wall time, not GPU timestamps. Unavailable/backward clocks invalidate
 * the whole sample; never substitute zero as a successful timing result. */
#ifndef CUBIT_PROBE_TIMING_H
#define CUBIT_PROBE_TIMING_H
#include <stdint.h>
#include <time.h>
enum probe_stage { PROBE_SETUP, PROBE_PIPELINE, PROBE_RECORD, PROBE_SUBMIT,
                   PROBE_WAIT, PROBE_READBACK, PROBE_CLEANUP, PROBE_STAGES };
struct probe_timing {
   uint64_t ns[PROBE_STAGES], last;
   enum probe_stage stage;
   int valid;
};
static int probe_now(uint64_t *out)
{
   struct timespec ts;
   if (clock_gettime(CLOCK_MONOTONIC, &ts) || ts.tv_sec < 0 ||
       ts.tv_nsec < 0 || ts.tv_nsec >= 1000000000L ||
       (uint64_t)ts.tv_sec > (UINT64_MAX-(uint64_t)ts.tv_nsec)/1000000000ULL)
      return 0;
   *out=(uint64_t)ts.tv_sec*1000000000ULL+(uint64_t)ts.tv_nsec;
   return 1;
}
static struct probe_timing probe_timing_start(void)
{
   struct probe_timing t={0};
   t.valid=probe_now(&t.last);
   return t;
}
static void probe_timing_mark(struct probe_timing *t, enum probe_stage next)
{
   uint64_t now;
   if (!t->valid) return;
   if (!probe_now(&now) || now<t->last ||
       now-t->last>UINT64_MAX-t->ns[t->stage]) {
      t->valid=0;
      return;
   }
   t->ns[t->stage]+=now-t->last;
   t->last=now;
   t->stage=next;
}
#endif
