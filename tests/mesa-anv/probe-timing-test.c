#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <stdio.h>
#include <time.h>
static struct timespec sample;
static int failure;
static int fake_clock(clockid_t id, struct timespec *out)
{ assert(id==CLOCK_MONOTONIC); *out=sample; return failure; }
#define clock_gettime fake_clock
#include "probe-timing.h"
int main(void)
{
   struct probe_timing t=probe_timing_start();
   assert(t.valid && t.last==0);
   for (unsigned i=1;i<=PROBE_STAGES;i++) {
      sample.tv_nsec=i*1234;
      probe_timing_mark(&t,i<PROBE_STAGES ? (enum probe_stage)i : PROBE_CLEANUP);
   }
   for (unsigned i=0;i<PROBE_STAGES;i++) assert(t.ns[i]==1234);
   sample.tv_nsec=0; probe_timing_mark(&t,PROBE_SETUP); assert(!t.valid);
   failure=1; t=probe_timing_start(); assert(!t.valid);
   failure=0; sample.tv_nsec=1000000000; t=probe_timing_start(); assert(!t.valid);
   sample.tv_nsec=-1; t=probe_timing_start(); assert(!t.valid);
   sample.tv_nsec=0; sample.tv_sec=-1; t=probe_timing_start(); assert(!t.valid);
   sample.tv_sec=0; t=probe_timing_start(); failure=1;
   probe_timing_mark(&t,PROBE_PIPELINE); assert(!t.valid);
   puts("CPU stage timing PASS: boundaries, zero epoch, backward/invalid/failed clock");
}
