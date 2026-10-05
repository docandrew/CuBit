"""Run actual libc collection/wait code under deterministic pthread schedules.

Nix required. Optional source argument permits checking a proposed change and
running the same regression against its predecessor as a negative control.
"""
from pathlib import Path
import os
import subprocess
import sys
import tempfile

source = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parents[1] / 'overlay/src/cubit/net.c'
s = source.read_text()
a = s.index('static void release_collector(') if 'static void release_collector(' in s else s.index('static void collect(')
b = s.index('hidden struct cubit_tcp *__cubit_tcp_new', a)
body = s[a:b]
# Shared scheduling primitives are mocked; the ownership and wait implementation
# above is copied verbatim from production, rather than reimplemented here.
preamble = r'''
#define _GNU_SOURCE
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#define hidden
#define LOCK(x) pthread_mutex_lock(&(x))
#define UNLOCK(x) pthread_mutex_unlock(&(x))
#define OP_NET_WAIT 1
#define WAIT_TOKEN 2
struct cubit_tcp { int bit; };
static pthread_mutex_t net_lock = PTHREAD_MUTEX_INITIALIZER;
static int waiter, wait_outstanding, opens_outstanding = 1, queue_count = 1;
static unsigned followers;
static uint64_t interest, wait_mask;
static unsigned long wait_deadline;
static unsigned interest_count[64];
static void add_interest(uint64_t m) { for(int i=0;i<64;i++) if(m>>i&1) {if(!interest_count[i]++) interest|=1ULL<<i;} }
static void drop_interest(uint64_t m) { for(int i=0;i<64;i++) if(m>>i&1) {if(!--interest_count[i]) interest&=~(1ULL<<i);} }
static pthread_mutex_t gate = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t changed = PTHREAD_COND_INITIALIZER;
static int seq, scenario, drain_entered, allow_drain, waits_b, done_a, done_b, forced;
static int drains, max_drains;
static _Thread_local int role;
static struct timespec limit(void) { struct timespec t; clock_gettime(CLOCK_REALTIME,&t);t.tv_sec+=2;return t; }
static int until(int *value, int want) {
    struct timespec t=limit();
    while(*value<want) if(pthread_cond_timedwait(&changed,&gate,&t)==ETIMEDOUT)return 0;
    return 1;
}
static int __cubit_readiness_seq(void) { LOCK(gate);int s=seq;UNLOCK(gate);return s; }
static void __cubit_readiness_changed(void) { LOCK(gate);seq++;pthread_cond_broadcast(&changed);UNLOCK(gate); }
static void __cubit_readiness_wait(int before,unsigned long deadline) {
    (void)deadline;LOCK(gate);
    if(role==2){waits_b++;pthread_cond_broadcast(&changed);}
    while(seq==before&&!forced)pthread_cond_wait(&changed,&gate);
    UNLOCK(gate);
}
static int any_slot(void) { return 1; }
static void end_wait(void) {}
static int submit(unsigned slot,int op,int count,int zero,unsigned long deadline,uint64_t wanted,int other,int token) {
    (void)slot;(void)op;(void)count;(void)zero;(void)deadline;(void)wanted;(void)other;(void)token;
    if(scenario==4 && role==1){
        LOCK(gate);drain_entered=1;pthread_cond_broadcast(&changed);
        if(!until(&waits_b,1))forced=1;
        UNLOCK(gate);return 0;
    }
    if(scenario==5)__cubit_readiness_changed();
    return 1;
}
static void drain(int block) {
    (void)block;LOCK(gate);drains++;if(drains>max_drains)max_drains=drains;
    if(role==1){
        drain_entered=1;pthread_cond_broadcast(&changed);
        if(scenario==1){
            if(!until(&waits_b,1))forced=1;
            seq++;pthread_cond_broadcast(&changed);
            if(!until(&waits_b,2))forced=1;
        }else while(!allow_drain&&!forced)pthread_cond_wait(&changed,&gate);
    }
    drains--;UNLOCK(gate);
    if(block){LOCK(net_lock);wait_outstanding=0;UNLOCK(net_lock);}
}
'''
trailer = r'''
static void *a_thread(void *unused) {
    (void)unused;role=1;
    if(scenario==1 || scenario==4)__cubit_net_wait(0,~0UL,1);else collect();
    LOCK(gate);done_a=1;pthread_cond_broadcast(&changed);UNLOCK(gate);return 0;
}
static void *b_thread(void *unused) {
    (void)unused;role=2;
    __cubit_net_wait(0,~0UL,1);
    if(scenario==1)__cubit_net_wait(__cubit_readiness_seq(),~0UL,1);
    LOCK(gate);done_b=1;pthread_cond_broadcast(&changed);UNLOCK(gate);return 0;
}
int main(int argc,char **argv) {
    scenario=argc>1?atoi(argv[1]):1;
    if(scenario==3){for(int i=0;i<1000;i++)collect();if(seq)return 1;puts("PASS idle collection does not self-wake");return 0;}
    if(scenario==5){
        __cubit_net_wait(0,~0UL,1);
        if(wait_outstanding){puts("FAIL collector handed off a thread-owned WAIT");return 1;}
        puts("PASS local event cancels and harvests WAIT before handoff");return 0;
    }
    pthread_t a,b;pthread_create(&a,0,a_thread,0);
    LOCK(gate);int ok=until(&drain_entered,1);UNLOCK(gate);
    pthread_create(&b,0,b_thread,0);
    LOCK(gate);
    if(scenario==2){ok &= until(&waits_b,1);allow_drain=1;pthread_cond_broadcast(&changed);}
    ok &= until(&done_a,1);ok &= until(&done_b,1);ok &= !forced && max_drains==(scenario==4?0:1);
    forced=1;seq++;pthread_cond_broadcast(&changed);UNLOCK(gate);
    pthread_join(a,0);pthread_join(b,0);
    printf("%s scenario=%d follower_waits=%d max_collectors=%d\n",ok?"PASS":"FAIL",scenario,waits_b,max_drains);
    return ok?0:1;
}
'''
with tempfile.TemporaryDirectory(prefix='net-handoff-') as tmp:
    p=Path(tmp);(p/'test.c').write_text(preamble+body+trailer)
    subprocess.run(['cc','-std=c11','-O2','-Wall','-Wextra','-Wno-unused-variable','-pthread',str(p/'test.c'),'-o',str(p/'test')],check=True)
    outcomes=[subprocess.run([str(p/'test'),str(n)],timeout=10).returncode for n in (1,2,3,4,5)]
    sys.exit(1 if any(outcomes) else 0)
