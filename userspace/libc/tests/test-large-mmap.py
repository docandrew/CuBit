"""Run the real libc mmap adapter against a deterministic syscall boundary."""
from pathlib import Path
import subprocess, tempfile
source = (Path(__file__).resolve().parents[1] / 'overlay/src/cubit/syscall.c').read_text()
body = source[source.index('static long sys_mmap('):source.index('/* --- futexes')]
preamble = r'''
#define _GNU_SOURCE
#include <assert.h>
#include <errno.h>
#include <sys/mman.h>
#include <stdio.h>
#include <limits.h>
#define CUBIT_ALLOCATE_OWNED_MEMORY 115
#define CUBIT_RELEASE_OWNED_MEMORY 116
#define CUBIT_PROTECT_OWNED_MEMORY 117
static unsigned calls, releases, protects;
static unsigned long request;
static int fail_allocate, fail_protect;
static unsigned long cubit(unsigned long op, unsigned long a, unsigned long b,
 unsigned long c, unsigned long d, unsigned long e) {
 (void)b; (void)c; (void)d; (void)e; calls++;
 if (op == CUBIT_ALLOCATE_OWNED_MEMORY) { request=a; return fail_allocate ? 0 : 0x580000000000UL; }
 if (op == CUBIT_RELEASE_OWNED_MEMORY) { releases++; return 0; }
 assert(op == CUBIT_PROTECT_OWNED_MEMORY); protects++; return fail_protect;
}
static long __cubit_fd_pread(int fd, void *base, unsigned long n, long off) {
 (void)fd; (void)base; (void)n; (void)off; return -EIO;
}
'''
checks = r'''
int main(void) {
 const long flags=MAP_PRIVATE|MAP_ANONYMOUS;
 const unsigned long limit=256UL*1024*1024;
 assert(sys_mmap(0,0,3,flags,-1,0)==-EINVAL);
 assert(sys_mmap(0,limit+1,3,flags,-1,0)==-ENOMEM);
 assert(sys_mmap(0,ULONG_MAX,3,flags,-1,0)==-ENOMEM);
 assert(calls==0);
 unsigned long sizes[]={16UL*1024*1024+1,20UL*1024*1024,limit};
 for(unsigned i=0;i<3;i++) {
  assert(sys_mmap(0,sizes[i],3,flags,-1,0)==0x580000000000L);
  assert(request==sizes[i]);
 }
 fail_allocate=1;
 assert(sys_mmap(0,sizes[1],3,flags,-1,0)==-ENOMEM);
 assert(releases==0);
 fail_allocate=0;
 assert(sys_mmap(0,sizes[1],PROT_READ,flags,-1,0)>0);
 assert(protects==1);
 fail_protect=1;
 assert(sys_mmap(0,sizes[1],PROT_NONE,flags,-1,0)==-ENOMEM);
 assert(protects==2 && releases==1);
 unsigned before=calls;
 assert(sys_mmap(0,sizes[1],PROT_READ|PROT_EXEC,flags,-1,0)==-ENOTSUP);
 assert(sys_mmap(0,sizes[1],3,flags|MAP_FIXED,-1,0)==-ENOTSUP);
 assert(calls==before);
 puts("PASS libc large mmap admission, failures, cleanup and NX restrictions");
}
'''
with tempfile.TemporaryDirectory(prefix='libc-large-mmap-') as directory:
    path=Path(directory)
    (path/'test.c').write_text(preamble+body+checks)
    subprocess.run(['cc','-std=c11','-Wall','-Wextra','-Werror','-O2',str(path/'test.c'),'-o',str(path/'test')],check=True)
    subprocess.run([str(path/'test')],check=True)
