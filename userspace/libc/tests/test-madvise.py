"""Exercise the actual advice dispatch, including AWS-LC's invalid probe."""
from pathlib import Path
import subprocess,tempfile
s=(Path(__file__).resolve().parents[1]/'overlay/src/cubit/syscall.c').read_text()
branch=s[s.index('\tcase SYS_madvise:'):s.index('\tcase SYS_mprotect:')]
c=r"""
#define _GNU_SOURCE
#include <assert.h>
#include <errno.h>
#include <sys/mman.h>
#include <stdio.h>
#define SYS_madvise 1
static long invoke(long c) { switch(1) {
"""+branch+r"""
} return -ENOSYS; }
int main(void) {
 int hints[]={MADV_NORMAL,MADV_RANDOM,MADV_SEQUENTIAL,MADV_WILLNEED,MADV_DONTNEED,MADV_FREE};
 for(unsigned i=0;i<sizeof(hints)/sizeof(*hints);i++) assert(invoke(hints[i])==0);
 assert(invoke(-1)==-EINVAL);
 assert(invoke(MADV_WIPEONFORK)==-EINVAL);
 assert(invoke(MADV_DONTFORK)==-EINVAL);
 assert(invoke(0x7fffffff)==-EINVAL);
 puts("PASS advisory hints retained; invalid and unsupported fork advice rejected");
}
"""
with tempfile.TemporaryDirectory(prefix='madvise-advice-') as d:
 p=Path(d);(p/'test.c').write_text(c)
 subprocess.run(['cc','-std=c11','-Wall','-Wextra','-Werror','-O2',str(p/'test.c'),'-o',str(p/'test')],check=True)
 subprocess.run([str(p/'test')],check=True)
