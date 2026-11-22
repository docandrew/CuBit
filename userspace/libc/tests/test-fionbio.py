"""Exercise the actual CuBit ioctl dispatch against a descriptor-state stub."""
from pathlib import Path
import subprocess
import sys
import tempfile
source=Path(sys.argv[1]) if len(sys.argv)>1 else Path(__file__).resolve().parents[1]/'overlay/src/cubit/syscall.c'
s=source.read_text();branch=s[s.index('\tcase SYS_ioctl:'):s.index('\tcase SYS_fstat:')]
c=r'''
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <stdio.h>
#define SYS_ioctl 1
static long flags;
static int writes;
static long __cubit_fd_fcntl(int fd,int op,long value) {
    if(fd!=7)return -EBADF;
    if(op==F_GETFL)return flags;
    assert(op==F_SETFL);flags=value;writes++;return 0;
}
static long invoke(unsigned long a,unsigned long b,unsigned long c) {
    switch(1) {
'''+branch+r'''
    }return -ENOSYS;
}
int main(void) {
    int on=1,off=0;
    flags=O_RDWR|O_APPEND;
    assert(invoke(7,FIONBIO,(unsigned long)&on)==0);
    assert(flags==(O_RDWR|O_APPEND|O_NONBLOCK));
    assert(invoke(7,FIONBIO,(unsigned long)&off)==0);
    assert(flags==(O_RDWR|O_APPEND));
    assert(invoke(7,FIONBIO,0)==-EFAULT);
    assert(invoke(-1,FIONBIO,(unsigned long)&on)==-EBADF);
    assert(invoke(7,0x1234,(unsigned long)&on)==-ENOTTY);
    assert(writes==2);
    puts("PASS FIONBIO enable/disable, preserved flags, bad descriptor/pointer and unknown ioctl");
}
'''
with tempfile.TemporaryDirectory(prefix='fionbio-') as tmp:
    p=Path(tmp);(p/'test.c').write_text(c)
    subprocess.run(['cc','-std=c11','-Wall','-Wextra','-O2',str(p/'test.c'),'-o',str(p/'test')],check=True)
    subprocess.run([str(p/'test')],check=True)
