------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Linux x86-64 system-call numbers the libc's Ada implements
--  (docs/c-removal.md): musl calls __cubit_syscall with these. The values
--  are musl's (arch/x86_64/bits/syscall.h.in); tests/libc-ada checks each.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;

package CuBit.Linux_System_Calls with Pure, SPARK_Mode is

   subtype Number is Interfaces.C.long;

   SYS_read                : constant Number := 0;
   SYS_write               : constant Number := 1;
   SYS_open                : constant Number := 2;
   SYS_close               : constant Number := 3;
   SYS_stat                : constant Number := 4;
   SYS_fstat               : constant Number := 5;
   SYS_lstat               : constant Number := 6;
   SYS_poll                : constant Number := 7;
   SYS_lseek               : constant Number := 8;
   SYS_mmap                : constant Number := 9;
   SYS_mprotect            : constant Number := 10;
   SYS_munmap              : constant Number := 11;
   SYS_brk                 : constant Number := 12;
   SYS_rt_sigaction        : constant Number := 13;
   SYS_rt_sigprocmask      : constant Number := 14;
   SYS_ioctl               : constant Number := 16;
   SYS_pread64             : constant Number := 17;
   SYS_pwrite64            : constant Number := 18;
   SYS_readv               : constant Number := 19;
   SYS_writev              : constant Number := 20;
   SYS_access              : constant Number := 21;
   SYS_pipe                : constant Number := 22;
   SYS_select              : constant Number := 23;
   SYS_sched_yield         : constant Number := 24;
   SYS_mremap              : constant Number := 25;
   SYS_madvise             : constant Number := 28;
   SYS_dup                 : constant Number := 32;
   SYS_dup2                : constant Number := 33;
   SYS_nanosleep           : constant Number := 35;
   SYS_getpid              : constant Number := 39;
   SYS_socket              : constant Number := 41;
   SYS_connect             : constant Number := 42;
   SYS_accept              : constant Number := 43;
   SYS_sendto              : constant Number := 44;
   SYS_recvfrom            : constant Number := 45;
   SYS_shutdown            : constant Number := 48;
   SYS_bind                : constant Number := 49;
   SYS_listen              : constant Number := 50;
   SYS_getsockname         : constant Number := 51;
   SYS_getpeername         : constant Number := 52;
   SYS_socketpair          : constant Number := 53;
   SYS_setsockopt          : constant Number := 54;
   SYS_getsockopt          : constant Number := 55;
   SYS_exit                : constant Number := 60;
   SYS_kill                : constant Number := 62;
   SYS_uname               : constant Number := 63;
   SYS_fcntl               : constant Number := 72;
   SYS_fsync               : constant Number := 74;
   SYS_fdatasync           : constant Number := 75;
   SYS_truncate            : constant Number := 76;
   SYS_ftruncate           : constant Number := 77;
   SYS_getcwd              : constant Number := 79;
   SYS_chdir               : constant Number := 80;
   SYS_fchdir              : constant Number := 81;
   SYS_rename              : constant Number := 82;
   SYS_mkdir               : constant Number := 83;
   SYS_rmdir               : constant Number := 84;
   SYS_unlink              : constant Number := 87;
   SYS_readlink            : constant Number := 89;
   SYS_getuid              : constant Number := 102;
   SYS_getgid              : constant Number := 104;
   SYS_geteuid             : constant Number := 107;
   SYS_getegid             : constant Number := 108;
   SYS_sigaltstack         : constant Number := 131;
   SYS_prctl               : constant Number := 157;
   SYS_gettid              : constant Number := 186;
   SYS_tkill               : constant Number := 200;
   SYS_futex               : constant Number := 202;
   SYS_sched_getaffinity   : constant Number := 204;
   SYS_getdents64          : constant Number := 217;
   SYS_set_tid_address     : constant Number := 218;
   SYS_clock_gettime       : constant Number := 228;
   SYS_clock_getres        : constant Number := 229;
   SYS_clock_nanosleep     : constant Number := 230;
   SYS_exit_group          : constant Number := 231;
   SYS_tgkill              : constant Number := 234;
   SYS_openat              : constant Number := 257;
   SYS_mkdirat             : constant Number := 258;
   SYS_newfstatat          : constant Number := 262;
   SYS_unlinkat            : constant Number := 263;
   SYS_renameat            : constant Number := 264;
   SYS_readlinkat          : constant Number := 267;
   SYS_faccessat           : constant Number := 269;
   SYS_pselect6            : constant Number := 270;
   SYS_ppoll               : constant Number := 271;
   SYS_set_robust_list     : constant Number := 273;
   SYS_accept4             : constant Number := 288;
   SYS_dup3                : constant Number := 292;
   SYS_pipe2               : constant Number := 293;
   SYS_prlimit64           : constant Number := 302;
   SYS_renameat2           : constant Number := 316;
   SYS_getrandom           : constant Number := 318;
   SYS_membarrier          : constant Number := 324;
   SYS_pwritev2            : constant Number := 328;
   SYS_faccessat2          : constant Number := 439;

end CuBit.Linux_System_Calls;
