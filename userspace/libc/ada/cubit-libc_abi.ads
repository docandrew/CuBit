------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The C library ABI the libc's Ada implements (docs/c-removal.md): errno
--  values, flags and structure layouts of the Linux x86-64 ABI, as musl's
--  headers define them. Each constant named as its C macro is checked
--  against musl's headers by tests/libc-ada (check_constants.py).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;

package CuBit.Libc_ABI with Pure, SPARK_Mode is

   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
   use type Interfaces.C.long;
   use type Interfaces.C.int;

   --  <errno.h>
   EPERM           : constant int := 1;
   ENOENT          : constant int := 2;
   EIO             : constant int := 5;
   E2BIG           : constant int := 7;
   EBADF           : constant int := 9;
   ENOTDIR         : constant int := 20;
   EISDIR          : constant int := 21;
   EXDEV           : constant int := 18;
   EMFILE          : constant int := 24;
   ESPIPE          : constant int := 29;
   EPIPE           : constant int := 32;
   EEXIST          : constant int := 17;
   EBUSY           : constant int := 16;
   ENOSPC          : constant int := 28;
   ENOTEMPTY       : constant int := 39;
   EPROTO          : constant int := 71;
   EADDRINUSE      : constant int := 98;
   ECONNRESET      : constant int := 104;
   ENOBUFS         : constant int := 105;
   ECONNREFUSED    : constant int := 111;
   EHOSTUNREACH    : constant int := 113;
   EALREADY        : constant int := 114;
   EINPROGRESS     : constant int := 115;
   ECHILD          : constant int := 10;
   EAGAIN          : constant int := 11;
   ENOMEM          : constant int := 12;
   EACCES          : constant int := 13;
   EFAULT          : constant int := 14;
   EINVAL          : constant int := 22;
   ENOTTY          : constant int := 25;
   EROFS           : constant int := 30;
   ERANGE          : constant int := 34;
   ENAMETOOLONG    : constant int := 36;
   ENOSYS          : constant int := 38;
   ENOTSOCK        : constant int := 88;
   ENOPROTOOPT     : constant int := 92;
   EPROTONOSUPPORT : constant int := 93;
   ENOTSUP         : constant int := 95;
   EOPNOTSUPP      : constant int := 95;
   EAFNOSUPPORT    : constant int := 97;
   EISCONN         : constant int := 106;
   ENOTCONN        : constant int := 107;
   ETIMEDOUT       : constant int := 110;

   --  <sys/wait.h>, <signal.h>
   WNOHANG    : constant int := 1;
   WUNTRACED  : constant int := 2;
   WCONTINUED : constant int := 8;
   SIGKILL    : constant int := 9;
   --  Wait status: exit code << 8, or the terminating signal.
   Status_Code_Shift : constant := 8;
   --  A process killed by signal N reports exit status 128 + N.
   Signal_Exit_Base : constant := 128;

   --  <sys/mman.h>
   PROT_NONE       : constant long := 0;
   PROT_READ       : constant long := 1;
   PROT_WRITE      : constant long := 2;
   MAP_PRIVATE     : constant long := 2;
   MAP_TYPE        : constant long := 15;
   MAP_FIXED       : constant long := 16;
   MAP_ANONYMOUS   : constant long := 32;
   MAP_NORESERVE   : constant long := 16384;
   MAP_STACK       : constant long := 131072;
   MADV_NORMAL     : constant long := 0;
   MADV_RANDOM     : constant long := 1;
   MADV_SEQUENTIAL : constant long := 2;
   MADV_WILLNEED   : constant long := 3;
   MADV_DONTNEED   : constant long := 4;
   MADV_FREE       : constant long := 8;

   --  <fcntl.h>, <unistd.h>, <sys/stat.h>, <limits.h>
   AT_FDCWD      : constant long := -100;
   AT_REMOVEDIR  : constant long := 512;
   AT_EMPTY_PATH : constant long := 4096;
   F_DUPFD       : constant int := 0;
   F_GETFD       : constant int := 1;
   F_SETFD       : constant int := 2;
   F_GETFL       : constant int := 3;
   F_SETFL       : constant int := 4;
   F_DUPFD_CLOEXEC : constant int := 1030;
   FD_CLOEXEC    : constant long := 1;
   O_RDONLY      : constant long := 0;
   O_WRONLY      : constant long := 1;
   O_RDWR        : constant long := 2;
   O_ACCMODE     : constant long := 8#10000003#;
   O_CREAT       : constant long := 8#100#;
   O_EXCL        : constant long := 8#200#;
   O_TRUNC       : constant long := 8#1000#;
   O_APPEND      : constant long := 8#2000#;
   O_DIRECTORY   : constant long := 8#200000#;
   O_NOFOLLOW    : constant long := 8#400000#;
   O_NONBLOCK    : constant long := 2048;
   SEEK_SET      : constant int := 0;
   SEEK_CUR      : constant int := 1;
   SEEK_END      : constant int := 2;
   S_IFIFO       : constant := 8#10000#;
   S_IFDIR       : constant := 8#40000#;
   S_IFREG       : constant := 8#100000#;
   S_IFSOCK      : constant := 8#140000#;
   O_CLOEXEC     : constant long := 524288;
   X_OK          : constant int := 1;
   W_OK          : constant int := 2;
   S_IXOTH       : constant := 1;
   S_IXGRP       : constant := 8;
   S_IXUSR       : constant := 64;
   PATH_MAX      : constant := 4096;

   --  <limits.h>
   IOV_MAX : constant := 1024;

   --  Futex operations (musl's src/internal/futex.h).
   FUTEX_WAIT           : constant int := 0;
   FUTEX_WAKE           : constant int := 1;
   FUTEX_REQUEUE        : constant int := 3;
   FUTEX_CMP_REQUEUE    : constant int := 4;
   FUTEX_WAIT_BITSET    : constant int := 9;
   FUTEX_PRIVATE        : constant int := 128;
   FUTEX_CLOCK_REALTIME : constant int := 256;
   --  Linux's <linux/futex.h>; musl never uses it, so does not define it.
   Futex_Wake_Bitset    : constant int := 10;

   --  <time.h>
   CLOCK_REALTIME         : constant long := 0;
   CLOCK_MONOTONIC        : constant long := 1;
   CLOCK_MONOTONIC_RAW    : constant long := 4;
   CLOCK_REALTIME_COARSE  : constant long := 5;
   CLOCK_MONOTONIC_COARSE : constant long := 6;
   CLOCK_BOOTTIME         : constant long := 7;
   TIMER_ABSTIME          : constant long := 1;

   --  <poll.h>, <sys/select.h>
   POLLIN     : constant := 1;
   POLLPRI    : constant := 2;
   POLLOUT    : constant := 4;
   POLLERR    : constant := 8;
   POLLHUP    : constant := 16;
   POLLNVAL   : constant := 32;
   POLLRDNORM : constant := 16#40#;
   POLLWRNORM : constant := 16#100#;
   POLLRDHUP  : constant := 16#2000#;
   FD_SETSIZE : constant := 1024;

   --  <sys/socket.h>, <netinet/in.h>, <netinet/tcp.h>, <sys/ioctl.h>
   AF_UNIX         : constant long := 1;
   AF_INET         : constant long := 2;
   AF_INET6        : constant long := 10;
   SOCK_STREAM     : constant long := 1;
   SOCK_NONBLOCK   : constant long := 2048;
   SOCK_CLOEXEC    : constant long := 524288;
   --  The socket type in the low bits of socket()'s type argument.
   Socket_Type_Mask : constant long := 16#F#;
   SOL_SOCKET      : constant long := 1;
   SO_REUSEADDR    : constant long := 2;
   SO_TYPE         : constant long := 3;
   SO_ERROR        : constant long := 4;
   SO_SNDBUF       : constant long := 7;
   SO_RCVBUF       : constant long := 8;
   SO_KEEPALIVE    : constant long := 9;
   SO_LINGER       : constant long := 13;
   IPPROTO_TCP     : constant long := 6;
   TCP_NODELAY     : constant long := 1;
   FIONBIO         : constant long := 21537;

   --  <netdb.h>, getaddrinfo's lookup (musl's src/network/lookup.h).
   AF_UNSPEC      : constant long := 0;
   AI_PASSIVE     : constant int := 1;
   AI_NUMERICHOST : constant int := 4;
   EAI_NONAME     : constant int := -2;
   EAI_FAMILY     : constant int := -6;
   EAI_MEMORY     : constant int := -10;
   MAXADDRS       : constant := 48;
   SHUT_WR        : constant int := 1;
   SHUT_RDWR      : constant int := 2;

   --  <sys/prctl.h>
   PR_SET_NAME : constant long := 15;
   PR_GET_NAME : constant long := 16;
   Thread_Name_Bytes : constant := 16;   --  TASK_COMM_LEN

   --  struct timespec and struct timeval: two 64-bit fields.
   type Timespec is record
      Seconds     : Integer_64;
      Nanoseconds : Integer_64;
   end record with Convention => C;
   type Timeval is record
      Seconds      : Integer_64;
      Microseconds : Integer_64;
   end record with Convention => C;

   --  struct iovec.
   type Io_Vector is record
      Base   : Unsigned_64;      --  an address
      Length : Unsigned_64;
   end record with Convention => C;

   --  struct pollfd.
   type Poll_Descriptor is record
      Descriptor : int;
      Events     : Integer_16;
      Returned   : Integer_16;
   end record with Convention => C;
   for Poll_Descriptor'Size use 64;

   --  struct utsname: six fields of 65 bytes.
   Utsname_Field_Bytes : constant := 65;
   Utsname_Fields      : constant := 6;
   --  struct rlimit: current and maximum.
   RLIM_INFINITY : constant Unsigned_64 := Unsigned_64'Last;
   --  The kernel's sigset_t and struct sigaction, as rt_sigprocmask and
   --  rt_sigaction report them (cleared: nothing blocked, no handler).
   Signal_Set_Maximum_Bytes : constant := 128;
   Signal_Action_Bytes      : constant := 32;
   --  struct rusage (<sys/resource.h>): two timevals and 14 longs.
   Rusage_Bytes : constant := 144;
   --  posix_spawn_file_actions_t (<spawn.h>): its __actions pointer.
   File_Actions_Pointer_Offset : constant := 8;
   --  musl's struct pthread (src/internal/pthread_impl.h): its tid, after
   --  self, dtv, prev, next, sysinfo and canary (x86-64).
   Pthread_Tid_Offset : constant := 48;

   --  <elf.h>: the auxiliary vector musl's start reads, and the program
   --  header that sets the main thread's stack size.
   AT_NULL      : constant := 0;
   AT_PHDR      : constant := 3;
   AT_PHENT     : constant := 4;
   AT_PHNUM     : constant := 5;
   AT_PAGESZ    : constant := 6;
   AT_RANDOM    : constant := 25;
   PT_GNU_STACK : constant := 16#6474E551#;
   --  Elf64_Ehdr: e_phoff, e_phentsize, e_phnum; Elf64_Phdr: p_type, p_memsz.
   Ehdr_Phoff_Offset     : constant := 32;
   Ehdr_Phentsize_Offset : constant := 54;
   Ehdr_Phnum_Offset     : constant := 56;
   Phdr_Type_Offset      : constant := 0;
   Phdr_Memsz_Offset     : constant := 40;

   --  <pthread.h>
   PTHREAD_CREATE_DETACHED : constant int := 1;
   --  pthread_attr_t: a union of 14 ints (x86-64).
   Pthread_Attribute_Bytes : constant := 56;

   --  The filesystem service's Directory.Inspection.V1 valid bits
   --  (CuBit.Filesystems; checked by tests/libc-ada).
   INSPECTED_SIZE   : constant := 1;
   INSPECTED_TIMES  : constant := 2;
   INSPECTED_MODE   : constant := 4;
   INSPECTED_LINKS  : constant := 8;
   INSPECTED_OWNER  : constant := 16;
   INSPECTED_OBJECT : constant := 32;

   --  Kernel PIDs are 1 .. 255 (kernel/src/process.ads).
   PID_Limit : constant := 256;

end CuBit.Libc_ABI;
