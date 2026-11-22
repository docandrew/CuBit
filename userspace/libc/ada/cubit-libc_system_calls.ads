------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Linux x86-64 system-call interface, implemented in the process over
--  CuBit system calls and services (docs/servo-port.md, docs/c-removal.md).
--  musl calls __cubit_syscall wherever it would execute `syscall`
--  (overlay/arch/x86_64/syscall_arch.h). Calls map onto CuBit as follows;
--  everything else returns -ENOSYS and is reported once.
--
--    threads      clone (clone.s), exit -> THREAD_CREATE/THREAD_EXIT
--    futexes      futex -> FUTEX_WAIT/FUTEX_WAKE (requeue wakes instead)
--    memory       brk -> SBRK; private mmap/whole munmap -> owned regions;
--                 unsupported protections and partial unmaps fail
--    time         clock_gettime/nanosleep -> the clock publication (no
--                 system call, docs/fast-clock.md), else the kernel
--                 microsecond clock; CLOCK_REALTIME adds the kernel's
--                 wall-clock offset
--    descriptors  read/write/close/fstat/poll/select -> fd.c's table of
--                 CuBit objects (stdout/stderr are the program's streams)
--    files        open/stat/... -> file.c, through filesystem.svc, inside
--                 the program's filesystem scopes
--    process      exit_group -> EXIT; getpid -> GETPID; kill of self exits
--    randomness   getrandom -> RDRAND (not yet the entropy service)
--    signals      none: masks and handlers are accepted and never fire
--
--  Results are a value, or -errno; errno itself is musl's to set.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Libc_System_Calls is

   subtype long is Interfaces.C.long;

   function Dispatch (N, A, B, C, D, E, F : long) return long
   with Export, Convention => C, External_Name => "__cubit_syscall";

   --  An argument the libc does not support: reported once per (What,
   --  Value) on the console (also used by the libc's C, fd.c and net.c).
   procedure Report_Unsupported (What : System.Address; Value : long)
   with Export, Convention => C, External_Name => "report_unsupported";

end CuBit.Libc_System_Calls;
