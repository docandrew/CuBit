------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's descriptors (docs/servo-connector.md, docs/c-removal.md): a local
--  table of CuBit objects. There are no implicit Unix descriptors; one
--  exists only for a CuBit object the program has:
--
--    each descriptor the program's manifest maps onto one of its output
--           connectors (docs/ccl-launch-parameters.md, "Connectors, not stdio"): that
--           connector's ring (CuBit.Streams), created on first write with the
--           connector's declared size and element type; an unmapped descriptor
--           (1 and 2 included) is not open (EBADF). CuBit has no stdout.
--    0      none (inlets are not delivered yet)
--    3...   files and directories opened through filesystem.svc (file.c);
--           pipes and socket pairs, in-process rings (CuBit.Libc_Rings);
--           TCP sockets over netstack (net.c)
--
--  Streams are not terminals. The process's mailbox is the libc's: a
--  dispatcher thread receives every message and serves stream
--  subscriptions.
--
--  The rules (open flags, lseek, fcntl, block counts), the pipe rings and
--  the directory records are proved SPARK units; this package holds the
--  table, the locks and the calls to the kernel and the libc's other parts.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Libc_Descriptors is

   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
   subtype size_t is Interfaces.C.size_t;
   subtype unsigned_long is Interfaces.C.unsigned_long;
   use type Interfaces.C.int;
   use type System.Address;

   --  Adopt what procmgr attached to the launch block after the strings:
   --  the ring table of rings the launcher lent (CuBit.Outlet_Rings), if
   --  any, then the program's description (CuBit.Program_Descriptions):
   --  which descriptors write which connectors, and into which lent rings.
   --  Called once at start, before main; a malformed or absent description
   --  maps nothing.
   procedure Adopt_Ports (Trailer : System.Address; Length : Natural);

   function Writev (Fd : int; Vectors : System.Address; Count : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_writev";
   function Read (Fd : int; Buffer : System.Address; Count : size_t) return long
   with Export, Convention => C, External_Name => "__cubit_fd_read";
   function Pread (Fd : int; Buffer : System.Address; Count : size_t; Offset : long)
     return long
   with Export, Convention => C, External_Name => "__cubit_fd_pread";
   function Pwrite (Fd : int; Buffer : System.Address; Count : size_t; Offset : long)
     return long
   with Export, Convention => C, External_Name => "__cubit_fd_pwrite";
   function Fsync (Fd : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_fsync";
   function Lseek (Fd : int; Offset : long; Whence : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_lseek";
   function Open (Path : System.Address; Flags : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_open";
   function Close (Fd : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_close";
   function Fstat (Fd : int; Status : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_fd_fstat";
   function Fcntl (Fd : int; Command : int; Argument : long) return long
   with Export, Convention => C, External_Name => "__cubit_fd_fcntl";
   function Getdents (Fd : int; Buffer : System.Address; Count : size_t) return long
   with Export, Convention => C, External_Name => "__cubit_fd_getdents";
   function Poll (Polls : System.Address; Count, Deadline : unsigned_long) return long
   with Export, Convention => C, External_Name => "__cubit_fd_poll";
   function Pipe (Pair : System.Address; Flags : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_pipe";
   function Socketpair (Pair : System.Address; Flags : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_socketpair";
   function Dup (Fd, Minimum, Target, Close_On_Exec : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_dup";
   function Path_Stat (Path, Status : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_path_stat";
   function At_Path (Directory : int; Path, Buffer : System.Address; Size : size_t;
                     Result : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_at_path";
   function Fchdir (Fd : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_fchdir";
   function Ftruncate (Fd : int; Length : long) return long
   with Export, Convention => C, External_Name => "__cubit_fd_ftruncate";
   function Path_Truncate (Path : System.Address; Length : long) return long
   with Export, Convention => C, External_Name => "__cubit_path_truncate";
   function Socket_Tcp (Flags : int) return long
   with Export, Convention => C, External_Name => "__cubit_fd_socket_tcp";
   function Accept_Connection (Fd : int; Address, Length : System.Address; Flags : int)
     return long
   with Export, Convention => C, External_Name => "__cubit_fd_accept";
   function Tcp_Of (Fd : int; Nonblocking : System.Address) return System.Address
   with Export, Convention => C, External_Name => "__cubit_fd_tcp";
   function Is_Socket (Fd : int) return int
   with Export, Convention => C, External_Name => "__cubit_fd_is_socket";

   --  The readiness futex, shared with net.c: bumped whenever a descriptor
   --  may have become ready; poll and blocking reads and writes wait on it.
   procedure Readiness_Changed
   with Export, Convention => C, External_Name => "__cubit_readiness_changed";
   function Readiness_Sequence return int
   with Export, Convention => C, External_Name => "__cubit_readiness_seq";
   procedure Readiness_Wait (Sequence : int; Deadline : unsigned_long)
   with Export, Convention => C, External_Name => "__cubit_readiness_wait";

end CuBit.Libc_Descriptors;
