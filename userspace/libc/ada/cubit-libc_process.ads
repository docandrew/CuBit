------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's process functions (docs/process-arguments.md), in Ada for C
--  programs (docs/c-removal.md). There is no fork and no exec: procmgr
--  starts a program by name with OP_LAUNCH, given a launch block, and the
--  kernel tells the launcher when it ends (EVENT_CHILD_EXIT). Spawn
--  and Wait_For_Child/Wait_With_Usage are built on that; Run_Command and Open_Command_Pipe have no shell.
--
--  Authority: procmgr's endpoint (manifest: (request-service
--  process-manager read-write process-manager)). Arguments, environment and
--  working directory are data. The child holds its own manifest's
--  authority and the places this program's launcher delegated to it, passed
--  on whole (docs/self-hosting.md, decision D2): never this program's own
--  manifest scopes, never more than it was given. procmgr checks both.
--
--  Names: one without '/' is procmgr's name as it stands. One with '/' is a
--  path, resolved like open's (CuBit.Path_Names, from the working
--  directory; "." and ".." taken out), and named relative to the system
--  volume when it is on it ("/toolchain/bin/as" and
--  "/toolchain/lib/../bin/as" are both "toolchain/bin/as"). The launch
--  table (may_launch) compares the result exactly.
--
--  Not supported: file actions (ENOTSUP: no descriptors cross processes
--  yet), spawn attributes (accepted, without effect: no signals, groups or
--  scheduler classes), PATH search, and waiting for processes this program
--  did not start.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Libc_Process is

   subtype int is Interfaces.C.int;

   function Spawn
     (Result : System.Address; Path : System.Address;
      File_Actions : System.Address; Attributes : System.Address;
      Arguments, Environment : System.Address) return int
   with Export, Convention => C, External_Name => "posix_spawn";

   --  The same: names are procmgr's, there is no PATH search.
   function Spawn_By_Search
     (Result : System.Address; Path : System.Address;
      File_Actions : System.Address; Attributes : System.Address;
      Arguments, Environment : System.Address) return int
   with Export, Convention => C, External_Name => "posix_spawnp";

   function Wait_With_Usage
     (Process : int; Status : System.Address; Options : int;
      Usage : System.Address) return int
   with Export, Convention => C, External_Name => "wait4";

   function Wait_For_Child (Process : int; Status : System.Address; Options : int)
     return int
   with Export, Convention => C, External_Name => "waitpid";

   --  No command processor: Run_Command(NULL) is 0, any command -1 (ENOSYS).
   function Run_Command (Command : System.Address) return int
   with Export, Convention => C, External_Name => "system";

   function Open_Command_Pipe (Command, Mode : System.Address) return System.Address
   with Export, Convention => C, External_Name => "popen";

   --  <cubit/debug.h>: the kernel console, a temporary debugging channel.
   procedure Debug_Write
     (Text : System.Address; Length : Interfaces.C.size_t)
   with Export, Convention => C, External_Name => "cubit_debug_write";

end CuBit.Libc_Process;
