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
--  on whole (docs/self-hosting.md, decision D2), or, through
--  cubit_posix_spawn_places, exactly the places the caller names: never
--  more than this program holds. procmgr checks both.
--
--  Names: one without '/' is procmgr's name as it stands. One with '/' is a
--  path, resolved like open's (CuBit.Path_Names, from the working
--  directory; "." and ".." taken out), and named relative to the system
--  volume when it is on it ("/toolchain/bin/as" and
--  "/toolchain/lib/../bin/as" are both "toolchain/bin/as"). The launch
--  table (may_launch) compares the result exactly.
--
--  File actions: only posix_spawn_file_actions_addchdir_np, the child's
--  working directory (resolved like chdir's name, in order), so a program
--  starts a child in another directory without changing its own. Others
--  are refused (ENOTSUP: no descriptors cross processes yet).
--
--  Not supported: spawn attributes (accepted, without effect: no signals,
--  groups or scheduler classes), PATH search, and waiting for processes
--  this program did not start.
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

   --  posix_spawn, but the child gets exactly the places in the region at
   --  Places_Region (Places_Bytes long; a CuBit.Launch_Grants region, none
   --  when 0) instead of this program's delegated places: a launcher gives
   --  each child only what it needs (ccl-build's steps, CCL-004). procmgr
   --  refuses the launch (EACCES) unless this program holds every entry
   --  with at least its rights; a malformed region is EINVAL.
   --  <cubit/launch.h>.
   function Spawn_With_Places
     (Result : System.Address; Path : System.Address;
      File_Actions : System.Address; Attributes : System.Address;
      Arguments, Environment : System.Address;
      Places_Region : System.Address; Places_Bytes : Interfaces.C.size_t) return int
   with Export, Convention => C, External_Name => "cubit_posix_spawn_places";

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

   --  An event another of the program's threads received (the stream
   --  dispatcher, a write polling for subscriptions): a child's exit is kept
   --  for Wait_For_Child; a control message is taken (below); anything else
   --  is ignored. Takes only its own lock, so it may be called with the
   --  stream lock or the child lock held.
   --
   --  Control messages (SEND_CONTROL, from the launcher or a holder of
   --  authority over this process: docs/ipc-delivery.md) are this libc's
   --  signals. It installs no handlers, so each takes a signal's default
   --  action, as kill does: Stop (like SIGTERM) and Interrupt (like SIGINT)
   --  end the process at once, with exit status 128 + that signal; Reload
   --  (like SIGHUP, re-read configuration) is ignored. A control is taken
   --  when the libc next receives events: at once while the stream
   --  dispatcher runs, otherwise in the next wait for a child. Processes
   --  this one started are not ended with it (no process groups).
   procedure Note_Event (Item : System.Address)
   with Export, Convention => C, External_Name => "__cubit_note_event";

end CuBit.Libc_Process;
