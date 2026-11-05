with Interfaces.C;
with Interfaces.C.Strings;

--  The system calls ccl-build's executor and store make that the Ada
--  library does not offer, one body per platform, chosen by the project's
--  source directories (never a case in the core):
--    linux/build_platform.adb  hosted on Linux: hard and symbolic links,
--      exact modes and times, renameat2 (RENAME_NOREPLACE), fsync, flock,
--      fork and execve.
--    cubit/build_platform.adb  on CuBit (SELF-002): the same calls through
--      the libc and filesystem.svc where CuBit has them (flock, fsync of
--      files and directories, rename, O_EXCL); no links (Link copies,
--      symbolic links are refused), no mode bits or file times
--      (Change_Mode and Set_Times change nothing), a rename that must not
--      replace as an O_EXCL claim of the name and a rename over it, and
--      steps started through procmgr (posix_spawn) with delegated places.
package Build_Platform is

   Platform_Error : exception;

   --  The host this body runs on, which selects the toolchains a build
   --  runs its steps with (build-params.ccl's toolchains, by host).
   type Build_Host is (Linux, CuBit);
   function This_Host return Build_Host;

   --  Whether a tree imported from Path (a Toolchain's, or a fetched
   --  library's seed) is recorded in place (by its identity, in the store's
   --  imports/) rather than copied: a tree in a place nothing rewrites,
   --  whose programs find each other by absolute names. Linux: any tree in
   --  the Nix store. CuBit: every toolchain, the GCC port's tree in its
   --  read-only toolchain place, configured --prefix=/toolchain and named
   --  exactly by its drivers' launch tables. Its links may be absolute.
   function Records_In_Place (Path : String; Toolchain : Boolean) return Boolean;
   --  Keeps a tree recorded in place alive while Root_Name (beside its
   --  import record) exists: on Linux a Nix garbage-collector root
   --  (nix-store --add-root, its output in the directory Work); on CuBit
   --  nothing collects the toolchain place. Raises Platform_Error when the
   --  tree cannot be kept.
   procedure Keep_Recorded_Tree (Path, Root_Name, Work : String);

   --  Modes of Storehouse entries and of the directories made for steps.
   Read_Only_File : constant := 8#444#;
   Read_Only_Executable : constant := 8#555#;
   Read_Only_Directory : constant := 8#555#;
   Writable_Directory : constant := 8#755#;

   procedure Link (Existing, New_Name : String);
   --  Link, failing (False) instead of replacing an existing name.
   procedure Link_New (Existing, New_Name : String; Placed : out Boolean);
   procedure Symbolic_Link (Target, New_Name : String);
   function Link_Target (Name : String) return String;
   function Is_Symbolic_Link (Name : String) return Boolean;
   function Is_Executable (Name : String) return Boolean;
   procedure Change_Mode (Name : String; Mode : Interfaces.C.unsigned);
   --  Sets access and modification time to Seconds after the epoch.
   procedure Set_Times (Name : String; Seconds : Interfaces.C.long);
   --  Rename, failing (False) instead of replacing an existing entry.
   procedure Rename_New (From, To : String; Placed : out Boolean);
   --  Rename, replacing any existing entry of that name.
   procedure Rename_Replace (From, To : String);
   --  Rename, reporting False instead of failing when From is absent
   --  (another process moved or deleted it first).
   procedure Rename_If_Present (From, To : String; Moved : out Boolean);
   --  Makes a directory, reporting False when the name exists.
   procedure Make_Directory_New (Name : String; Created : out Boolean);
   --  Deletes a file or an empty directory; an absent name is no error.
   procedure Delete_File_If_Present (Name : String);
   procedure Delete_Directory_If_Present (Name : String);
   procedure Sync (Name : String);
   --  This process announces that it lives, in a store's lock directory,
   --  until it exits: where a process number cannot be probed (CuBit), by
   --  holding a lock there that Process_Alive then asks; on Linux nothing
   --  (kill (pid, 0) answers). Called once per store opened.
   procedure Mark_Alive (Lock_Directory : String);
   function Process_Alive (Process : Interfaces.C.int) return Boolean;
   function Process_Id return Interfaces.C.int;

   --  Advisory locks (flock) on lock files. The kernel releases a lock when
   --  its holder exits, however it exits, so a lock held is a live holder:
   --  no process identities to recycle, nothing stale to break. Lock files
   --  are opened close-on-exec, so a step a builder runs never inherits one.
   type Lock_Mode is (Shared, Exclusive);
   type Lock is private;
   No_Lock : constant Lock;
   function Held (L : Lock) return Boolean;
   --  Opens (creating it) and locks Name. With Wait, blocks until granted;
   --  without, returns No_Lock at once when another process holds it.
   --  A lock file another holder deleted meanwhile is opened again, so
   --  the lock returned is always on the file the name now denotes.
   function Acquire (Name : String; Mode : Lock_Mode; Wait : Boolean) return Lock;
   --  Unlocks and closes; with Delete the lock file is deleted first,
   --  while still held, so no other process can hold it under that name.
   procedure Release (L : in out Lock; Delete : Boolean)
   with Pre => Held (L), Post => not Held (L);

   --  Arguments as C strings, terminated by a null pointer.
   type Argument_List is array (Positive range <>) of Interfaces.C.Strings.chars_ptr;

   --  Runs Program with Arguments (Arguments (1) is argv[0]) in Directory
   --  and an empty environment. With a nonempty Standard_Output, the
   --  child's descriptor 1 is that file, created; otherwise it is inherited.
   --  Exit_Status is the exit code, or Signalled when a signal ended it.
   Signalled : constant := -1;
   procedure Run
     (Program : String; Arguments : Argument_List; Directory : String;
      Standard_Output : String; Exit_Status : out Integer);

private
   use type Interfaces.C.int;
   type Lock is record
      Descriptor : Interfaces.C.int := -1;
      Name : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
   end record;
   No_Lock : constant Lock := (Descriptor => -1, Name => Interfaces.C.Strings.Null_Ptr);
   function Held (L : Lock) return Boolean is (L.Descriptor >= 0);
end Build_Platform;
