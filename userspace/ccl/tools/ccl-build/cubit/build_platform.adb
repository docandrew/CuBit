with Ada.Containers.Indefinite_Vectors;
with GNAT.OS_Lib;
with System;
with Build_Timing;

--  Build_Platform on CuBit (SELF-002; docs/ccl-build.md, "On CuBit"): the
--  calls through CuBit's libc, which answers them from filesystem.svc and
--  procmgr. What CuBit does not have is mapped here, never in the core:
--    - no hard links: Link copies, Link_New copies into a name made with
--      O_EXCL (a reader may see it empty before it is filled; only claims
--      of older ccl-builds use it, and on CuBit there are none);
--    - no symbolic links: none are made or followed;
--    - no mode bits or file times: Change_Mode and Set_Times change
--      nothing. Storehouse entries are read-only because only the
--      software manager's access profile may write the Storehouse place;
--    - no RENAME_NOREPLACE: Rename_New claims the final name with O_EXCL
--      (an empty directory for a directory) and renames over its own
--      claim. A concurrent publisher of the same name loses the claim and
--      reports not placed; entries are content-addressed, so the only
--      cost of the claim's window is a reader that finds it empty and
--      quarantines it, after which the name is published again;
--    - imported toolchains are recorded in place (Records_In_Place):
--      the GCC port's tree is configured --prefix=/toolchain and its
--      driver's launch table names cc1, gnat1, as and ld there exactly;
--    - no kill (pid, 0): a process announces that it lives with a lock it
--      holds in each store's lock directory (Mark_Alive), which
--      filesystem.svc releases when the process exits however it ends;
--    - no fork: Run starts the program through procmgr (posix_spawn,
--      which passes this program's delegated places on to it, decision
--      D2) in Directory, given as a file action (addchdir), so this
--      program's own working directory never changes. A
--      program's standard output cannot be a file (no descriptors cross
--      processes): such a step is refused; the tools take an output name.
package body Build_Platform is
   use Interfaces.C;
   use Interfaces.C.Strings;

   --  The libc's values (musl x86-64; <fcntl.h>, <sys/file.h>, <errno.h>).
   O_RDONLY : constant int := 0;
   O_WRONLY : constant int := 1;
   O_RDWR : constant int := 2;
   O_CREAT : constant int := 8#100#;
   O_EXCL : constant int := 8#200#;
   O_CLOEXEC : constant int := 8#2000000#;
   ENOENT : constant := 2;
   EINTR : constant := 4;
   EINVAL : constant := 22;
   EEXIST : constant := 17;
   EWOULDBLOCK : constant := 11;
   LOCK_SH : constant int := 1;
   LOCK_EX : constant int := 2;
   LOCK_NB : constant int := 4;
   Lock_File_Mode : constant unsigned := 8#644#;
   Created_File_Mode : constant unsigned := 8#644#;
   Copy_Buffer_Bytes : constant := 65_536;
   Exit_Code_Shift : constant := 256;      --  WEXITSTATUS: bits 8 .. 15
   Exit_Code_Range : constant := 256;
   Signal_Mask : constant := 16#7F#;       --  WTERMSIG: bits 0 .. 6
   Alive_Prefix : constant String := "process-";

   --  struct stat (<bits/stat.h>, x86-64), 144 bytes.
   type Timespec is record
      Seconds : long;
      Nanoseconds : long;
   end record with Convention => C;
   type Reserved_Words is array (1 .. 3) of long with Convention => C;
   type File_Status is record
      Device, Inode, Links : unsigned_long;
      Mode, User, Group : unsigned;
      Padding : int;
      Special_Device : unsigned_long;
      Size, Block_Size, Blocks : long;
      Accessed, Modified, Changed : Timespec;
      Reserved : Reserved_Words;
   end record with Convention => C;

   function c_open (Name : chars_ptr; Flags : int; Mode : unsigned) return int
     with Import, Convention => C, External_Name => "open";
   function c_read (Descriptor : int; Buffer : System.Address; Count : size_t) return long
     with Import, Convention => C, External_Name => "read";
   function c_write (Descriptor : int; Buffer : System.Address; Count : size_t) return long
     with Import, Convention => C, External_Name => "write";
   function c_fsync (Descriptor : int) return int
     with Import, Convention => C, External_Name => "fsync";
   function c_close (Descriptor : int) return int
     with Import, Convention => C, External_Name => "close";
   function c_rename (Old_Name, New_Name : chars_ptr) return int
     with Import, Convention => C, External_Name => "rename";
   function c_flock (Descriptor, Operation : int) return int
     with Import, Convention => C, External_Name => "flock";
   function c_fstat (Descriptor : int; Status : access File_Status) return int
     with Import, Convention => C, External_Name => "fstat";
   function c_stat (Name : chars_ptr; Status : access File_Status) return int
     with Import, Convention => C, External_Name => "stat";
   function c_mkdir (Name : chars_ptr; Mode : unsigned) return int
     with Import, Convention => C, External_Name => "mkdir";
   function c_unlink (Name : chars_ptr) return int
     with Import, Convention => C, External_Name => "unlink";
   function c_rmdir (Name : chars_ptr) return int
     with Import, Convention => C, External_Name => "rmdir";
   function c_getpid return int
     with Import, Convention => C, External_Name => "getpid";
   --  posix_spawn_file_actions_t (<spawn.h>, x86-64): 80 bytes, opaque.
   type File_Actions_Words is array (1 .. 10) of unsigned_long with Convention => C;
   function c_file_actions_init (Actions : System.Address) return int
     with Import, Convention => C, External_Name => "posix_spawn_file_actions_init";
   function c_file_actions_addchdir (Actions : System.Address; Directory : chars_ptr) return int
     with Import, Convention => C, External_Name => "posix_spawn_file_actions_addchdir_np";
   function c_file_actions_destroy (Actions : System.Address) return int
     with Import, Convention => C, External_Name => "posix_spawn_file_actions_destroy";
   function c_posix_spawn
     (Process : access int; Program : chars_ptr; File_Actions, Attributes : System.Address;
      Arguments, Environment : System.Address) return int
     with Import, Convention => C, External_Name => "posix_spawn";
   function c_waitpid (Process : int; Status : access int; Options : int) return int
     with Import, Convention => C, External_Name => "waitpid";

   procedure Check (Result : int; What, Name : String) is
   begin
      if Result /= 0 then
         raise Platform_Error with What & " " & Name & ": errno" & Integer'Image (GNAT.OS_Lib.Errno);
      end if;
   end Check;

   function Is_Directory (Name : String) return Boolean is
      C_Name : chars_ptr := New_String (Name);
      Status : aliased File_Status;
      S_IFMT : constant unsigned := 8#170000#;
      S_IFDIR : constant unsigned := 8#040000#;
      Found : constant Boolean := c_stat (C_Name, Status'Access) = 0;
   begin
      Free (C_Name);
      return Found and then (Status.Mode and S_IFMT) = S_IFDIR;
   end Is_Directory;

   --  Copies Existing to New_Name, which it makes (O_EXCL). Placed is False
   --  when New_Name exists; other failures raise.
   procedure Copy_New (Existing, New_Name : String; Placed : out Boolean) is
      A : chars_ptr := New_String (Existing);
      B : chars_ptr := New_String (New_Name);
      From : constant int := c_open (A, O_RDONLY + O_CLOEXEC, 0);
      To : int := -1;
      Buffer : aliased char_array (1 .. Copy_Buffer_Bytes);
      Got, Put : long;
      Failed : Boolean := From < 0;
      Ignored : int;
   begin
      Placed := False;
      if not Failed then
         To := c_open (B, O_WRONLY + O_CREAT + O_EXCL + O_CLOEXEC, Created_File_Mode);
         if To < 0 then
            Failed := GNAT.OS_Lib.Errno /= EEXIST;
         else
            Placed := True;
            Build_Timing.Count (Build_Timing.Files_Copied);
            loop
               Got := c_read (From, Buffer'Address, Buffer'Length);
               exit when Got = 0;
               if Got < 0 then
                  exit when GNAT.OS_Lib.Errno /= EINTR;
               else
                  Put := c_write (To, Buffer'Address, size_t (Got));
                  Build_Timing.Count (Build_Timing.Bytes_Copied, Interfaces.Unsigned_64 (Got));
                  if Put /= Got then
                     Failed := True;
                     exit;
                  end if;
               end if;
            end loop;
            Failed := Failed or else Got < 0;
         end if;
      end if;
      if To >= 0 then
         Failed := c_close (To) /= 0 or else Failed;
      end if;
      if From >= 0 then
         Ignored := c_close (From);
      end if;
      Free (A); Free (B);
      if Failed then
         raise Platform_Error with "copy " & Existing & " to " & New_Name & ": errno"
           & Integer'Image (GNAT.OS_Lib.Errno);
      end if;
   end Copy_New;

   procedure Link (Existing, New_Name : String) is
      Placed : Boolean;
   begin
      Copy_New (Existing, New_Name, Placed);
      if not Placed then
         raise Platform_Error with "copy " & New_Name & ": exists";
      end if;
   end Link;

   procedure Link_New (Existing, New_Name : String; Placed : out Boolean) is
   begin
      Copy_New (Existing, New_Name, Placed);
   end Link_New;

   function This_Host return Build_Host is (CuBit);

   --  Every toolchain: the toolchain place is read-only to builders, and
   --  the GCC port's drivers name their programs there exactly. A fetched
   --  library's seed is copied.
   function Records_In_Place (Path : String; Toolchain : Boolean) return Boolean is
     (Toolchain and then Path'Length > 0);

   procedure Keep_Recorded_Tree (Path, Root_Name, Work : String) is null;

   procedure Symbolic_Link (Target, New_Name : String) is
   begin
      raise Platform_Error with "symbolic link " & New_Name & " to " & Target & ": none on CuBit";
   end Symbolic_Link;

   function Link_Target (Name : String) return String is
   begin
      raise Platform_Error with "readlink " & Name & ": no symbolic links on CuBit";
      return "";
   end Link_Target;

   function Is_Symbolic_Link (Name : String) return Boolean is (False);

   function Is_Executable (Name : String) return Boolean is
     (GNAT.OS_Lib.Is_Executable_File (Name));

   procedure Change_Mode (Name : String; Mode : unsigned) is null;

   procedure Set_Times (Name : String; Seconds : long) is null;

   procedure Rename_New (From, To : String; Placed : out Boolean) is
      A : chars_ptr := New_String (From);
      B : chars_ptr := New_String (To);
      Directory : constant Boolean := Is_Directory (From);
      Claim : int;
      Result : int;
   begin
      --  The claim: the name made exclusively, of From's kind.
      if Directory then
         Claim := c_mkdir (B, Writable_Directory);
      else
         Claim := c_open (B, O_WRONLY + O_CREAT + O_EXCL + O_CLOEXEC, Created_File_Mode);
         if Claim >= 0 then
            Claim := c_close (Claim);
         end if;
      end if;
      if Claim < 0 then
         Free (A); Free (B);
         Placed := False;
         if GNAT.OS_Lib.Errno /= EEXIST then
            raise Platform_Error with "claim " & To & ": errno" & Integer'Image (GNAT.OS_Lib.Errno);
         end if;
         return;
      end if;
      Result := c_rename (A, B);
      if Result /= 0 then
         declare
            Error : constant Integer := GNAT.OS_Lib.Errno;
            Ignored : constant int := (if Directory then c_rmdir (B) else c_unlink (B));
         begin
            Free (A); Free (B);
            raise Platform_Error with "rename " & To & ": errno" & Integer'Image (Error)
              & (if Ignored = 0 then "" else " (claim left)");
         end;
      end if;
      Free (A); Free (B);
      Placed := True;
   end Rename_New;

   procedure Rename_Replace (From, To : String) is
      A : chars_ptr := New_String (From);
      B : chars_ptr := New_String (To);
      Result : constant int := c_rename (A, B);
   begin
      Free (A); Free (B);
      Check (Result, "rename", To);
   end Rename_Replace;

   procedure Rename_If_Present (From, To : String; Moved : out Boolean) is
      A : chars_ptr := New_String (From);
      B : chars_ptr := New_String (To);
      Result : constant int := c_rename (A, B);
   begin
      Free (A); Free (B);
      Moved := Result = 0;
      if Result /= 0 and then GNAT.OS_Lib.Errno /= ENOENT then
         Check (Result, "rename", To);
      end if;
   end Rename_If_Present;

   procedure Make_Directory_New (Name : String; Created : out Boolean) is
      C_Name : chars_ptr := New_String (Name);
      Result : constant int := c_mkdir (C_Name, Writable_Directory);
   begin
      Free (C_Name);
      Created := Result = 0;
      if Result /= 0 and then GNAT.OS_Lib.Errno /= EEXIST then
         Check (Result, "mkdir", Name);
      end if;
   end Make_Directory_New;

   procedure Delete_File_If_Present (Name : String) is
      C_Name : chars_ptr := New_String (Name);
      Result : constant int := c_unlink (C_Name);
   begin
      Free (C_Name);
      if Result /= 0 and then GNAT.OS_Lib.Errno /= ENOENT then
         Check (Result, "unlink", Name);
      end if;
   end Delete_File_If_Present;

   procedure Delete_Directory_If_Present (Name : String) is
      C_Name : chars_ptr := New_String (Name);
      Result : constant int := c_rmdir (C_Name);
   begin
      Free (C_Name);
      if Result /= 0 and then GNAT.OS_Lib.Errno /= ENOENT then
         Check (Result, "rmdir", Name);
      end if;
   end Delete_Directory_If_Present;

   function Acquire (Name : String; Mode : Lock_Mode; Wait : Boolean) return Lock is
      Operation : constant int :=
        (case Mode is when Shared => LOCK_SH, when Exclusive => LOCK_EX) + (if Wait then 0 else LOCK_NB);
   begin
      loop
         declare
            C_Name : chars_ptr := New_String (Name);
            Descriptor : constant int := c_open (C_Name, O_RDWR + O_CREAT + O_CLOEXEC, Lock_File_Mode);
            Result : int;
            Opened, Named : aliased File_Status;
            Ignored : int;
         begin
            if Descriptor < 0 then
               Free (C_Name);
               raise Platform_Error with "open " & Name & ": errno" & Integer'Image (GNAT.OS_Lib.Errno);
            end if;
            loop
               Result := c_flock (Descriptor, Operation);
               exit when Result = 0 or else GNAT.OS_Lib.Errno /= EINTR;
            end loop;
            if Result /= 0 then
               Ignored := c_close (Descriptor);
               Free (C_Name);
               if GNAT.OS_Lib.Errno = EWOULDBLOCK and then not Wait then
                  return No_Lock;
               end if;
               raise Platform_Error with "flock " & Name & ": errno" & Integer'Image (GNAT.OS_Lib.Errno);
            end if;
            --  As on Linux: only a lock on the file the name now denotes.
            if c_fstat (Descriptor, Opened'Access) = 0 and then c_stat (C_Name, Named'Access) = 0
              and then Opened.Device = Named.Device and then Opened.Inode = Named.Inode
            then
               return (Descriptor => Descriptor, Name => C_Name);
            end if;
            Ignored := c_close (Descriptor);
            Free (C_Name);
         end;
      end loop;
   end Acquire;

   procedure Release (L : in out Lock; Delete : Boolean) is
   begin
      if Delete then
         Check ((if c_unlink (L.Name) = 0 then 0 else (if GNAT.OS_Lib.Errno = ENOENT then 0 else -1)),
                "unlink", Value (L.Name));
      end if;
      Check (c_close (L.Descriptor), "close", Value (L.Name));
      Free (L.Name);
      L := No_Lock;
   end Release;

   --  The lock directories this process announced itself in, and its
   --  locks there, held until it exits.
   package Name_Lists is new Ada.Containers.Indefinite_Vectors (Positive, String);
   package Lock_Lists is new Ada.Containers.Indefinite_Vectors (Positive, Lock);
   Alive_Directories : Name_Lists.Vector;
   Alive_Locks : Lock_Lists.Vector;

   function Image (Process : int) return String is
      Text : constant String := int'Image (Process);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Mark_Alive (Lock_Directory : String) is
   begin
      if Alive_Directories.Contains (Lock_Directory) then
         return;
      end if;
      --  Waits only while another process probes it (Process_Alive).
      Alive_Locks.Append
        (Acquire (Lock_Directory & "/" & Alive_Prefix & Image (c_getpid), Exclusive, Wait => True));
      Alive_Directories.Append (Lock_Directory);
   end Mark_Alive;

   --  Alive while it holds its lock in a directory this process uses.
   function Process_Alive (Process : int) return Boolean is
   begin
      if Process = c_getpid then
         return True;
      elsif Alive_Directories.Is_Empty then
         raise Platform_Error with "process liveness asked before Mark_Alive";
      end if;
      for Directory of Alive_Directories loop
         declare
            Probe : Lock := Acquire (Directory & "/" & Alive_Prefix & Image (Process), Exclusive,
                                     Wait => False);
         begin
            if not Held (Probe) then
               return True;
            end if;
            Release (Probe, Delete => True);   --  a dead process's, or none
         end;
      end loop;
      return False;
   end Process_Alive;

   procedure Sync (Name : String) is
      C_Name : chars_ptr := New_String (Name);
      Descriptor : constant int := c_open (C_Name, O_RDONLY + O_CLOEXEC, 0);
   begin
      Free (C_Name);
      if Descriptor < 0 then
         raise Platform_Error with "open " & Name;
      end if;
      --  EINVAL: the volume cannot synchronize because it keeps nothing
      --  across a restart (the live RAM root, @mem:0): a store there is
      --  as durable as it can be once written. Any other failure stands.
      if c_fsync (Descriptor) /= 0 and then GNAT.OS_Lib.Errno /= EINVAL then
         raise Platform_Error with "fsync " & Name & ": errno" & Integer'Image (GNAT.OS_Lib.Errno);
      end if;
      Check (c_close (Descriptor), "close", Name);
      Build_Timing.Count (Build_Timing.Syncs);
   end Sync;

   function Process_Id return int is (c_getpid);

   procedure Run
     (Program : String; Arguments : Argument_List; Directory : String;
      Standard_Output : String; Exit_Status : out Integer)
   is
      C_Program : chars_ptr;
      C_Directory : chars_ptr;
      Vector : aliased Argument_List (1 .. Arguments'Length + 1) := [others => Null_Ptr];
      Environment : aliased constant Argument_List (1 .. 1) := [others => Null_Ptr];
      Actions : aliased File_Actions_Words := [others => 0];
      Child : aliased int := 0;
      Status : aliased int := 0;
      Spawned : int;
      Ignored : int;
   begin
      if Standard_Output'Length > 0 then
         raise Platform_Error with Program & ": standard output to " & Standard_Output
           & " (no descriptors cross processes on CuBit; the tool must name its output)";
      end if;
      Vector (1 .. Arguments'Length) := Arguments;
      C_Program := New_String (Program);
      C_Directory := New_String (Directory);
      --  The child starts in Directory; this program's own working
      --  directory never changes (posix_spawn_file_actions_addchdir_np).
      Check (c_file_actions_init (Actions'Address), "posix_spawn_file_actions_init", Program);
      Spawned := c_file_actions_addchdir (Actions'Address, C_Directory);
      if Spawned = 0 then
         --  Until procmgr has read the image, the kernel made the process
         --  and procmgr answered (Build_Timing).
         Build_Timing.Count (Build_Timing.Spawns);
         Build_Timing.Enter (Build_Timing.Spawn);
         Spawned := c_posix_spawn (Child'Access, C_Program, Actions'Address, System.Null_Address,
                                   Vector'Address, Environment'Address);
         Build_Timing.Leave;
      end if;
      Ignored := c_file_actions_destroy (Actions'Address);
      Free (C_Program); Free (C_Directory);
      if Spawned /= 0 then
         raise Platform_Error with "posix_spawn " & Program & " in " & Directory & ": error"
           & Integer'Image (Integer (Spawned));
      end if;
      declare
         Timed : Build_Timing.Scope (Build_Timing.Tool_Wait);
         pragma Unreferenced (Timed);
      begin
         if c_waitpid (Child, Status'Access, 0) /= Child then
            raise Platform_Error with "waitpid " & Program;
         end if;
      end;
      if Integer (Status) mod (Signal_Mask + 1) = 0 then
         Exit_Status := Integer (Status) / Exit_Code_Shift mod Exit_Code_Range;
      else
         Exit_Status := Signalled;
      end if;
   end Run;

end Build_Platform;
