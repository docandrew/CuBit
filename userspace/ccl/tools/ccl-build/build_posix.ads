with Interfaces.C;
with Interfaces.C.Strings;

--  The Linux system calls the hosted executor and store need that the Ada
--  library does not offer: hard and symbolic links, exact modes, fsync,
--  and starting a program with its own directory, environment and stdout.
--  Linux-only: on CuBit the store is the Storehouse place and steps are
--  launched through procmgr with delegated places (docs/ccl-build.md).
package Build_POSIX is

   POSIX_Error : exception;

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
end Build_POSIX;
