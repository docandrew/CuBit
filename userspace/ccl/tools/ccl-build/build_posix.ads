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
   procedure Sync (Name : String);
   function Process_Alive (Process : Interfaces.C.int) return Boolean;
   function Process_Id return Interfaces.C.int;

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

end Build_POSIX;
