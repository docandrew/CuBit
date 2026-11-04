with GNAT.OS_Lib;
with System;

package body Build_POSIX is
   use Interfaces.C;
   use Interfaces.C.Strings;

   --  Linux x86-64 values (<fcntl.h>, <stdio.h>, <sys/wait.h>).
   AT_FDCWD : constant int := -100;
   AT_SYMLINK_NOFOLLOW : constant int := 16#100#;
   RENAME_NOREPLACE : constant unsigned := 1;
   EEXIST : constant := 17;
   O_RDONLY : constant int := 0;
   O_WRONLY : constant int := 1;
   O_CREAT : constant int := 8#100#;
   O_EXCL : constant int := 8#200#;
   O_TRUNC : constant int := 8#1000#;
   O_RDWR : constant int := 2;
   O_CLOEXEC : constant int := 8#2000000#;
   ENOENT : constant := 2;
   EINTR : constant := 4;
   EWOULDBLOCK : constant := 11;
   LOCK_SH : constant int := 1;
   LOCK_EX : constant int := 2;
   LOCK_NB : constant int := 4;
   Lock_File_Mode : constant unsigned := 8#644#;
   Created_File_Mode : constant unsigned := 8#644#;
   Standard_Output_Descriptor : constant int := 1;
   Exec_Failed_Status : constant int := 127;
   Exit_Code_Shift : constant := 256;      --  WEXITSTATUS: bits 8 .. 15
   Exit_Code_Range : constant := 256;
   Signal_Mask : constant := 16#7F#;       --  WTERMSIG: bits 0 .. 6
   Link_Text_Limit : constant := 4096;     --  PATH_MAX

   type Timespec is record
      Seconds : long;
      Nanoseconds : long;
   end record with Convention => C;
   type Timespec_Pair is array (1 .. 2) of Timespec with Convention => C;

   --  struct stat on Linux x86-64 (<bits/struct_stat.h>), 144 bytes.
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

   function c_link (Existing, New_Name : chars_ptr) return int
     with Import, Convention => C, External_Name => "link";
   function c_symlink (Target, New_Name : chars_ptr) return int
     with Import, Convention => C, External_Name => "symlink";
   function c_readlink (Name : chars_ptr; Buffer : System.Address; Size : size_t) return long
     with Import, Convention => C, External_Name => "readlink";
   function c_chmod (Name : chars_ptr; Mode : unsigned) return int
     with Import, Convention => C, External_Name => "chmod";
   function c_utimensat (Directory : int; Name : chars_ptr; Times : access constant Timespec_Pair;
                         Flags : int) return int
     with Import, Convention => C, External_Name => "utimensat";
   function c_renameat2 (Old_Directory : int; Old_Name : chars_ptr; New_Directory : int;
                         New_Name : chars_ptr; Flags : unsigned) return int
     with Import, Convention => C, External_Name => "renameat2";
   function c_open (Name : chars_ptr; Flags : int; Mode : unsigned) return int
     with Import, Convention => C, External_Name => "open";
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
   function c_kill (Process, Signal : int) return int
     with Import, Convention => C, External_Name => "kill";
   function c_getpid return int
     with Import, Convention => C, External_Name => "getpid";
   function c_fork return int
     with Import, Convention => C, External_Name => "fork";
   function c_chdir (Name : chars_ptr) return int
     with Import, Convention => C, External_Name => "chdir";
   function c_dup2 (From, To : int) return int
     with Import, Convention => C, External_Name => "dup2";
   function c_execve (Program : chars_ptr; Arguments, Environment : System.Address) return int
     with Import, Convention => C, External_Name => "execve";
   procedure c_exit (Status : int)
     with Import, Convention => C, External_Name => "_exit", No_Return;
   function c_waitpid (Process : int; Status : access int; Options : int) return int
     with Import, Convention => C, External_Name => "waitpid";

   procedure Check (Result : int; What, Name : String) is
   begin
      if Result /= 0 then
         raise POSIX_Error with What & " " & Name & ": errno" & Integer'Image (GNAT.OS_Lib.Errno);
      end if;
   end Check;

   procedure Link (Existing, New_Name : String) is
      A : chars_ptr := New_String (Existing);
      B : chars_ptr := New_String (New_Name);
      Result : constant int := c_link (A, B);
   begin
      Free (A); Free (B);
      Check (Result, "link", New_Name);
   end Link;

   procedure Link_New (Existing, New_Name : String; Placed : out Boolean) is
      A : chars_ptr := New_String (Existing);
      B : chars_ptr := New_String (New_Name);
      Result : constant int := c_link (A, B);
   begin
      Free (A); Free (B);
      Placed := Result = 0;
      if Result /= 0 and then GNAT.OS_Lib.Errno /= EEXIST then
         Check (Result, "link", New_Name);
      end if;
   end Link_New;

   procedure Symbolic_Link (Target, New_Name : String) is
      A : chars_ptr := New_String (Target);
      B : chars_ptr := New_String (New_Name);
      Result : constant int := c_symlink (A, B);
   begin
      Free (A); Free (B);
      Check (Result, "symlink", New_Name);
   end Symbolic_Link;

   function Link_Target (Name : String) return String is
      Buffer : aliased char_array (1 .. Link_Text_Limit);
      C_Name : chars_ptr := New_String (Name);
      Length : constant long := c_readlink (C_Name, Buffer'Address, Buffer'Length);
   begin
      Free (C_Name);
      if Length < 0 or else Length >= Link_Text_Limit then
         raise POSIX_Error with "readlink " & Name;
      end if;
      return To_Ada (Buffer (1 .. size_t (Length)), Trim_Nul => False);
   end Link_Target;

   function Is_Symbolic_Link (Name : String) return Boolean is
     (GNAT.OS_Lib.Is_Symbolic_Link (Name));

   function Is_Executable (Name : String) return Boolean is
     (GNAT.OS_Lib.Is_Executable_File (Name));

   procedure Change_Mode (Name : String; Mode : unsigned) is
      C_Name : chars_ptr := New_String (Name);
      Result : constant int := c_chmod (C_Name, Mode);
   begin
      Free (C_Name);
      Check (Result, "chmod", Name);
   end Change_Mode;

   procedure Set_Times (Name : String; Seconds : long) is
      Times : aliased constant Timespec_Pair := [others => (Seconds, 0)];
      C_Name : chars_ptr := New_String (Name);
      Result : constant int := c_utimensat (AT_FDCWD, C_Name, Times'Access, AT_SYMLINK_NOFOLLOW);
   begin
      Free (C_Name);
      Check (Result, "utimensat", Name);
   end Set_Times;

   procedure Rename_New (From, To : String; Placed : out Boolean) is
      A : chars_ptr := New_String (From);
      B : chars_ptr := New_String (To);
      Result : constant int := c_renameat2 (AT_FDCWD, A, AT_FDCWD, B, RENAME_NOREPLACE);
   begin
      Free (A); Free (B);
      Placed := Result = 0;
      if Result /= 0 and then GNAT.OS_Lib.Errno /= EEXIST then
         Check (Result, "rename", To);
      end if;
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
               raise POSIX_Error with "open " & Name & ": errno" & Integer'Image (GNAT.OS_Lib.Errno);
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
               raise POSIX_Error with "flock " & Name & ": errno" & Integer'Image (GNAT.OS_Lib.Errno);
            end if;
            --  The holder before us may have deleted the file while we
            --  waited: then our lock is on a file no longer named, and the
            --  next process would create and lock a new one. Only a lock on
            --  the file the name denotes counts.
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

   --  kill (Process, 0) probes without signalling; EPERM still means alive.
   function Process_Alive (Process : int) return Boolean is
      Probe : constant int := 0;
      EPERM : constant := 1;
   begin
      return c_kill (Process, Probe) = 0 or else GNAT.OS_Lib.Errno = EPERM;
   end Process_Alive;

   procedure Sync (Name : String) is
      C_Name : chars_ptr := New_String (Name);
      Descriptor : constant int := c_open (C_Name, O_RDONLY, 0);
   begin
      Free (C_Name);
      if Descriptor < 0 then
         raise POSIX_Error with "open " & Name;
      end if;
      Check (c_fsync (Descriptor), "fsync", Name);
      Check (c_close (Descriptor), "close", Name);
   end Sync;

   function Process_Id return int is (c_getpid);

   procedure Run
     (Program : String; Arguments : Argument_List; Directory : String;
      Standard_Output : String; Exit_Status : out Integer)
   is
      --  Everything the child touches is made before fork: after it, the
      --  child only makes system calls.
      C_Program : chars_ptr := New_String (Program);
      C_Directory : chars_ptr := New_String (Directory);
      C_Output : chars_ptr :=
        (if Standard_Output'Length > 0 then New_String (Standard_Output) else Null_Ptr);
      Vector : aliased Argument_List (1 .. Arguments'Length + 1) := [others => Null_Ptr];
      Environment : aliased constant Argument_List (1 .. 1) := [others => Null_Ptr];
      Child : int;
      Status : aliased int := 0;
      Ignored : int;
   begin
      Vector (1 .. Arguments'Length) := Arguments;
      Child := c_fork;
      if Child = 0 then
         if c_chdir (C_Directory) /= 0 then
            c_exit (Exec_Failed_Status);
         end if;
         if C_Output /= Null_Ptr then
            declare
               Descriptor : constant int :=
                 c_open (C_Output, O_WRONLY + O_CREAT + O_EXCL + O_TRUNC, Created_File_Mode);
            begin
               if Descriptor < 0 or else c_dup2 (Descriptor, Standard_Output_Descriptor) < 0 then
                  c_exit (Exec_Failed_Status);
               end if;
               Ignored := c_close (Descriptor);
            end;
         end if;
         Ignored := c_execve (C_Program, Vector'Address, Environment'Address);
         c_exit (Exec_Failed_Status);
      end if;
      Free (C_Program); Free (C_Directory); Free (C_Output);
      if Child < 0 then
         raise POSIX_Error with "fork " & Program;
      end if;
      if c_waitpid (Child, Status'Access, 0) /= Child then
         raise POSIX_Error with "waitpid " & Program;
      end if;
      --  WIFEXITED: no terminating signal in the low seven bits.
      if Integer (Status) mod (Signal_Mask + 1) = 0 then
         Exit_Status := Integer (Status) / Exit_Code_Shift mod Exit_Code_Range;
      else
         Exit_Status := Signalled;
      end if;
   end Run;

end Build_POSIX;
