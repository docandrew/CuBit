------------------------------------------------------------------------------
--  CuBit live-storage diagnostic
--
--  Exercises create, write, close, stale-handle rejection, reopen, and read
--  against the live system's writable NVMe filesystem.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Directory_Pages;
with CuBit.Filesystem_Queues;
with CuBit.Filesystem_Sessions;
with CuBit.Filesystem_Events;
with CuBit.File_Access;
with CuBit.Volume_Descriptions;
with CuBit.Channels;

procedure main is
   use ASCII;

   PAGE_SIZE : constant Unsigned_64 := 4096;
   PATH : constant String := "@nvme:0/config.dat";
   CREATE_PATH : constant String := "@nvme:0/cubit-new.dat";
   PAYLOAD : constant String := "CuBit storage ready";

   raw      : Unsigned_64;
   aligned  : Unsigned_64;
   grantRef : CuBit.Memory_Grants.Grant_Reference;
   grantOk  : Boolean;

   function exerciseOwnedMemory return Boolean is
      A, B, Again : Unsigned_64;
      Bytes : constant Unsigned_64 := 3 * PAGE_SIZE;
      function Release (Base, Size : Unsigned_64) return Boolean is
        (syscall (SYSCALL_RELEASE_OWNED_MEMORY, Base, Size) = 0);
      function Check (Base : Unsigned_64; Expected, Value : Unsigned_8) return Boolean is
         type Data is array (1 .. Natural (Bytes)) of Unsigned_8;
         Buffer : Data with Import, Volatile,
           Address => To_Address (Integer_Address (Base));
      begin
         for I in Buffer'Range loop
            if Buffer (I) /= Expected then return False; end if;
            Buffer (I) := Value;
         end loop;
         return True;
      end Check;
   begin
      if syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 0) /= 0 or else
        syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Unsigned_64'Last) /= 0 or else
        Release (aligned, PAGE_SIZE)
      then return False; end if;
      for Round in 1 .. 64 loop
         A := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Bytes - 1);
         B := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Bytes);
         if A = 0 or B = 0 or A = B or A mod PAGE_SIZE /= 0 or B mod PAGE_SIZE /= 0
         then return False; end if;
         if not Check (A, 0, 16#A5#) or else not Check (B, 0, 16#5A#) then return False; end if;
         -- Interior, wrong-size and non-owned releases must not change data.
         if Release (A + PAGE_SIZE, Bytes) or else Release (A, PAGE_SIZE) or else
           not Check (A, 16#A5#, 16#A5#) or else not Release (A, Bytes - 1) or else
           Release (A, Bytes)
         then return False; end if;
         Again := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Bytes);
         -- First-fit reuses the hole, but fresh physical contents must be zero.
         if Again /= A or else not Check (Again, 0, 16#33#) or else
           not Check (B, 16#5A#, 16#5A#) or else not Release (B, Bytes) or else
           not Release (Again, Bytes)
         then return False; end if;
      end loop;
      return True;
   end exerciseOwnedMemory;

   function exerciseOwnedGrantRetention return Boolean is
      use CuBit.Memory_Grants;
      PID : constant Process_ID := From_Word (syscall (SYSCALL_GETPID));
      Base, Replacement : Unsigned_64;
      Reference : Grant_Reference;
      Alias_Address : System.Address;
      OK : Boolean;
      type Page is array (1 .. 4096) of Unsigned_8;
      function Matches (Address : System.Address; Value : Unsigned_8) return Boolean is
         Data : Page with Import, Volatile, Address => Address;
      begin
         for I in Data'Range loop
            if Data (I) /= Value then return False; end if;
         end loop;
         return True;
      end Matches;
      procedure Fill (Address : System.Address; Value : Unsigned_8) is
         Data : Page with Import, Volatile, Address => Address;
      begin
         for I in Data'Range loop Data (I) := Value; end loop;
      end Fill;
   begin
      for Round in 1 .. 64 loop
         Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 4096);
         if Base = 0 then return False; end if;
         Fill (To_Address (Integer_Address (Base)), 16#A7#);
         Create_For_Process (PID, To_Address (Integer_Address (Base)), 1,
                             False, Reference, OK);
         if not OK then return False; end if;
         Acquire (Reference, PID, 0, 4096, Read_Access, Alias_Address, OK);
         if not OK or else Alias_Address = System.Null_Address then return False; end if;
         if syscall (SYSCALL_RELEASE_OWNED_MEMORY, Base, 4096) /= 0 then return False; end if;
         Replacement := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 4096);
         if Replacement /= Base or else
           not Matches (To_Address (Integer_Address (Replacement)), 0)
         then return False; end if;
         Fill (To_Address (Integer_Address (Replacement)), 16#3C#);
         -- The new allocation at the same VA must not recycle pinned backing.
         if not Matches (Alias_Address, 16#A7#) then return False; end if;
         Revoke (Reference, OK);
         if not OK or else Retirement_Confirmed (Reference) or else
           not Matches (Alias_Address, 16#A7#)
         then return False; end if;
         Return_Acquisition (Reference, OK);
         if not OK or else not Retirement_Confirmed (Reference) or else
           not Matches (To_Address (Integer_Address (Replacement)), 16#3C#) or else
           syscall (SYSCALL_RELEASE_OWNED_MEMORY, Replacement, 4096) /= 0
         then return False; end if;
      end loop;
      return True;
   end exerciseOwnedGrantRetention;

   function exerciseRejectedObjects return Boolean is
      Buffer : String (1 .. 4096)
        with Import, Address => To_Address (Integer_Address (aligned));
      Options : constant array (Positive range <>) of Open_Options :=
        [OPEN_READ_ONLY, OPEN_WRITE_ONLY, OPEN_READ_WRITE,
         OPEN_READ_WRITE or OPEN_TRUNCATE, OPEN_READ_WRITE or OPEN_CREATE];

      function Rejected (Name : String; Expected : Unsigned_32) return Boolean is
         M : Message;
      begin
         for Flags of Options loop
            Buffer (1 .. Name'Length) := Name;
            M := Open_Request (grantRef, Name'Length, Flags);
            M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
            if M.tag.label /= Expected or else M.words (0) /= 0 then
               debugPrint ("LINK-POLICY-CHECK: unexpected open result " & Name & LF);
               return False;
            end if;
         end loop;
         return True;
      end Rejected;
   begin
      return Rejected ("@nvme:0/nav-link", REPLY_WRONG_OBJECT_TYPE) and then
        Rejected ("@nvme:0/file-link", REPLY_WRONG_OBJECT_TYPE) and then
        Rejected ("@nvme:0/long-file-link", REPLY_WRONG_OBJECT_TYPE) and then
        Rejected ("@nvme:0/linked-file", REPLY_UNSUPPORTED_OBJECT) and then
        Rejected ("@nvme:0/linked-alias", REPLY_UNSUPPORTED_OBJECT);
   end exerciseRejectedObjects;

   function exerciseRAMVolume return Boolean is
      Name : constant String := "@mem:0/work/block-protocol-check.dat";
      Handle : File_Handle;
      M : Message;
      Buffer : String (1 .. 4096)
        with Import, Address => To_Address (Integer_Address (aligned));

      function Open_File (Options : Open_Options) return Boolean is
      begin
         Buffer (1 .. Name'Length) := Name;
         M := Open_Request (grantRef, Name'Length, Options);
         M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
         Handle := File_Handle (M.words (0));
         return M.tag.label = REPLY_OK;
      end Open_File;
   begin
      if not Open_File (OPEN_READ_WRITE or OPEN_CREATE or OPEN_TRUNCATE) then
         return False;
      end if;
      --  Cross a device-sector boundary and grow through a sparse prefix.
      Buffer (1 .. PAYLOAD'Length) := PAYLOAD;
      M := Write_At_Request (Handle, grantRef, PAYLOAD'Length, 509);
      M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
      if M.tag.label /= REPLY_OK or else M.words (0) /= PAYLOAD'Length then
         return False;
      end if;
      M := Flush_Request (Handle);
      M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
      if M.tag.label /= REPLY_DURABILITY_UNSUPPORTED then
         return False;
      end if;
      M := Close_Request (Handle);
      M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
      if M.tag.label /= REPLY_OK or else not Open_File (OPEN_READ_ONLY) then
         return False;
      end if;
      Buffer := [others => '?'];
      M := Read_At_Request (Handle, grantRef, 509 + PAYLOAD'Length, 0);
      M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
      if M.tag.label /= REPLY_OK or else
        M.words (0) /= 509 + PAYLOAD'Length or else
        Buffer (1 .. 509) /= [1 .. 509 => Character'Val (0)] or else
        Buffer (510 .. 509 + PAYLOAD'Length) /= PAYLOAD
      then
         return False;
      end if;
      M := Close_Request (Handle);
      M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
      if M.tag.label /= REPLY_OK or else
        not Open_File (OPEN_READ_WRITE or OPEN_TRUNCATE)
      then
         return False;
      end if;
      M := Read_Request (Handle, grantRef, 1);
      M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
      if M.tag.label /= REPLY_OK or else M.words (0) /= 0 then
         return False;
      end if;
      M := Close_Request (Handle);
      M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
      return M.tag.label = REPLY_OK;
   end exerciseRAMVolume;

   function exerciseVolumeIsolation return Boolean is
      type File_Handle_Array is array (Positive range <>) of File_Handle;
      RAM_Name : constant String := "@mem:0/work/block-protocol-check.dat";
      RAM, Disk, Alias : File_Handle;
      M : Message;
      Buffer : String (1 .. 4096)
        with Import, Address => To_Address (Integer_Address (aligned));

      function Open_File
        (Name : String; Options : Open_Options; Handle : out File_Handle)
         return Boolean
      is
      begin
         Buffer (1 .. Name'Length) := Name;
         M := Open_Request (grantRef, Name'Length, Options);
         M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
         Handle := File_Handle (M.words (0));
         return M.tag.label = REPLY_OK;
      end Open_File;

      function Write_File (Handle : File_Handle; Value : String) return Boolean is
      begin
         Buffer (1 .. Value'Length) := Value;
         M := Write_At_Request (Handle, grantRef, Value'Length, 0);
         M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
         return M.tag.label = REPLY_OK and then M.words (0) = Value'Length;
      end Write_File;

      function Read_File (Handle : File_Handle; Value : String) return Boolean is
      begin
         Buffer := [others => '?'];
         M := Read_At_Request (Handle, grantRef, 16, 0);
         M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
         return M.tag.label = REPLY_OK and then M.words (0) >= Value'Length
           and then Buffer (1 .. Value'Length) = Value;
      end Read_File;
   begin
      if not Open_File (RAM_Name, OPEN_READ_WRITE, RAM) or else
         not Open_File (PATH, OPEN_READ_WRITE, Disk) or else
         not Write_File (RAM, "RAM") or else
         not Write_File (Disk, "DISK") or else
         not Read_File (RAM, "RAM") or else
         not Read_File (Disk, "DISK") or else
         not Open_File (RAM_Name, OPEN_READ_WRITE or OPEN_TRUNCATE, Alias)
      then
         return False;
      end if;
      --  Truncating a RAM alias must update RAM's other handle, not disk state.
      M := Read_At_Request (RAM, grantRef, 1, 0);
      M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
      if M.tag.label /= REPLY_OK or else M.words (0) /= 0 or else
        not Read_File (Disk, "DISK")
      then
         return False;
      end if;
      for Handle of File_Handle_Array'[RAM, Disk, Alias] loop
         M := Close_Request (Handle);
         M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
         if M.tag.label /= REPLY_OK then
            return False;
         end if;
      end loop;
      return True;
   end exerciseVolumeIsolation;

   function exerciseGrantReferences return Boolean is
      use CuBit.Memory_Grants;
      use type CuBit.Memory_Grants.Grant_Reference;
      pid         : constant Process_ID := From_Word (syscall (SYSCALL_GETPID));
      --  Another process: the neighbouring identity.
      wrongOwner  : constant Process_ID := From_Word (To_Word (pid) xor 1);
      firstRef    : Grant_Reference;
      secondRef   : Grant_Reference;
      mapped      : System.Address;
      ok          : Boolean;
   begin
      Create_For_Process
        (grantee   => pid,
         localAddr => To_Address (Integer_Address (aligned)),
         numPages  => 1,
         readWrite => False,
         reference => firstRef,
         success   => ok);
      if not ok then
         return False;
      end if;

      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if not ok or else mapped = System.Null_Address then
         return False;
      end if;

      --  Multiple accepted uses share one mapping-owned pin but retain distinct
      --  return obligations.
      Acquire
        (firstRef, pid, 8, 8, Read_Access, mapped, ok);
      if not ok or else mapped = System.Null_Address then
         return False;
      end if;

      Acquire
        (firstRef, pid, 0, 16, Write_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Acquire
        (firstRef, pid, PAGE_SIZE - 4, 8, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Acquire
        (firstRef, wrongOwner, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Revoke (firstRef, ok);
      if not ok or else Retirement_Confirmed (firstRef) then
         return False;
      end if;

      -- Revocation is accepted while acquired, but becomes pending: no new
      -- borrower can enter and the existing mapping remains pinned.
      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Return_Acquisition (firstRef, ok);
      if not ok then
         return False;
      end if;

      --  One borrower remains, so revocation is still pending.
      if Retirement_Confirmed (firstRef) then
         return False;
      end if;
      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Return_Acquisition (firstRef, ok);
      if not ok then
         return False;
      end if;

      --  The final return completes revocation and retires this generation.
      if not Retirement_Confirmed (firstRef) or else
        Retirement_Confirmed ((slot => 0, generation => 1))
      then
         return False;
      end if;
      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Create_For_Process
        (grantee   => pid,
         localAddr => To_Address (Integer_Address (aligned)),
         numPages  => 1,
         readWrite => False,
         reference => secondRef,
         success   => ok);
      --  The table is global (KERN-003 step 2a), so another process may take
      --  the freed slot first. Either way the new reference is not the old
      --  one: a reused slot carries a later generation.
      if not ok or else secondRef = firstRef then
         return False;
      end if;

      if not Retirement_Confirmed (firstRef) or else
        Retirement_Confirmed (secondRef)
      then
         return False;
      end if;
      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Acquire
        (secondRef, pid, 0, 16, Read_Access, mapped, ok);
      if not ok then
         return False;
      end if;

      Return_Acquisition (secondRef, ok);
      if not ok then
         return False;
      end if;

      Acquire_Via_Capability
        (CAP_SLOT_SELF, secondRef, 0, 16, Read_Access, mapped, ok);
      if not ok then
         return False;
      end if;

      Return_Acquisition (secondRef, ok);
      if not ok then
         return False;
      end if;

      Revoke (secondRef, ok);
      return ok;
   end exerciseGrantReferences;

   function exerciseDoubleOverwrite return Boolean is
      Name : constant String := "@nvme:0/double-existing";
      Text : constant String := "double mapping";
      Offset : constant Unsigned_64 := 8 * 1024 * 1024;
      Buffer : String (1 .. 4096)
        with Import, Address => To_Address (Integer_Address (aligned));
      Handle : File_Handle;
      M : Message;
      function Check (Request : Message; Count : Unsigned_64) return Boolean is
      begin
         M := Request;
         M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
         return M.tag.label = REPLY_OK and then M.words (0) = Count;
      end Check;
   begin
      Buffer (Name'Range) := Name;
      M := Open_Request (grantRef, Name'Length, OPEN_READ_WRITE);
      M.tag := capCall (CAP_SLOT_FS, M, CuBit.Messages.Wait_Forever);
      if M.tag.label /= REPLY_OK then
         return False;
      end if;
      Handle := File_Handle (M.words (0));
      Buffer (Text'Range) := Text;
      if not Check (Write_At_Request (Handle, grantRef, Text'Length, Offset + 11), Text'Length)
      then return False; end if;
      Buffer := [others => '?'];
      if not Check (Read_At_Request (Handle, grantRef, 11 + Text'Length + 11, Offset),
                    11 + Text'Length + 11) or else
        Buffer (1 .. 11) /= [1 .. 11 => 'D'] or else
        Buffer (12 .. 11 + Text'Length) /= Text or else
        Buffer (12 + Text'Length .. 22 + Text'Length) /=
          [12 + Text'Length .. 22 + Text'Length => 'D'] or else
        not Check (Seek_Request (Handle, 0, From_End), Offset + 8192) or else
        not Check (Flush_Request (Handle), 0) or else
        not Check (Close_Request (Handle), 0)
      then return False; end if;
      debugPrint ("FILE-DOUBLE-OVERWRITE-CHECK: PASS" & LF);
      return True;
   end exerciseDoubleOverwrite;

   function exerciseExclusive return Boolean is
      Name : constant String := "@nvme:0/cubit-exclusive.dat";
      Renamed : constant String := "@nvme:0/cubit-exclusive-moved.dat";
      Text : constant String := "exclusive contents";
      Buffer : String (1 .. Natural (PAGE_SIZE))
        with Import, Address => To_Address (Integer_Address (aligned));
      A, B, Stale : File_Handle;
      Response : Message;
      function Check (Request : Message; Label : Unsigned_32 := REPLY_OK;
                      Count : Unsigned_64 := Unsigned_64'Last) return Boolean is
      begin
         Response := Request;
         Response.tag := capCall (CAP_SLOT_FS, Response, CuBit.Messages.Wait_Forever);
         if Response.tag.label /= Label or else
           (Count /= Unsigned_64'Last and then Response.words (0) /= Count)
         then
            debugPrint ("FILE-EXCLUSIVE-CHECK: op" & Request.tag.label'Image &
              " reply" & Response.tag.label'Image & LF);
            return False;
         end if;
         return True;
      end Check;
      function Open (Options : Open_Options; Handle : out File_Handle;
                     Label : Unsigned_32 := REPLY_OK) return Boolean is
      begin
         Buffer (Name'Range) := Name;
         if not Check (Open_Request (grantRef, Name'Length, Options), Label) then
            Handle := INVALID_FILE_HANDLE;
            return False;
         end if;
         Handle := (if Label = REPLY_OK then File_Handle (Response.words (0))
                    else INVALID_FILE_HANDLE);
         return True;
      end Open;
      type Options_List is array (Positive range <>) of Open_Options;
      Conflicts : constant Options_List :=
        [OPEN_READ_ONLY, OPEN_WRITE_ONLY, OPEN_READ_WRITE,
         OPEN_READ_WRITE or OPEN_CREATE, OPEN_READ_WRITE or OPEN_TRUNCATE,
         OPEN_READ_WRITE or OPEN_DENY_SHARING];
   begin
      if not Open (OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE, A) then return False; end if;
      if not Open (OPEN_READ_WRITE or OPEN_DENY_SHARING or OPEN_TRUNCATE,
                   B, REPLY_SHARING_VIOLATION) or else
        not Check (Close_Request (A)) or else
        not Open (OPEN_READ_ONLY or OPEN_DENY_SHARING, B, REPLY_ERR) or else
        not Open (OPEN_READ_WRITE or OPEN_DENY_SHARING, A)
      then return False; end if;
      Buffer (Text'Range) := Text;
      if not Check (Write_At_Request (A, grantRef, Text'Length, 0), REPLY_OK, Text'Length)
      then return False; end if;
      for Options of Conflicts loop
         if not Open (Options, B, REPLY_SHARING_VIOLATION) then return False; end if;
      end loop;
      Buffer (1 .. Name'Length + Renamed'Length) := Name & Renamed;
      if not Check (Rename_Request (grantRef, Name'Length, Renamed'Length),
                    REPLY_SHARING_VIOLATION) or else
        not Check (Read_At_Request (A, grantRef, Text'Length, 0), REPLY_OK, Text'Length)
        or else Buffer (Text'Range) /= Text or else
        not Check (Flush_Request (A))
      then return False; end if;
      Stale := A;
      if not Check (Close_Request (A)) or else
        not Open (OPEN_READ_WRITE or OPEN_DENY_SHARING, A) or else
        not Check (Close_Request (Stale), REPLY_ERR) or else
        not Open (OPEN_READ_ONLY, B, REPLY_SHARING_VIOLATION) or else
        not Check (Close_Request (A)) or else
        not Open (OPEN_READ_ONLY, A) or else not Open (OPEN_READ_ONLY, B) or else
        not Check (Close_Request (A)) or else not Check (Close_Request (B))
      then return False; end if;
      debugPrint ("FILE-EXCLUSIVE-CHECK: PASS" & LF);
      return True;
   end exerciseExclusive;

   function exerciseCoherence return Boolean is
      Name : constant String := "@nvme:0/cubit-coherence.dat";
      Text : constant String := "shared metadata";
      Buffer : String (1 .. Natural (PAGE_SIZE))
        with Import, Address => To_Address (Integer_Address (aligned));
      A, B, C : File_Handle;
      Response : Message;
      function Check
        (Request : Message; Count : Unsigned_64 := Unsigned_64'Last;
         Label : Unsigned_32 := REPLY_OK) return Boolean is
      begin
         Response := Request;
         Response.tag := capCall (CAP_SLOT_FS, Response, CuBit.Messages.Wait_Forever);
         if Response.tag.label /= Label or else
           (Count /= Unsigned_64'Last and then Response.words (0) /= Count)
         then
            debugPrint ("FILE-COHERENCE-CHECK: op" &
              Unsigned_32'Image (Request.tag.label) & " reply" &
              Unsigned_32'Image (Response.tag.label) & " bytes" &
              Unsigned_64'Image (Response.words (0)) & LF);
            return False;
         end if;
         return True;
      end Check;
      function Open (Options : Open_Options; Handle : out File_Handle)
        return Boolean is
      begin
         Buffer (Name'Range) := Name;
         if not Check (Open_Request (grantRef, Name'Length, Options)) then
            Handle := INVALID_FILE_HANDLE;
            return False;
         end if;
         Handle := File_Handle (Response.words (0));
         return True;
      end Open;
   begin
      if not Open (OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE, A) or else
        not Open (OPEN_READ_WRITE, B)
      then return False; end if;
      Buffer (Text'Range) := Text;
      if not Check (Write_At_Request (A, grantRef, Text'Length, 0), Text'Length)
      then return False; end if;
      Buffer := [others => '?'];
      --  B was opened before A grew the file: size and block mapping must be live.
      if not Check (Read_At_Request (B, grantRef, Text'Length, 0), Text'Length)
        or else Buffer (Text'Range) /= Text
      then return False; end if;
      if not Check (Seek_Request (A, 3, From_Start), 3) or else
        not Check (Seek_Request (B, 5, From_Start), 5)
      then return False; end if;
      Buffer (Text'Range) := Text;
      if not Check (Write_At_Request (B, grantRef, Text'Length, PAGE_SIZE),
                    Text'Length) or else
        not Check (Seek_Request (A, 0, From_Current), 3) or else
        not Check (Seek_Request (B, 0, From_Current), 5)
      then return False; end if;
      Buffer := [others => '?'];
      if not Check (Read_At_Request (A, grantRef, Text'Length, PAGE_SIZE),
                    Text'Length) or else Buffer (Text'Range) /= Text or else
        not Check (Read_At_Request (A, grantRef, Text'Length, 0), Text'Length)
        or else Buffer (Text'Range) /= Text
      then return False; end if;
      --  A rejected zero-progress write must not synthesize a larger file.
      --  256 TiB lies beyond the triple-indirect extent of every block size.
      if not Check (Write_At_Request (A, grantRef, 1, 16#1_0000_0000_0000#),
                    0, REPLY_FILE_RANGE_UNSUPPORTED) or else
        not Check (Seek_Request (B, 0, From_End), PAGE_SIZE + Text'Length) or else
        not Check (Seek_Request (A, 0, From_End), PAGE_SIZE + Text'Length)
      then return False; end if;
      if not Open (OPEN_READ_ONLY, C) or else
        not Check (Write_At_Request (C, grantRef, 1, 0), 0, REPLY_ACCESS_DENIED)
        or else not Check (Resize_Request (C, 7), 0, REPLY_ACCESS_DENIED)
        or else not Check (Close_Request (C))
      then return False; end if;
      --  Resizing uses the existing write authority and shared inode identity;
      --  neither shrink nor growth may silently seek any alias's cursor.
      if not Check (Seek_Request (A, 3, From_Start), 3) or else
        not Check (Seek_Request (B, 5, From_Start), 5) or else
        not Check (Resize_Request (A, 7), 0) or else
        not Check (Seek_Request (A, 0, From_Current), 3) or else
        not Check (Seek_Request (B, 0, From_Current), 5) or else
        not Check (Read_At_Request (B, grantRef, 1, 7), 0) or else
        not Check (Resize_Request (B, 1025), 0) or else
        not Check (Seek_Request (A, 0, From_Current), 3) or else
        not Check (Seek_Request (B, 0, From_Current), 5)
      then return False; end if;
      Buffer := [others => '?'];
      if not Check (Read_At_Request (A, grantRef, 1025, 0), 1025) or else
        Buffer (1 .. 7) /= Text (1 .. 7)
      then return False; end if;
      for I in 8 .. 1025 loop
         if Buffer (I) /= Character'Val (0) then
            return False;
         end if;
      end loop;
      declare
         Malformed : Message := Resize_Request (A, 0);
      begin
         Malformed.tag.length := 1;
         if not Check (Malformed, 0, REPLY_ERR) or else
           not Check (Resize_Request (A, Unsigned_64'Last),
                      0, REPLY_FILE_RANGE_UNSUPPORTED) or else
           not Check (Seek_Request (B, 0, From_End), 1025)
         then return False; end if;
      end;
      debugPrint ("FILE-RESIZE-CHECK: PASS" & LF);
      --  Sparse double-indirect growth must be coherent through both aliases,
      --  and a removed tail must stay zero when it becomes visible again.
      declare
         Double_Offset : constant Unsigned_64 := 16#0100_0000#;
      begin
         Buffer (Text'Range) := Text;
         if not Check (Write_At_Request (A, grantRef, Text'Length, Double_Offset),
                       Text'Length) or else
           not Check (Read_At_Request (B, grantRef, Text'Length, Double_Offset),
                       Text'Length) or else Buffer (Text'Range) /= Text or else
           not Check (Resize_Request (B, Double_Offset + 7), 0) or else
           not Check (Seek_Request (A, 0, From_End), Double_Offset + 7) or else
           not Check (Resize_Request (A, Double_Offset + 1025), 0)
         then return False; end if;
         Buffer := [others => '?'];
         if not Check (Read_At_Request (B, grantRef, 1025, Double_Offset), 1025)
           or else Buffer (1 .. 7) /= Text (1 .. 7)
         then return False; end if;
         for I in 8 .. 1025 loop
            if Buffer (I) /= Character'Val (0) then return False; end if;
         end loop;
         if not Check (Flush_Request (A), 0) then return False; end if;
         debugPrint ("FILE-DOUBLE-RESIZE-CHECK: PASS" & LF);
      end;
      --  4.25 GiB is triple-indirect on 1, 2 and 4 KiB volumes (it needs
      --  LARGE_FILE). Sparse growth there must be coherent through both
      --  aliases, a removed tail must stay zero, and shrinking back below the
      --  triple extent must retire the whole triple tree.
      declare
         Triple_Offset : constant Unsigned_64 := 16#1_1000_0000#;
         Previous_Size : constant Unsigned_64 := 16#0100_0000# + 1025;
      begin
         Buffer (Text'Range) := Text;
         if not Check (Write_At_Request (A, grantRef, Text'Length, Triple_Offset),
                       Text'Length) or else
           not Check (Read_At_Request (B, grantRef, Text'Length, Triple_Offset),
                       Text'Length) or else Buffer (Text'Range) /= Text or else
           not Check (Resize_Request (B, Triple_Offset + 7), 0) or else
           not Check (Seek_Request (A, 0, From_End), Triple_Offset + 7) or else
           not Check (Resize_Request (A, Triple_Offset + 1025), 0)
         then return False; end if;
         Buffer := [others => '?'];
         if not Check (Read_At_Request (B, grantRef, 1025, Triple_Offset), 1025)
           or else Buffer (1 .. 7) /= Text (1 .. 7)
         then return False; end if;
         for I in 8 .. 1025 loop
            if Buffer (I) /= Character'Val (0) then return False; end if;
         end loop;
         if not Check (Resize_Request (B, Previous_Size), 0) or else
           not Check (Seek_Request (A, 0, From_End), Previous_Size) or else
           not Check (Read_At_Request (A, grantRef, 1, Triple_Offset), 0) or else
           not Check (Flush_Request (A), 0)
         then return False; end if;
         debugPrint ("FILE-TRIPLE-RESIZE-CHECK: PASS" & LF);
      end;
      --  Truncation through a third handle invalidates every old block mapping.
      if not Open (OPEN_READ_WRITE or OPEN_TRUNCATE, C) or else
        not Check (Read_At_Request (A, grantRef, 1, 0), 0) or else
        not Check (Seek_Request (B, 0, From_End), 0) or else
        not Check (Close_Request (A))
      then return False; end if;
      Buffer (Text'Range) := Text;
      if not Check (Write_At_Request (B, grantRef, Text'Length, 0), Text'Length)
      then return False; end if;
      Buffer := [others => '?'];
      if not Check (Read_At_Request (C, grantRef, Text'Length, 0), Text'Length)
        or else Buffer (Text'Range) /= Text or else
        not Check (Close_Request (B)) or else not Check (Close_Request (C))
      then return False; end if;
      if not Open (OPEN_READ_ONLY, A) or else
        not Check (Read_At_Request (A, grantRef, Text'Length, 0), Text'Length)
        or else Buffer (Text'Range) /= Text or else not Check (Close_Request (A))
      then return False; end if;
      debugPrint ("FILE-COHERENCE-CHECK: PASS" & LF);
      return True;
   end exerciseCoherence;

   function exercisePositioned return Boolean is
      Test_Path : constant String := "@nvme:0/cubit-positioned.dat";
      Second : constant String := "Independent offset";
      Buffer : String (1 .. Natural (PAGE_SIZE))
        with Import, Address => To_Address (Integer_Address (aligned));
      Handle : File_Handle;
      Response : Message;
      Bad : Message;
      Stale : CuBit.Memory_Grants.Grant_Reference := grantRef;

      function Check
        (Request : Message; Label : Unsigned_32 := REPLY_OK;
         Count : Unsigned_64 := Unsigned_64'Last) return Boolean
      is
      begin
         Response := Request;
         Response.tag := capCall (CAP_SLOT_FS, Response, CuBit.Messages.Wait_Forever);
         if Response.tag.label /= Label or else
           (Count /= Unsigned_64'Last and then Response.words (0) /= Count)
         then
            debugPrint ("POSITIONED-IO-CHECK: request" &
              Unsigned_32'Image (Request.tag.label) & " reply" &
              Unsigned_32'Image (Response.tag.label) & " count" &
              Unsigned_64'Image (Response.words (0)) & LF);
            return False;
         end if;
         return True;
      end Check;

      function Cursor_Unchanged return Boolean is
        (Check (Seek_Request (Handle, 0, From_Current), REPLY_OK, 3));
   begin
      Buffer (Test_Path'Range) := Test_Path;
      if not Check (Open_Request
        (grantRef, Test_Path'Length,
         OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE))
      then return False; end if;
      Handle := File_Handle (Response.words (0));
      Buffer (PAYLOAD'Range) := PAYLOAD;
      if not Check (Write_Request (Handle, grantRef, PAYLOAD'Length),
                    REPLY_OK, PAYLOAD'Length) or else
        not Check (Seek_Request (Handle, 3, From_Start), REPLY_OK, 3)
      then return False; end if;

      Buffer (Second'Range) := Second;
      if not Check (Write_At_Request (Handle, grantRef, Second'Length, 64),
                    REPLY_OK, Second'Length) or else not Cursor_Unchanged
      then return False; end if;
      Buffer := [others => '?'];
      if not Check (Read_At_Request (Handle, grantRef, Second'Length, 64),
                    REPLY_OK, Second'Length) or else
        Buffer (Second'Range) /= Second or else not Cursor_Unchanged
      then return False; end if;

      --  Read the last two bytes followed by EOF: a short successful prefix,
      --  untouched buffer tail, and no modification of the seek cursor.
      Buffer := [others => '?'];
      if not Check (Read_At_Request
        (Handle, grantRef, 10, 64 + Second'Length - 2), REPLY_OK, 2) or else
        Buffer (1 .. 2) /= Second (Second'Last - 1 .. Second'Last) or else
        Buffer (3 .. 10) /= "????????" or else not Cursor_Unchanged or else
        not Check (Read_At_Request
          (Handle, grantRef, 1, 64 + Second'Length), REPLY_OK, 0)
      then return False; end if;

      --  Repeated acquisitions must be returned; payload contains no header.
      for I in 1 .. 128 loop
         Buffer := [others => '?'];
         if not Check (Read_At_Request (Handle, grantRef, PAYLOAD'Length, 0),
                       REPLY_OK, PAYLOAD'Length) or else
           Buffer (PAYLOAD'Range) /= PAYLOAD
         then return False; end if;
      end loop;
      if not Cursor_Unchanged or else
        not Check (Read_Request (Handle, grantRef, 3), REPLY_OK, 3) or else
        Buffer (1 .. 3) /= PAYLOAD (4 .. 6) or else
        not Check (Seek_Request (Handle, 3, From_Start), REPLY_OK, 3)
      then return False; end if;

      Stale.generation := (if Stale.generation = 1 then 2
                           else Stale.generation - 1);
      if not Check (Read_At_Request (Handle, Stale, 1, 0),
                    REPLY_ACCESS_DENIED, 0) or else
        not Check (Write_At_Request (Handle, Stale, 1, 0),
                   REPLY_ACCESS_DENIED, 0) or else
        not Check (Read_At_Request (Handle, grantRef, PAGE_SIZE + 1, 0),
                   REPLY_ACCESS_DENIED, 0) or else
        not Check (Write_At_Request (Handle, grantRef, PAGE_SIZE + 1, 0),
                   REPLY_ACCESS_DENIED, 0)
      then return False; end if;
      if not Check (Read_At_Request
          (Handle, grantRef, 1, Unsigned_64'Last), REPLY_OUT_OF_RANGE, 0) or else
        not Check (Write_At_Request
          (Handle, grantRef, 1, Unsigned_64'Last), REPLY_OUT_OF_RANGE, 0) or else
        not Check (Read_At_Request
          (Handle, grantRef, 0, Unsigned_64'Last), REPLY_OK, 0) or else
        not Check (Write_At_Request
          (Handle, grantRef, 0, Unsigned_64'Last), REPLY_OK, 0)
      then return False; end if;

      Bad := Read_At_Request (Handle, grantRef, 1, 0);
      Bad.words (1) := 0; -- generation zero
      if not Check (Bad, REPLY_ERR, 0) then return False; end if;
      Bad.words (1) := CuBit.Grant_References.Wire_Field_Base +
        CuBit.Grant_References.Maximum_Slot + 1;
      if not Check (Bad, REPLY_ERR, 0) then return False; end if;
      Bad := Write_At_Request (Handle, grantRef, 1, 0);
      Bad.tag.length := 3;
      if not Check (Bad, REPLY_ERR, 0) then return False; end if;
      Bad.tag.length := 4;
      Bad.tag.flags := 1;
      if not Check (Bad, REPLY_ERR, 0) then return False; end if;
      Bad.tag.flags := 0;
      Bad.tag.reserved := 1;
      if not Check (Bad, REPLY_ERR, 0) or else not Cursor_Unchanged
      then return False; end if;

      declare
         Read_Only_Loan : CuBit.Memory_Grants.Grant_Reference;
         Granted, Released : Boolean;
         Correct : Boolean;
      begin
         CuBit.Memory_Grants.Create_Via_Capability
           (CAP_SLOT_FS, To_Address (Integer_Address (aligned)), 1, False,
            Read_Only_Loan, Granted);
         if not Granted then return False; end if;
         Buffer (PAYLOAD'Range) := PAYLOAD;
         --  Reading a file writes the destination grant; writing a file only
         --  reads the source grant. The new wire encoding must preserve this.
         Correct := Check (Read_At_Request (Handle, Read_Only_Loan, 1, 0),
                           REPLY_ACCESS_DENIED, 0) and then
           Check (Write_At_Request (Handle, Read_Only_Loan, PAYLOAD'Length, 0),
                  REPLY_OK, PAYLOAD'Length);
         CuBit.Memory_Grants.Revoke (Read_Only_Loan, Released);
         if not Correct or else not Released then return False; end if;
      end;

      declare
         Completion : aliased CompletionEntry := NULL_COMPLETION;
         Token : constant Unsigned_64 := 16#504F_5349#;
      begin
         Buffer := [others => '?'];
         if not capSubmit
           (CAP_SLOT_FS,
            Read_At_Request (Handle, grantRef, Second'Length, 64), Token)
         then return False; end if;
         --  Do not touch the accepted request's buffer until completion.
         if waitCompletion (Completion'Address, 1, 1) /= 1 or else
           Completion.status /= COMPLETION_OK or else Completion.token /= Token or else
           Completion.msg.tag.label /= REPLY_OK or else
           Completion.msg.words (0) /= Second'Length or else
           Buffer (Second'Range) /= Second or else not Cursor_Unchanged
         then return False; end if;
      end;

      --  Rejected writes must not have changed the original data.
      if not Check (Read_At_Request (Handle, grantRef, PAYLOAD'Length, 0),
                    REPLY_OK, PAYLOAD'Length) or else
        Buffer (PAYLOAD'Range) /= PAYLOAD or else
        not Check (Close_Request (Handle)) or else
        not Check (Read_At_Request (Handle, grantRef, 1, 0), REPLY_ERR, 0) or else
        not Check (Write_At_Request (Handle, grantRef, 1, 0), REPLY_ERR, 0)
      then return False; end if;

      Buffer (Test_Path'Range) := Test_Path;
      if not Check (Open_Request (grantRef, Test_Path'Length, OPEN_READ_ONLY))
      then return False; end if;
      Handle := File_Handle (Response.words (0));
      if not Check (Write_At_Request (Handle, grantRef, 0, 0),
                    REPLY_ACCESS_DENIED, 0) or else
        not Check (Write_At_Request (Handle, grantRef, 1, 0),
                   REPLY_ACCESS_DENIED, 0) or else not Check (Close_Request (Handle))
      then return False; end if;
      Buffer (Test_Path'Range) := Test_Path;
      if not Check (Open_Request (grantRef, Test_Path'Length, OPEN_WRITE_ONLY))
      then return False; end if;
      Handle := File_Handle (Response.words (0));
      if not Check (Read_At_Request (Handle, grantRef, 0, 0),
                    REPLY_ACCESS_DENIED, 0) or else
        not Check (Read_At_Request (Handle, grantRef, 1, 0),
                   REPLY_ACCESS_DENIED, 0) or else not Check (Close_Request (Handle))
      then return False; end if;
      debugPrint ("POSITIONED-IO-CHECK: PASS" & LF);
      return True;
   end exercisePositioned;

   function exerciseStorage return Boolean is
      msg       : Message := NULL_MESSAGE;
      handle    : File_Handle;
      staleTag  : MessageTag;
      staleGeneration : constant Unsigned_64 :=
        (if grantRef.generation > 1 then grantRef.generation - 1
         else grantRef.generation + 1);
      staleRef : constant CuBit.Memory_Grants.Grant_Reference :=
        (slot       => grantRef.slot,
         generation =>
           CuBit.Memory_Grants.Grant_Generation (staleGeneration));

      function directoryContainsExpectedFiles return Boolean is
         directoryPath : constant String := "@nvme:0/";
         directory : Directory_Handle;
         foundConfig : Boolean := False;
         foundCreated : Boolean := False;

         function entryEquals
           (name : CuBit.Directory_Pages.Name_Bytes; length : Natural;
            expected : String) return Boolean
         is
         begin
            if length /= expected'Length then
               return False;
            end if;
            for index in 1 .. expected'Length loop
               if Character'Val (name (index)) /=
                 expected (expected'First + index - 1)
               then
                  return False;
               end if;
            end loop;
            return True;
         end entryEquals;
      begin
         declare
            pathBuffer : String (directoryPath'Range)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            pathBuffer := directoryPath;
         end;

         msg := Open_Request (grantRef, directoryPath'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_WRONG_OBJECT_TYPE then
            debugPrint
              ("STORAGE-CHECK: directory accepted as file handle" & LF);
            return False;
         end if;

         msg := Open_Directory_Request (grantRef, directoryPath'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK then
            debugPrint ("STORAGE-CHECK: directory open failed" & LF);
            return False;
         end if;
         directory := Directory_Handle (msg.words (0));

         loop
            msg := Read_Directory_Page_Request (directory, grantRef);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= REPLY_OK then
               debugPrint ("STORAGE-CHECK: directory page failed" & LF);
               return False;
            end if;

            declare
               package DP renames CuBit.Directory_Pages;
               shared : constant DP.Page
                 with Import, Address => To_Address (Integer_Address (aligned));
               page : constant DP.Page := shared;   --  copied, then checked
               valid, ended, ok : Boolean;
               count : DP.Entry_Count;
               used : DP.Used_Bytes;
               resume, stamp : Unsigned_64;
               atEntry : Natural := DP.Header_Bytes;
               nextEntry : Natural;
               item : DP.Facts;
               name : DP.Name_Bytes;
               length : DP.Name_Length;
            begin
               DP.Check (page, valid, count, used, ended, resume, stamp);
               if not valid then
                  debugPrint ("STORAGE-CHECK: invalid directory page" & LF);
                  return False;
               end if;
               for index in 1 .. count loop
                  DP.Get (page, atEntry, used, item, name, length, nextEntry, ok);
                  if not ok then
                     return False;
                  end if;
                  atEntry := nextEntry;
                  --  Listings carry metadata: the fixture's size is known.
                  if entryEquals (name, length, "config.dat") then
                     foundConfig := (item.Valid and INSPECTED_SIZE) /= 0 and then
                       item.Kind = DIRECTORY_KIND_FILE;
                  end if;
                  foundCreated := foundCreated or else
                    entryEquals (name, length, "cubit-new.dat");
               end loop;
               exit when ended;
            end;
         end loop;

         msg := Close_Directory_Request (directory);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         return msg.tag.label = REPLY_OK and foundConfig and foundCreated;
      end directoryContainsExpectedFiles;

      function exerciseDirectoryNavigation return Boolean is
         root, child, reopened : Directory_Handle;
         firstResume : Unsigned_64;
         --  The page's resume token (CuBit.Directory_Pages), read in place:
         --  compared only, never used to index anything.
         resumeToken : Unsigned_64
           with Import, Volatile, Address => To_Address
             (Integer_Address (aligned + CuBit.Directory_Pages.Resume_At));

         procedure Put_Name (name : String) is
            view : String (name'Range)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            view := name;
         end Put_Name;

         function Child_Request
           (parent : Directory_Handle; name : String) return Message
         is
         begin
            Put_Name (name);
            return Open_Child_Directory_Request
              (parent, grantRef, name'Length);
         end Child_Request;

         function Reject_Name (name : String) return Boolean is
         begin
            msg := Child_Request (root, name);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            return msg.tag.label /= REPLY_OK;
         end Reject_Name;
      begin
         Put_Name ("@nvme:0/");
         msg := Open_Directory_Request (grantRef, 8);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         root := Directory_Handle (msg.words (0));
         msg := Read_Directory_Page_Request (root, grantRef);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         firstResume := resumeToken;
         msg := Rewind_Directory_Request (root);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         msg := Read_Directory_Page_Request (root, grantRef);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK or else resumeToken /= firstResume then
            return False;
         end if;

         if not Reject_Name (".") or else not Reject_Name ("..") or else
           not Reject_Name ("../config.dat") or else
           not Reject_Name ("lost+found/child") or else
           not Reject_Name ("@mem:0/") or else
           not Reject_Name ("lost+found" & Character'Val (0)) or else
           not Reject_Name ("config.dat") or else
           not Reject_Name ("nav-link")
         then
            debugPrint ("DIRECTORY-NAVIGATION-CHECK: name escape accepted" & LF);
            return False;
         end if;
         msg := Child_Request (root, "lost+found");
         msg.words (1) := 0;
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_ERR then
            return False;
         end if;
         msg := Child_Request (root, "lost+found");
         msg.words (1) := 256;
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_ERR then
            return False;
         end if;
         msg := Child_Request (root, "lost+found");
         msg.words (3) := staleGeneration;
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_ACCESS_DENIED then
            return False;
         end if;
         msg := Read_Request (File_Handle (root), grantRef, 1);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label = REPLY_OK then
            return False;
         end if;

         --  Repeated visits must release slots and reject stale generations.
         for visit in 1 .. 64 loop
            msg := Child_Request (root, "lost+found");
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            child := Directory_Handle (msg.words (0));
            msg := Read_Directory_Page_Request (child, grantRef);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            msg := Close_Directory_Request (child);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            msg := Child_Request (root, "lost+found");
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            reopened := Directory_Handle (msg.words (0));
            if child = reopened then
               return False;
            end if;
            msg := Rewind_Directory_Request (child);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= REPLY_WRONG_OBJECT_TYPE then
               return False;
            end if;
            msg := Child_Request (child, "lost+found");
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= REPLY_WRONG_OBJECT_TYPE then
               return False;
            end if;
            msg := Close_Directory_Request (reopened);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
         end loop;
         msg := Close_Directory_Request (root);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         debugPrint ("DIRECTORY-NAVIGATION-CHECK: PASS" & LF);
         return True;
      end exerciseDirectoryNavigation;

      function rejectsMalformedDirectory return Boolean is
         corruptPath : constant String := "@nvme:0/corrupt-dir";
         directory : Directory_Handle;
         closeMessage : Message;
      begin
         declare
            pathBuffer : String (corruptPath'Range)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            pathBuffer := corruptPath;
         end;

         msg := Open_Directory_Request (grantRef, corruptPath'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK then
            --  The adversarial fixture is installed only by the focused
            --  headless test. Its absence is not a storage failure.
            return True;
         end if;
         directory := Directory_Handle (msg.words (0));

         declare
            nameBuffer : String (1 .. 5)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            nameBuffer := "child";
         end;
         msg := Open_Child_Directory_Request (directory, grantRef, 5);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_MALFORMED_FILESYSTEM then
            debugPrint ("STORAGE-CHECK: malformed child lookup misreported" & LF);
            return False;
         end if;

         msg := Read_Directory_Page_Request (directory, grantRef);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         closeMessage := Close_Directory_Request (directory);
         closeMessage.tag := capCall (CAP_SLOT_FS, closeMessage, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_MALFORMED_FILESYSTEM or else
           closeMessage.tag.label /= REPLY_OK
         then
            debugPrint
              ("STORAGE-CHECK: malformed directory was not rejected" & LF);
            return False;
         end if;
         declare
            procedure Check_Metadata_Rename
              (parent : String; expected : Unsigned_32) is
               oldPath : constant String := parent & "/old";
               newPath : constant String := parent & "/new";
               view : String (1 .. oldPath'Length + newPath'Length)
                 with Import, Address => To_Address (Integer_Address (aligned));
            begin
               view := oldPath & newPath;
               msg := Rename_Request (grantRef, oldPath'Length, newPath'Length);
               msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
               if msg.tag.label /= expected then
                  debugPrint ("STORAGE-CHECK: metadata rename " & parent &
                    " expected" & expected'Image & " got" & msg.tag.label'Image & LF);
               end if;
            end Check_Metadata_Rename;
         begin
            Check_Metadata_Rename (corruptPath, REPLY_MALFORMED_FILESYSTEM);
            if msg.tag.label /= REPLY_MALFORMED_FILESYSTEM then
               return False;
            end if;
            --  htree: admitted (no "old" in it, so not found).
            Check_Metadata_Rename ("@nvme:0/indexed-dir", REPLY_NOT_FOUND);
            if msg.tag.label /= REPLY_NOT_FOUND then
               return False;
            end if;
            --  Extents: a layout this service does not write.
            Check_Metadata_Rename
              ("@nvme:0/extents-dir", REPLY_FILE_RANGE_UNSUPPORTED);
            if msg.tag.label /= REPLY_FILE_RANGE_UNSUPPORTED then
               return False;
            end if;
         end;
         debugPrint ("MALFORMED-DIRECTORY-CHECK: PASS" & LF);
         return True;
      end rejectsMalformedDirectory;

      function exerciseRename return Boolean is
         --  The runner adds fixtures to the shared root directory. Its
         --  source block may be completely full: keep this rename the same
         --  size, and exercise name growth in lost+found below instead.
         renamed : constant String := "@nvme:0/cubit-alt.dat";
         nested : constant String := "@nvme:0/lost+found/rename-before.dat";
         nestedAfter : constant String := "@nvme:0/lost+found/rename-after-much-longer.dat";
         movedAcross : constant String := "@nvme:0/lost+found/moved-across.dat";

         procedure Put (value : String) is
            view : String (value'Range)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            view := value;
         end Put;

         function Rename_Is
           (before, after : String; expected : Unsigned_32) return Boolean is
         begin
            Put (before & after);
            msg := Rename_Request (grantRef, before'Length, after'Length);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= expected then
               debugPrint ("RENAME-CHECK: unexpected reply for " & before &
                 " -> " & after & ":" & Unsigned_32'Image (msg.tag.label) & LF);
               return False;
            end if;
            return True;
         end Rename_Is;

         function Has_Payload (name : String) return Boolean is
            file : File_Handle;
            good : Boolean;
            bytes : String (PAYLOAD'Range)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            Put (name);
            msg := Open_Request (grantRef, name'Length);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            file := File_Handle (msg.words (0));
            msg := Read_Request (file, grantRef, PAYLOAD'Length);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            good := msg.tag.label = REPLY_OK and then
              msg.words (0) = PAYLOAD'Length and then bytes = PAYLOAD;
            msg := Close_Request (file);
            msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
            return good and then msg.tag.label = REPLY_OK;
         end Has_Payload;
      begin
         Put (CREATE_PATH);
         msg := Open_Request
           (grantRef, CREATE_PATH'Length, OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_ALREADY_EXISTS or else not Has_Payload (CREATE_PATH) then
            debugPrint ("STORAGE-CHECK: exclusive create reused existing file" & LF);
            return False;
         end if;
         --  (Renaming onto an existing name replaces it, as POSIX requires;
         --  the hosted tests check that, without disturbing these fixtures.)
         if not Has_Payload (CREATE_PATH) or else not Has_Payload (PATH) or else
           not Rename_Is (CREATE_PATH, CREATE_PATH, REPLY_OK) or else
           not Rename_Is (CREATE_PATH, renamed, REPLY_OK) or else
           not Has_Payload (renamed) or else
           not Rename_Is (CREATE_PATH, renamed, REPLY_NOT_FOUND) or else
           --  Across directories, and back.
           not Rename_Is (renamed, movedAcross, REPLY_OK) or else
           not Has_Payload (movedAcross) or else
           not Rename_Is (movedAcross, renamed, REPLY_OK) or else
           not Has_Payload (renamed) or else
           not Rename_Is
             (renamed, "@nvme:1/elsewhere.dat", REPLY_ACCESS_DENIED) or else
           not Rename_Is (renamed, "@nvme:0/..", REPLY_ERR) or else
           not Has_Payload (renamed) or else
           not Rename_Is (renamed, CREATE_PATH, REPLY_OK)
         then
            return False;
         end if;

         --  Existing handles refer to the inode, not its former name. Also
         --  exercise parent resolution: the original path-only shim treated
         --  the entire relative path as a root directory entry name.
         Put (nested);
         msg := Open_Request
           (grantRef, nested'Length, OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         handle := File_Handle (msg.words (0));
         Put (PAYLOAD);
         msg := Write_Request (handle, grantRef, PAYLOAD'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK or else
           not Rename_Is (nested, nestedAfter, REPLY_OK)
         then
            return False;
         end if;
         msg := Seek_Request (handle, 0, From_Start);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         msg := Read_Request (handle, grantRef, PAYLOAD'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         declare
            bytes : String (PAYLOAD'Range)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            if msg.tag.label /= REPLY_OK or else bytes /= PAYLOAD then
               return False;
            end if;
         end;
         msg := Close_Request (handle);
         msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
         if msg.tag.label /= REPLY_OK or else not Has_Payload (nestedAfter) then
            return False;
         end if;
         debugPrint ("RENAME-CHECK: PASS" & LF);
         return True;
      end exerciseRename;
   begin
      --  Possessing a normal filesystem endpoint does not convey policy
      --  administration. In particular, an application cannot grant itself
      --  a wildcard profile by calling the management operation directly.
      msg := NULL_MESSAGE;
      msg.tag := (label => OP_SET_ACL, length => 4,
                  flags => 0, reserved => 0);
      msg.words :=
        (0 => syscall (SYSCALL_GETPID), 1 => 0, 2 => 0, 3 => 0);
      staleTag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if staleTag.label /= REPLY_ACCESS_DENIED then
         debugPrint ("STORAGE-CHECK: ordinary endpoint changed FS policy" & LF);
         return False;
      end if;

      declare
         pathBuffer : String (PATH'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         pathBuffer := PATH;
      end;

      --  The filesystem must independently reject a stale, nonzero
      --  generation even when the slot and path bytes are otherwise valid.
      msg := Open_Request (staleRef, PATH'Length);
      staleTag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: stale grant accepted by FS" & LF);
         return False;
      end if;

      --  Unknown flags must not silently alter the authority installed on a
      --  returned handle.
      msg := Open_Request (grantRef, PATH'Length, Open_Options (4));
      staleTag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: invalid open options accepted" & LF);
         return False;
      end if;

      --  Exercise the distinct read-write mode against a sparse scratch file.
      --  Its first write must allocate outside a full block group in the
      --  headless fixture.
      msg := Open_Request
        (grantRef, PATH'Length, OPEN_READ_WRITE);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: read-write open failed" & LF);
         return False;
      end if;
      handle := File_Handle (msg.words (0));

      declare
         payloadBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         payloadBuffer := PAYLOAD;
      end;

      msg := Write_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint
           ("STORAGE-CHECK: write failed label=" &
            Unsigned_32'Image (msg.tag.label) & " count=" &
            Unsigned_64'Image (msg.words (0)) & LF);
         return False;
      end if;

      --  A flush must acknowledge the real NVMe barrier, not close() or RAM.
      msg := Flush_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK or else msg.tag.length /= 1 or else
        msg.words (0) /= 0
      then
         debugPrint ("STORAGE-FLUSH-CHECK: write flush failed" & LF);
         return False;
      end if;
      msg := Flush_Request (handle);
      msg.tag.length := 0;
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_ERR then
         debugPrint ("STORAGE-FLUSH-CHECK: malformed request accepted" & LF);
         return False;
      end if;

      --  256 TiB lies beyond the triple-indirect extent of every block size.
      --  A range the writer cannot represent must be reported as such,
      --  never as a successful zero-byte write.
      msg := Seek_Request (handle, 16#1_0000_0000_0000#, From_Start);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: large seek failed" & LF);
         return False;
      end if;

      msg := Write_Request (handle, grantRef, 1);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_FILE_RANGE_UNSUPPORTED or else
         msg.words (0) /= 0
      then
         debugPrint
           ("STORAGE-CHECK: unsupported write range was not explicit" & LF);
         return False;
      end if;

      msg := Seek_Request (handle, 0, From_Start);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: read-write seek failed" & LF);
         return False;
      end if;

      msg := Read_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint ("STORAGE-CHECK: read-write handle lost read access" & LF);
         return False;
      end if;

      declare
         readWriteBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         if readWriteBuffer /= PAYLOAD then
            debugPrint ("STORAGE-CHECK: read-write content mismatch" & LF);
            return False;
         end if;
      end;

      msg := Close_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: close failed" & LF);
         return False;
      end if;

      msg := Flush_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_ERR then
         debugPrint ("STORAGE-FLUSH-CHECK: stale handle accepted" & LF);
         return False;
      end if;

      --  The just-closed handle must remain invalid even though the next open
      --  is likely to reuse the same table slot.
      msg := Read_Request (handle, grantRef, 1);
      staleTag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: stale handle accepted" & LF);
         return False;
      end if;

      declare
         pathBuffer : String (PATH'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         pathBuffer := PATH;
      end;

      msg := Open_Request (grantRef, PATH'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: reopen failed" & LF);
         return False;
      end if;
      handle := File_Handle (msg.words (0));

      --  A read-only handle may flush (fsync of an O_RDONLY descriptor, as
      --  Linux): a flush writes back and commits, it changes no data.
      msg := Flush_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-FLUSH-CHECK: read-only handle refused" & LF);
         return False;
      end if;
      debugPrint ("STORAGE-FLUSH-CHECK: PASS" & LF);

      --  One page was granted.  The service must not trust the protocol's
      --  requested count as if the entire 16 MiB aperture slot were mapped.
      msg := Read_Request (handle, grantRef, PAGE_SIZE + 1);
      staleTag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: oversized grant range accepted" & LF);
         return False;
      end if;

      msg := Read_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint ("STORAGE-CHECK: read failed" & LF);
         return False;
      end if;

      declare
         readBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         if readBuffer /= PAYLOAD then
            debugPrint ("STORAGE-CHECK: content mismatch" & LF);
            return False;
         end if;
      end;

      msg := Close_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         return False;
      end if;

      --  Exercise inode allocation and directory insertion independently of
      --  the pre-created sparse fixture.
      declare
         pathBuffer : String (CREATE_PATH'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         pathBuffer := CREATE_PATH;
      end;

      msg := Open_Request
        (grantRef, CREATE_PATH'Length,
         OPEN_READ_WRITE or OPEN_CREATE);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: create failed" & LF);
         return False;
      end if;
      handle := File_Handle (msg.words (0));

      declare
         payloadBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         payloadBuffer := PAYLOAD;
      end;

      msg := Write_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint ("STORAGE-CHECK: created-file write failed" & LF);
         return False;
      end if;

      msg := Seek_Request (handle, 0, From_Start);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         return False;
      end if;

      msg := Read_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint ("STORAGE-CHECK: created-file read failed" & LF);
         return False;
      end if;

      declare
         readBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         if readBuffer /= PAYLOAD then
            debugPrint ("STORAGE-CHECK: created-file mismatch" & LF);
            return False;
         end if;
      end;

      msg := Close_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label /= REPLY_OK then
         return False;
      end if;

      return directoryContainsExpectedFiles and then
        exerciseDirectoryNavigation and then rejectsMalformedDirectory and then
        exerciseRename;
   end exerciseStorage;

   --  The request queue's wake and queued rename
   --  (docs/filesystem-protocol-v2.md steps 1 and 2), through
   --  CuBit.Filesystem_Sessions.
   function exerciseQueue return Boolean is
      package FQ renames CuBit.Filesystem_Queues;
      package FS renames CuBit.Filesystem_Sessions;
      use type FS.Token;
      use type FS.Wait_Result;
      Session : FS.Session;
      Opened : Boolean;
      --  Process-wide, increasing completion tokens for the wake.
      Next_Wake_Token : Unsigned_64 := 16#F5_0000#;
      SHORT_MS : constant := 50;
      PROMPT_MS : constant := 2_000;

      procedure Put (Value : String) is
         View : String (Value'Range) with Import, Address => FS.Arena (Session);
      begin
         View := Value;
      end Put;

      --  One request, waited for (blocking form, explicit deadline).
      function Call (Item : FQ.Request; Value : out Unsigned_64) return Unsigned_32 is
         Tag : FS.Token;
         Answer : FQ.Queues.Completion;
         Got : Boolean;
         Waited : FS.Wait_Result;
      begin
         Value := 0;
         FS.Submit (Session, Item, Tag);
         if Tag = 0 then
            return REPLY_ERR;
         end if;
         FS.Wait_Answer (Session, Deadline_After (PROMPT_MS), Waited);
         if Waited /= FS.Answer_Waiting then
            return REPLY_ERR;
         end if;
         FS.Reap (Session, Answer, Got);
         if not Got or else Answer.Tag /= Tag then
            return REPLY_ERR;
         end if;
         Value := Answer.Answer.Value;
         return Answer.Answer.Status;
      end Call;

      function Path_Call (Operation : Unsigned_32; Path : String; Options : Unsigned_32 := 0;
                          Handle : Unsigned_64 := 0) return Unsigned_32 is
         Ignore : Unsigned_64;
      begin
         Put (Path);
         return Call ((Operation => Operation, Options => Options, Handle => Handle,
                       Length => Path'Length, others => <>), Ignore);
      end Path_Call;

      function Arm return Boolean is
         Accepted : Boolean;
      begin
         Next_Wake_Token := Next_Wake_Token + 1;
         FS.Arm_Wake (Session, Next_Wake_Token, Accepted);
         return Accepted;
      end Arm;

      --  Wait in "the event loop" until the wake completes or Milliseconds
      --  pass; Woken says how it completed.
      function Wake_Completes (Milliseconds : Unsigned_64; Woken : out Boolean) return Boolean is
         Deadline : constant Unsigned_64 := Deadline_After (Milliseconds);
         Receipt : aliased CompletionEntry;
         Consumed : Boolean;
      begin
         Woken := False;
         loop
            while Poll_Completion (Receipt'Address) /= 0 loop
               FS.Complete_Wake (Session, Receipt, Consumed, Woken);
               if Consumed then
                  return True;
               end if;
               debugPrint ("QUEUE-WAKE-CHECK: foreign completion" & LF);
            end loop;
            --  Other activity (a kernel notice) also wakes the wait: the
            --  deadline is checked here too.
            exit when Wait_For_Activity_Until (Deadline) /= Work_Available or else
              syscall (SYSCALL_GETTIME) >= Deadline;
         end loop;
         return False;
      end Wake_Completes;

      function Wake_Check return Boolean is
         Woken : Boolean;
         Tag : FS.Token;
         Answer : FQ.Queues.Completion;
         Got : Boolean;
         Directory : Unsigned_64;
         Waited : FS.Wait_Result;
      begin
         --  Nothing outstanding: the wake is held, not answered.
         if not Arm or else Wake_Completes (SHORT_MS, Woken) then
            debugPrint ("QUEUE-WAKE-CHECK: idle wake answered" & LF);
            return False;
         end if;
         --  An answer posted wakes it, without a call blocking anywhere.
         Put ("@nvme:0/");
         FS.Submit (Session, (Operation => FQ.Queue_Open_Directory, Length => 8, others => <>), Tag);
         if Tag = 0 or else not Wake_Completes (PROMPT_MS, Woken) or else not Woken then
            debugPrint ("QUEUE-WAKE-CHECK: answer did not wake" & LF);
            return False;
         end if;
         FS.Reap (Session, Answer, Got);
         if not Got or else Answer.Tag /= Tag or else Answer.Answer.Status /= REPLY_OK then
            debugPrint ("QUEUE-WAKE-CHECK: open answer" & LF);
            return False;
         end if;
         Directory := Answer.Answer.Value;
         --  Answers already waiting: a wake is answered at once.
         FS.Submit (Session, (Operation => FQ.Queue_Close_Directory, Handle => Directory, others => <>), Tag);
         FS.Wait_Answer (Session, Deadline_After (PROMPT_MS), Waited);
         if Waited /= FS.Answer_Waiting or else not Arm or else
           not Wake_Completes (PROMPT_MS, Woken) or else not Woken
         then
            debugPrint ("QUEUE-WAKE-CHECK: waiting answer not reported" & LF);
            return False;
         end if;
         FS.Reap (Session, Answer, Got);
         if not Got or else Answer.Answer.Status /= REPLY_OK then
            return False;
         end if;
         --  A blocking wait supersedes a held wake (answered), and its own
         --  timeout leaves nothing that answers the next wake early.
         if not Arm then
            return False;
         end if;
         FS.Wait_Answer (Session, Deadline_After (SHORT_MS), Waited);
         if Waited /= FS.Deadline_Reached or else not Wake_Completes (PROMPT_MS, Woken) then
            debugPrint ("QUEUE-WAKE-CHECK: supersede" & LF);
            return False;
         end if;
         if not Arm or else Wake_Completes (SHORT_MS, Woken) then
            debugPrint ("QUEUE-WAKE-CHECK: stale wait answered a new wake" & LF);
            return False;
         end if;
         Put ("@nvme:0/");
         FS.Submit (Session, (Operation => FQ.Queue_Open_Directory, Length => 8, others => <>), Tag);
         if not Wake_Completes (PROMPT_MS, Woken) or else not Woken then
            return False;
         end if;
         FS.Reap (Session, Answer, Got);
         if not Got or else Answer.Answer.Status /= REPLY_OK or else
           Path_Call (FQ.Queue_Close_Directory, "", Handle => Answer.Answer.Value) /= REPLY_OK
         then
            return False;
         end if;
         debugPrint ("QUEUE-WAKE-CHECK: PASS" & LF);
         return True;
      end Wake_Check;

      function Rename_Check return Boolean is
         A : constant String := "@nvme:0/queue-rename-a.dat";
         B : constant String := "@nvme:0/queue-rename-b.dat";
         C : constant String := "@nvme:0/lost+found/queue-rename-c.dat";
         D : constant String := "@nvme:0/queue-rename-d.dat";
         Create : constant Unsigned_32 :=
           Unsigned_32 (OPEN_READ_WRITE or OPEN_CREATE or OPEN_TRUNCATE);

         function Make (Name : String) return Boolean is
            Handle : Unsigned_64;
            Status : Unsigned_32;
         begin
            Put (Name);
            Status := Call ((Operation => FQ.Queue_Open, Options => Create,
                             Length => Name'Length, others => <>), Handle);
            return Status = REPLY_OK and then
              Path_Call (FQ.Queue_Close, "", Handle => Handle) = REPLY_OK;
         end Make;

         function Rename_Is (Before, After : String; Expected : Unsigned_32;
                             Split : Unsigned_64 := 0) return Boolean is
            Ignore : Unsigned_64;
            Status : Unsigned_32;
         begin
            Put (Before & After);
            Status := Call ((Operation => FQ.Queue_Rename,
                             Position => (if Split = 0 then Before'Length else Split),
                             Length => Before'Length + After'Length, others => <>), Ignore);
            if Status /= Expected then
               debugPrint ("QUEUE-RENAME-CHECK: " & Before & " -> " & After & " expected" &
                 Expected'Image & " got" & Status'Image & LF);
            end if;
            return Status = Expected;
         end Rename_Is;

         function Exists (Name : String) return Boolean is
            Handle : Unsigned_64;
         begin
            Put (Name);
            if Call ((Operation => FQ.Queue_Open, Length => Name'Length, others => <>), Handle) /= REPLY_OK then
               return False;
            end if;
            return Path_Call (FQ.Queue_Close, "", Handle => Handle) = REPLY_OK;
         end Exists;
      begin
         if not Make (A) or else
           not Rename_Is (A, B, REPLY_OK) or else Exists (A) or else not Exists (B) or else
           --  A move into another directory of the volume.
           not Rename_Is (B, C, REPLY_OK) or else not Exists (C) or else
           --  Onto an existing file: replaced (POSIX).
           not Make (D) or else not Rename_Is (C, D, REPLY_OK) or else Exists (C) or else
           not Exists (D) or else
           --  A split that leaves a path empty.
           not Rename_Is (D, A, REPLY_ERR, Split => D'Length + A'Length) or else
           --  Outside the grant, and across volumes.
           not Rename_Is (D, "@nvme:1/queue-rename.dat", REPLY_ACCESS_DENIED) or else
           not Rename_Is (D, "@mem:0/work/queue-rename.dat", REPLY_CROSS_VOLUME) or else
           not Exists (D) or else
           Path_Call (FQ.Queue_Unlink, D) /= REPLY_OK or else Exists (D)
         then
            return False;
         end if;
         debugPrint ("QUEUE-RENAME-CHECK: PASS" & LF);
         return True;
      end Rename_Check;

      --  Directory.Page.V2 through the queue (docs/filesystem-protocol-v2.md
      --  step 3): a folder of many names over several pages, each entry
      --  with its metadata; a resume token continued on a new handle; a
      --  token of another folder refused; and an entry removed at the
      --  listing's cursor (its record merged into the one before) neither
      --  listed nor hiding the entries after it.
      function Directory_Check return Boolean is
         package V2 renames CuBit.Directory_Pages;
         Folder : constant String := "@nvme:0/v2-dir";
         Names : constant := 120;
         PAGE_AT : constant := 4_096;   --  pages after the path in the arena
         type Seen_Set is array (1 .. Names) of Boolean;

         function Name_Of (I : Positive) return String is
            Digits_Image : constant String := Positive'Image (I);
            Number : constant String := Digits_Image (Digits_Image'First + 1 .. Digits_Image'Last);
         begin
            --  Mixed lengths, so records of several sizes share pages.
            return (if I mod 3 = 0 then "a-much-longer-entry-name-" & Number & ".dat"
                    elsif I mod 3 = 1 then "f" & Number else "mid-" & Number & ".txt");
         end Name_Of;

         function Index_Of (Name : String) return Natural is
         begin
            for I in 1 .. Names loop
               if Name_Of (I) = Name then
                  return I;
               end if;
            end loop;
            return 0;
         end Index_Of;

         function Open_Folder (Handle : out Unsigned_64) return Boolean is
         begin
            Put (Folder);
            return Call ((Operation => FQ.Queue_Open_Directory, Length => Folder'Length, others => <>),
                         Handle) = REPLY_OK;
         end Open_Folder;

         --  One page into PAGE_AT; its entries marked in Seen (a repeat or an
         --  unknown name fails). First: the page's first entry's index.
         function Read_Page
           (Handle : Unsigned_64; Seen : in out Seen_Set; Ended : out Boolean;
            Resume : out Unsigned_64; First : out Natural; Count : out Natural) return Boolean
         is
            Pages : Unsigned_64;
            Shared : constant V2.Page with Import, Address => FS.Arena (Session) + Storage_Offset (PAGE_AT);
            Copy : V2.Page;
            Valid, OK : Boolean;
            Entries : V2.Entry_Count;
            Used : V2.Used_Bytes;
            Stamp : Unsigned_64;
            At_Entry : Natural := V2.Header_Bytes;
            Next : Natural;
            Item : V2.Facts;
            Bytes : V2.Name_Bytes;
            Length : V2.Name_Length;
         begin
            Ended := False;
            Resume := 0;
            First := 0;
            Count := 0;
            if Call ((Operation => FQ.Queue_Read_Directory, Options => FQ.Directory_Metadata,
                      Handle => Handle, Length => V2.Page_Bytes, Arena_Offset => PAGE_AT, others => <>),
                     Pages) /= REPLY_OK or else Pages /= 1
            then
               return False;
            end if;
            Copy := Shared;
            V2.Check (Copy, Valid, Entries, Used, Ended, Resume, Stamp);
            if not Valid then
               debugPrint ("QUEUE-DIRECTORY-CHECK: page refused" & LF);
               return False;
            end if;
            for Ordinal in 1 .. Entries loop
               V2.Get (Copy, At_Entry, Used, Item, Bytes, Length, Next, OK);
               if not OK then
                  return False;
               end if;
               At_Entry := Next;
               declare
                  Name : String (1 .. Length);
                  Index : Natural;
               begin
                  for C in Name'Range loop
                     Name (C) := Character'Val (Bytes (C));
                  end loop;
                  Index := Index_Of (Name);
                  if Index = 0 or else Seen (Index) or else Item.Kind /= V2.Kind_File or else
                    (Item.Valid and V2.Valid_Size) = 0 or else (Item.Valid and V2.Valid_Times) = 0
                  then
                     debugPrint ("QUEUE-DIRECTORY-CHECK: bad entry " & Name & LF);
                     return False;
                  end if;
                  Seen (Index) := True;
                  if First = 0 then
                     First := Index;
                  end if;
               end;
               Count := Count + 1;
            end loop;
            return True;
         end Read_Page;

         Handle, Other, Root : Unsigned_64;
         Seen, Again : Seen_Set := [others => False];
         Ended : Boolean;
         Resume, First_Resume, Root_Resume : Unsigned_64;
         First, Count, Second_First, Pages : Natural := 0;
         Ignore : Unsigned_64;
      begin
         if Path_Call (FQ.Queue_Mkdir, Folder) /= REPLY_OK then
            debugPrint ("QUEUE-DIRECTORY-CHECK: mkdir" & LF);
            return False;
         end if;
         for I in 1 .. Names loop
            declare
               Path : constant String := Folder & "/" & Name_Of (I);
               File : Unsigned_64;
            begin
               Put (Path);
               if Call ((Operation => FQ.Queue_Open,
                         Options => Unsigned_32 (OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE),
                         Length => Path'Length, others => <>), File) /= REPLY_OK or else
                 Path_Call (FQ.Queue_Close, "", Handle => File) /= REPLY_OK
               then
                  debugPrint ("QUEUE-DIRECTORY-CHECK: create " & Path & LF);
                  return False;
               end if;
            end;
         end loop;
         --  The whole listing, a page at a time.
         if not Open_Folder (Handle) then
            return False;
         end if;
         loop
            if not Read_Page (Handle, Seen, Ended, Resume, First, Count) then
               return False;
            end if;
            Pages := Pages + 1;
            if Pages = 1 then
               First_Resume := Resume;
            elsif Pages = 2 then
               Second_First := First;
            end if;
            exit when Ended or else Pages > Names;
         end loop;
         if Seen /= [1 .. Names => True] or else Pages < 3 then
            debugPrint ("QUEUE-DIRECTORY-CHECK: listing incomplete, pages" & Pages'Image & LF);
            return False;
         end if;
         --  Resumed on a new handle: the page after the first.
         if not Open_Folder (Other) or else
           Call ((Operation => FQ.Queue_Seek_Directory, Handle => Other, Position => First_Resume,
                  others => <>), Ignore) /= REPLY_OK or else
           not Read_Page (Other, Again, Ended, Resume, First, Count) or else First /= Second_First
         then
            debugPrint ("QUEUE-DIRECTORY-CHECK: resume" & LF);
            return False;
         end if;
         --  A token of another folder.
         Put ("@nvme:0/");
         if Call ((Operation => FQ.Queue_Open_Directory, Length => 8, others => <>), Root) /= REPLY_OK then
            return False;
         end if;
         Again := [others => False];
         declare
            Pages_Read : Unsigned_64;
            Root_Page : constant V2.Page with Import, Address => FS.Arena (Session) + Storage_Offset (PAGE_AT);
            Copy : V2.Page;
            Valid : Boolean;
            Entries : V2.Entry_Count;
            Used : V2.Used_Bytes;
            Stamp : Unsigned_64;
         begin
            if Call ((Operation => FQ.Queue_Read_Directory, Handle => Root, Length => V2.Page_Bytes,
                      Arena_Offset => PAGE_AT, others => <>), Pages_Read) /= REPLY_OK
            then
               return False;
            end if;
            Copy := Root_Page;
            V2.Check (Copy, Valid, Entries, Used, Ended, Root_Resume, Stamp);
            if not Valid or else
              Call ((Operation => FQ.Queue_Seek_Directory, Handle => Other, Position => Root_Resume,
                     others => <>), Ignore) /= REPLY_OUT_OF_RANGE
            then
               debugPrint ("QUEUE-DIRECTORY-CHECK: foreign token accepted" & LF);
               return False;
            end if;
         end;
         if Path_Call (FQ.Queue_Close_Directory, "", Handle => Root) /= REPLY_OK then
            return False;
         end if;
         --  Remove the entry the cursor stands on, then go on: it is not
         --  listed, and every other name still is, once.
         Again := [others => False];
         if Call ((Operation => FQ.Queue_Seek_Directory, Handle => Other, Position => 0,
                   others => <>), Ignore) /= REPLY_OK or else
           not Read_Page (Other, Again, Ended, Resume, First, Count) or else
           Path_Call (FQ.Queue_Unlink, Folder & "/" & Name_Of (Second_First)) /= REPLY_OK
         then
            return False;
         end if;
         loop
            if not Read_Page (Other, Again, Ended, Resume, First, Count) then
               return False;
            end if;
            exit when Ended;
         end loop;
         for I in 1 .. Names loop
            if Again (I) /= (I /= Second_First) then
               debugPrint ("QUEUE-DIRECTORY-CHECK: after removal " & Name_Of (I) & LF);
               return False;
            end if;
         end loop;
         --  Clean up.
         if Path_Call (FQ.Queue_Close_Directory, "", Handle => Handle) /= REPLY_OK or else
           Path_Call (FQ.Queue_Close_Directory, "", Handle => Other) /= REPLY_OK
         then
            return False;
         end if;
         for I in 1 .. Names loop
            if I /= Second_First and then
              Path_Call (FQ.Queue_Unlink, Folder & "/" & Name_Of (I)) /= REPLY_OK
            then
               return False;
            end if;
         end loop;
         if Path_Call (FQ.Queue_Rmdir, Folder) /= REPLY_OK then
            return False;
         end if;
         debugPrint ("QUEUE-DIRECTORY-CHECK: PASS" & LF);
         return True;
      end Directory_Check;

      --  Change notifications (docs/filesystem-protocol-v2.md step 4): a
      --  folder watch and a subtree watch see creations, writes, renames
      --  and removals, typed and named; a flood past the ring's room ends in
      --  one Rescan_Needed and nothing after it until the client reads; the
      --  watch then works again; unwatching or removing the folder ends a
      --  watch with a Watch_Ended record.
      function Events_Check return Boolean is
         package FE renames CuBit.Filesystem_Events;
         use type FE.Event_Kind, FS.Event_Result;
         Folder : constant String := "@nvme:0/watch-dir";
         Opened, Woken : Boolean;
         Folder_Handle, Folder_Watch, Tree_Watch, Ignore : Unsigned_64;

         --  The next record, waiting through the wake if none is there.
         function Next (Item : out FE.Event; Name : out String; Length : out Natural) return Boolean is
            Bytes : FE.Name_Bytes;
            Got : FE.Name_Length;
            Result : FS.Event_Result;
         begin
            Name := [others => ' '];
            Length := 0;
            for Attempt in 1 .. 3 loop
               FS.Take_Event (Session, Item, Bytes, Got, Result);
               case Result is
                  when FS.Taken =>
                     Length := Natural'Min (Got, Name'Length);
                     for I in 1 .. Length loop
                        Name (Name'First + I - 1) := Character'Val (Bytes (I));
                     end loop;
                     return True;
                  when FS.Malformed =>
                     debugPrint ("QUEUE-EVENTS-CHECK: malformed record" & LF);
                     return False;
                  when FS.Empty =>
                     if not Arm or else not Wake_Completes (PROMPT_MS, Woken) then
                        debugPrint ("QUEUE-EVENTS-CHECK: step 1" & LF);
                        return False;
                     end if;
               end case;
            end loop;
            debugPrint ("QUEUE-EVENTS-CHECK: step 2" & LF);
            return False;
         end Next;

         function Expect (Watch : Unsigned_64; Kind : FE.Event_Kind; Name : String) return Boolean is
            Item : FE.Event;
            Text : String (1 .. 300);
            Length : Natural;
         begin
            if not Next (Item, Text, Length) then
               debugPrint ("QUEUE-EVENTS-CHECK: no record for " & Name & LF);
               return False;
            elsif Unsigned_64 (Item.Watch) /= Watch or else Item.Kind /= Kind or else
              Text (1 .. Length) /= Name
            then
               debugPrint ("QUEUE-EVENTS-CHECK: wanted " & Name & " got " & Text (1 .. Length) &
                 Item.Watch'Image & FE.Event_Kind'Image (Item.Kind) & LF);
               debugPrint ("QUEUE-EVENTS-CHECK: step 3" & LF);
               return False;
            end if;
            return True;
         end Expect;

         function Make (Path : String) return Boolean is
            File : Unsigned_64;
         begin
            Put (Path);
            return Call ((Operation => FQ.Queue_Open,
                          Options => Unsigned_32 (OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE),
                          Length => Path'Length, others => <>), File) = REPLY_OK
              and then Path_Call (FQ.Queue_Close, "", Handle => File) = REPLY_OK;
         end Make;

         --  Flood names: long relative paths (two 240-byte folders) but
         --  short entries, so the folder stays within ext2's direct blocks
         --  (the service removes no directory that uses indirect blocks).
         Deep_1 : constant String := "sub/" & [1 .. 240 => 'd'];
         Deep_2 : constant String := Deep_1 & "/" & [1 .. 240 => 'e'];
         function Long_Name (I : Positive) return String is
            Image : constant String := Positive'Image (I);
         begin
            return Deep_2 & "/f" & Image (Image'First + 1 .. Image'Last);
         end Long_Name;

         Empty_Ring : FE.Event;
         Bytes_Unused : FE.Name_Bytes;
         Length_Unused : FE.Name_Length;
         Result_Unused : FS.Event_Result := FS.Empty;
         Created_Seen, Rescans, After_Rescan : Natural := 0;
         Flood : constant := 200;
      begin
         FS.Open_Events (Session, Opened);
         if not Opened or else Path_Call (FQ.Queue_Mkdir, Folder) /= REPLY_OK then
            debugPrint ("QUEUE-EVENTS-CHECK: setup" & LF);
            return False;
         end if;
         Put (Folder);
         if Call ((Operation => FQ.Queue_Open_Directory, Length => Folder'Length, others => <>),
                  Folder_Handle) /= REPLY_OK or else
           Call ((Operation => FQ.Queue_Watch, Handle => Folder_Handle, Options => 16#80#,
                  others => <>), Ignore) /= REPLY_ERR or else
           Call ((Operation => FQ.Queue_Watch, Handle => Folder_Handle, others => <>),
                 Folder_Watch) /= REPLY_OK or else
           Call ((Operation => FQ.Queue_Watch, Handle => Folder_Handle, Options => FQ.Watch_Subtree,
                  others => <>), Tree_Watch) /= REPLY_OK or else
           Path_Call (FQ.Queue_Close_Directory, "", Handle => Folder_Handle) /= REPLY_OK
         then
            debugPrint ("QUEUE-EVENTS-CHECK: watch" & LF);
            return False;
         end if;
         --  A file, a folder and a file in it: the folder watch sees its own
         --  entries only, the subtree watch all of them.
         if not Make (Folder & "/a.txt") or else
           not Expect (Folder_Watch, FE.Created, "a.txt") or else
           not Expect (Tree_Watch, FE.Created, "a.txt") or else
           Path_Call (FQ.Queue_Mkdir, Folder & "/sub") /= REPLY_OK or else
           not Expect (Folder_Watch, FE.Created, "sub") or else
           not Expect (Tree_Watch, FE.Created, "sub") or else
           not Make (Folder & "/sub/b.txt") or else
           not Expect (Tree_Watch, FE.Created, "sub/b.txt")
         then
            debugPrint ("QUEUE-EVENTS-CHECK: step 4" & LF);
            return False;
         end if;
         --  A write, reported once at close.
         declare
            Path : constant String := Folder & "/a.txt";
            File : Unsigned_64;
         begin
            Put (Path);
            if Call ((Operation => FQ.Queue_Open, Options => Unsigned_32 (OPEN_READ_WRITE),
                      Length => Path'Length, others => <>), File) /= REPLY_OK
            then
               debugPrint ("QUEUE-EVENTS-CHECK: step 5" & LF);
               return False;
            end if;
            Put ("hello");
            for Twice in 1 .. 2 loop
               if Call ((Operation => FQ.Queue_Write_At, Handle => File, Position => 0, Length => 5,
                         others => <>), Ignore) /= REPLY_OK
               then
                  debugPrint ("QUEUE-EVENTS-CHECK: step 6" & LF);
                  return False;
               end if;
            end loop;
            if Path_Call (FQ.Queue_Close, "", Handle => File) /= REPLY_OK or else
              not Expect (Folder_Watch, FE.Modified, "a.txt") or else
              not Expect (Tree_Watch, FE.Modified, "a.txt")
            then
               debugPrint ("QUEUE-EVENTS-CHECK: step 7" & LF);
               return False;
            end if;
         end;
         --  A rename: two records sharing a cookie, each watch.
         Put (Folder & "/a.txt" & Folder & "/c.txt");
         if Call ((Operation => FQ.Queue_Rename, Position => Folder'Length + 6,
                   Length => 2 * (Folder'Length + 6), others => <>), Ignore) /= REPLY_OK or else
           not Expect (Folder_Watch, FE.Renamed_From, "a.txt") or else
           not Expect (Tree_Watch, FE.Renamed_From, "a.txt") or else
           not Expect (Folder_Watch, FE.Renamed_To, "c.txt") or else
           not Expect (Tree_Watch, FE.Renamed_To, "c.txt") or else
           Path_Call (FQ.Queue_Unlink, Folder & "/c.txt") /= REPLY_OK or else
           not Expect (Folder_Watch, FE.Removed, "c.txt") or else
           not Expect (Tree_Watch, FE.Removed, "c.txt")
         then
            debugPrint ("QUEUE-EVENTS-CHECK: step 8" & LF);
            return False;
         end if;
         --  An unwatched watch's last record is its Watch_Ended.
         if Call ((Operation => FQ.Queue_Unwatch, Handle => Folder_Watch, others => <>), Ignore) /= REPLY_OK
           or else not Expect (Folder_Watch, FE.Watch_Ended, "")
         then
            debugPrint ("QUEUE-EVENTS-CHECK: step 9" & LF);
            return False;
         end if;
         --  A flood, unread: events while there is room, then one
         --  Rescan_Needed, then nothing for that watch.
         if Path_Call (FQ.Queue_Mkdir, Folder & "/" & Deep_1) /= REPLY_OK or else
           Path_Call (FQ.Queue_Mkdir, Folder & "/" & Deep_2) /= REPLY_OK
         then
            debugPrint ("QUEUE-EVENTS-CHECK: deep folders" & LF);
            return False;
         end if;
         for I in 1 .. Flood loop
            if not Make (Folder & "/" & Long_Name (I)) then
               debugPrint ("QUEUE-EVENTS-CHECK: flood create" & LF);
               return False;
            end if;
         end loop;
         loop
            FS.Take_Event (Session, Empty_Ring, Bytes_Unused, Length_Unused, Result_Unused);
            exit when Result_Unused /= FS.Taken;
            if Empty_Ring.Kind = FE.Rescan_Needed then
               Rescans := Rescans + 1;
            elsif Rescans > 0 then
               After_Rescan := After_Rescan + 1;
            elsif Empty_Ring.Kind = FE.Created then
               Created_Seen := Created_Seen + 1;
            end if;
         end loop;
         if Result_Unused /= FS.Empty or else Rescans /= 1 or else After_Rescan /= 0 or else
           Created_Seen = 0 or else Created_Seen >= Flood
         then
            debugPrint ("QUEUE-EVENTS-CHECK: flood" & Created_Seen'Image & Rescans'Image &
              After_Rescan'Image & LF);
            debugPrint ("QUEUE-EVENTS-CHECK: step 10" & LF);
            return False;
         end if;
         --  Read: the watch works again.
         if not Make (Folder & "/sub/after.txt") or else
           not Expect (Tree_Watch, FE.Created, "sub/after.txt")
         then
            debugPrint ("QUEUE-EVENTS-CHECK: step 11" & LF);
            return False;
         end if;
         --  Clean up (unwatched), then the folder's removal ends a watch.
         if Call ((Operation => FQ.Queue_Unwatch, Handle => Tree_Watch, others => <>), Ignore) /= REPLY_OK
           or else not Expect (Tree_Watch, FE.Watch_Ended, "")
         then
            debugPrint ("QUEUE-EVENTS-CHECK: step 12" & LF);
            return False;
         end if;
         for I in 1 .. Flood loop
            if Path_Call (FQ.Queue_Unlink, Folder & "/" & Long_Name (I)) /= REPLY_OK then
               debugPrint ("QUEUE-EVENTS-CHECK: step 13" & LF);
               return False;
            end if;
         end loop;
         if Path_Call (FQ.Queue_Rmdir, Folder & "/" & Deep_2) /= REPLY_OK or else
           Path_Call (FQ.Queue_Rmdir, Folder & "/" & Deep_1) /= REPLY_OK or else
           Path_Call (FQ.Queue_Unlink, Folder & "/sub/after.txt") /= REPLY_OK or else
           Path_Call (FQ.Queue_Unlink, Folder & "/sub/b.txt") /= REPLY_OK or else
           Path_Call (FQ.Queue_Rmdir, Folder & "/sub") /= REPLY_OK
         then
            debugPrint ("QUEUE-EVENTS-CHECK: step 14" & LF);
            return False;
         end if;
         Put (Folder);
         if Call ((Operation => FQ.Queue_Open_Directory, Length => Folder'Length, others => <>),
                  Folder_Handle) /= REPLY_OK or else
           Call ((Operation => FQ.Queue_Watch, Handle => Folder_Handle, others => <>),
                 Folder_Watch) /= REPLY_OK or else
           Path_Call (FQ.Queue_Close_Directory, "", Handle => Folder_Handle) /= REPLY_OK or else
           Path_Call (FQ.Queue_Rmdir, Folder) /= REPLY_OK or else
           not Expect (Folder_Watch, FE.Watch_Ended, "") or else
           Call ((Operation => FQ.Queue_Unwatch, Handle => Folder_Watch, others => <>), Ignore) /= REPLY_ERR
         then
            debugPrint ("QUEUE-EVENTS-CHECK: step 15" & LF);
            return False;
         end if;
         debugPrint ("QUEUE-EVENTS-CHECK: PASS" & LF);
         return True;
      end Events_Check;

      --  Granted-scope query (docs/filesystem-protocol-v2.md step 6): the
      --  service answers this process's own profile, exactly its manifest's
      --  two scopes; a range too small for them is refused with the count.
      function Scopes_Check return Boolean is
         package FA renames CuBit.File_Access;
         Count : Unsigned_64;
         Policy : FA.Policy;
         Decoded : Boolean;
         Read_Write_Create : constant FA.Rights_Set :=
           [FA.Read_Objects | FA.Write_Objects | FA.Create_Objects => True, others => False];
      begin
         if Call ((Operation => FQ.Queue_List_Scopes, Length => FA.Wire_Entry_Bytes, others => <>),
                  Count) /= REPLY_NO_SPACE or else Count /= 2
         then
            debugPrint ("QUEUE-SCOPES-CHECK: small range" & Count'Image & LF);
            return False;
         end if;
         if Call ((Operation => FQ.Queue_List_Scopes, Length => 2 * FA.Wire_Entry_Bytes, others => <>),
                  Count) /= REPLY_OK or else Count /= 2
         then
            debugPrint ("QUEUE-SCOPES-CHECK: list" & Count'Image & LF);
            return False;
         end if;
         declare
            Shared : constant FA.Wire_Bytes (1 .. 2 * FA.Wire_Entry_Bytes)
              with Import, Address => FS.Arena (Session);
            Copy : constant FA.Wire_Bytes := Shared;   --  copy, then validate
            Prefix_At : constant := 9;
            function Prefix (Number : Positive) return String is
               Base : constant Positive := (Number - 1) * FA.Wire_Entry_Bytes + 1;
               Length : constant Natural := Natural (Copy (Base + 1));
               Text : String (1 .. Length);
            begin
               for I in Text'Range loop
                  Text (I) := Character'Val (Copy (Base + Prefix_At - 1 + I - 1));
               end loop;
               return Text;
            end Prefix;
         begin
            FA.Decode (Copy, Policy, Decoded);
            if not Decoded or else Prefix (1) /= "@nvme:0/" or else Prefix (2) /= "@mem:0/work/"
              or else Copy (1) /= FA.Rights_To_Wire (Read_Write_Create)
              or else not FA.Allows (Policy, "@nvme:0/watch-dir", Read_Write_Create)
              or else FA.Allows (Policy, "@cd:0/apps", [FA.Read_Objects => True, others => False])
            then
               debugPrint ("QUEUE-SCOPES-CHECK: entries " & Prefix (1) & " " & Prefix (2) & LF);
               return False;
            end if;
         end;
         debugPrint ("QUEUE-SCOPES-CHECK: PASS" & LF);
         return True;
      end Scopes_Check;

      --  Free space (step 7): a volume's description, free counts that
      --  follow a 1 MiB file's writes and removal, refusals for a path
      --  outside the grant, a traversal and a short range.
      function Volume_Check return Boolean is
         package VD renames CuBit.Volume_Descriptions;
         use type VD.Volume_Kind;
         Ignore : Unsigned_64;

         function Describe (Path : String; Item : out VD.Description) return Unsigned_32 is
            Label : Unsigned_32;
            Length : Unsigned_64;
            Decoded : Boolean;
         begin
            Item := (others => <>);
            Put (Path);
            Label := Call ((Operation => FQ.Queue_Describe_Volume, Position => Path'Length,
                            Length => Unsigned_64'Max (Path'Length, VD.Record_Bytes), others => <>),
                           Length);
            if Label = REPLY_OK then
               declare
                  Shared : constant VD.Record_Image with Import, Address => FS.Arena (Session);
                  Copy : constant VD.Record_Image := Shared;
               begin
                  VD.Decode (Copy, Item, Decoded);
                  if Length /= VD.Record_Bytes or else not Decoded then
                     return REPLY_ERR;
                  end if;
               end;
            end if;
            return Label;
         end Describe;

         function Name_Is (Item : VD.Description; Name : String) return Boolean is
           (Item.Length = Name'Length and then
            (for all I in Name'Range => Item.Name (I - Name'First + 1) = Character'Pos (Name (I))));

         Before, During, After, Work : VD.Description;
         Path : constant String := "@nvme:0/volume-check.bin";
         Chunk : constant := 4_096;
         Chunks : constant := 256;   --  1 MiB
         File : Unsigned_64;
      begin
         if Describe ("@nvme:0/", Before) /= REPLY_OK or else
           Before.Kind not in VD.Ext2 | VD.Ext3 or else not Name_Is (Before, "nvme:0") or else
           Before.Total_Blocks = 0 or else Before.Free_Blocks = 0 or else Before.Total_Inodes = 0
         then
            debugPrint ("QUEUE-VOLUME-CHECK: describe nvme" & LF);
            return False;
         end if;
         Put (Path);
         if Call ((Operation => FQ.Queue_Open,
                   Options => Unsigned_32 (OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE),
                   Length => Path'Length, others => <>), File) /= REPLY_OK
         then
            return False;
         end if;
         Put ([1 .. Chunk => 'v']);
         for I in 0 .. Chunks - 1 loop
            if Call ((Operation => FQ.Queue_Write_At, Handle => File, Position => Unsigned_64 (I * Chunk),
                      Length => Chunk, others => <>), Ignore) /= REPLY_OK
            then
               return False;
            end if;
         end loop;
         if Path_Call (FQ.Queue_Close, "", Handle => File) /= REPLY_OK or else
           Describe ("@nvme:0/volume-check.bin", During) /= REPLY_OK or else
           During.Free_Blocks + Unsigned_64 (Chunks * Chunk) / Unsigned_64 (During.Block) > Before.Free_Blocks
           or else During.Free_Inodes >= Before.Free_Inodes
         then
            debugPrint ("QUEUE-VOLUME-CHECK: free after write" & Before.Free_Blocks'Image &
              During.Free_Blocks'Image & LF);
            return False;
         end if;
         --  Removed: its blocks are free, or freed and free once committed.
         if Path_Call (FQ.Queue_Unlink, Path) /= REPLY_OK or else
           Describe ("@nvme:0/", After) /= REPLY_OK or else
           After.Free_Blocks + After.Releasing_Blocks + Before.Releasing_Blocks < Before.Free_Blocks
           or else After.Free_Inodes /= Before.Free_Inodes
         then
            debugPrint ("QUEUE-VOLUME-CHECK: free after unlink" & After.Free_Blocks'Image &
              After.Releasing_Blocks'Image & LF);
            return False;
         end if;
         if Describe ("@mem:0/work/", Work) /= REPLY_OK or else not Name_Is (Work, "mem:0") or else
           Describe ("@cd:0/apps", Work) /= REPLY_ACCESS_DENIED or else
           Describe ("@nvme:0/../x", Work) /= REPLY_ERR
         then
            debugPrint ("QUEUE-VOLUME-CHECK: other volumes" & LF);
            return False;
         end if;
         Put ("@nvme:0/");
         if Call ((Operation => FQ.Queue_Describe_Volume, Position => 8, Length => VD.Record_Bytes - 1,
                   others => <>), Ignore) /= REPLY_ERR
         then
            debugPrint ("QUEUE-VOLUME-CHECK: short range" & LF);
            return False;
         end if;
         debugPrint ("QUEUE-VOLUME-CHECK: PASS" & LF);
         return True;
      end Volume_Check;

      --  Server-side copy (step 5): whole files and ranges, across volumes,
      --  byte-exact; a cancel and a passed deadline end a copy with a prefix
      --  copied; past Maximum_Copies running, busy; refusals for a target
      --  that cannot write and for overlapping ranges of one file.
      function Copy_Check return Boolean is
         Chunk : constant := 4_096;
         Source_Bytes : constant := 1_536 * 1_024;
         Big_Bytes : constant := 3 * Source_Bytes;
         Source_Path : constant String := "@nvme:0/copy-source.bin";
         Ignore : Unsigned_64;

         function Pattern (At_Byte : Unsigned_64) return Character is
           (Character'Val ((At_Byte * 7 + At_Byte / 4_099) mod 251));

         function Open (Path : String; Options : Open_Options; File : out Unsigned_64) return Boolean is
         begin
            Put (Path);
            return Call ((Operation => FQ.Queue_Open, Options => Unsigned_32 (Options),
                          Length => Path'Length, others => <>), File) = REPLY_OK;
         end Open;

         function Close (File : Unsigned_64) return Boolean is
           (Path_Call (FQ.Queue_Close, "", Handle => File) = REPLY_OK);

         function Copy_Request
           (Source, Target, From, To, Length : Unsigned_64;
            Deadline : Unsigned_64 := CuBit.Messages.Wait_Forever) return FQ.Request is
           (Operation => FQ.Queue_Copy, Handle => Source, Spare_1 => Target, Position => From,
            Arena_Offset => To, Length => Length, Spare_2 => Deadline, others => <>);

         --  Target bytes At .. At + Length - 1 hold the source's From ...
         function Holds (File, At_Byte, From, Length : Unsigned_64) return Boolean is
            Done : Unsigned_64 := 0;
            Got : Unsigned_64;
         begin
            while Done < Length loop
               declare
                  Part : constant Unsigned_64 := Unsigned_64'Min (Chunk, Length - Done);
               begin
                  if Call ((Operation => FQ.Queue_Read_At, Handle => File, Position => At_Byte + Done,
                            Length => Part, others => <>), Got) /= REPLY_OK or else Got /= Part
                  then
                     return False;
                  end if;
                  declare
                     View : constant String (1 .. Natural (Part)) with Import, Address => FS.Arena (Session);
                     Copy : constant String := View;
                  begin
                     for I in Copy'Range loop
                        if Copy (I) /= Pattern ((From + Done + Unsigned_64 (I - 1)) mod Source_Bytes) then
                           return False;
                        end if;
                     end loop;
                  end;
                  Done := Done + Part;
               end;
            end loop;
            return True;
         end Holds;

         --  Submit, then wait for answers until Tag's (others recorded).
         type Answer_Of is record
            Tag : FS.Token := 0;
            Status : Unsigned_32 := 0;
            Value : Unsigned_64 := 0;
         end record;
         Seen : array (1 .. 8) of Answer_Of;
         Seen_Count : Natural := 0;
         function Await (Tag : FS.Token; Status : out Unsigned_32; Value : out Unsigned_64) return Boolean is
            Answer : FQ.Queues.Completion;
            Got : Boolean;
            Waited : FS.Wait_Result;
         begin
            Status := 0;
            Value := 0;
            for I in 1 .. Seen_Count loop
               if Seen (I).Tag = Tag then
                  Status := Seen (I).Status;
                  Value := Seen (I).Value;
                  Seen (I) := Seen (Seen_Count);
                  Seen_Count := Seen_Count - 1;
                  return True;
               end if;
            end loop;
            loop
               FS.Wait_Answer (Session, Deadline_After (PROMPT_MS), Waited);
               if Waited /= FS.Answer_Waiting then
                  return False;
               end if;
               FS.Reap (Session, Answer, Got);
               if Got then
                  if Answer.Tag = Tag then
                     Status := Answer.Answer.Status;
                     Value := Answer.Answer.Value;
                     return True;
                  elsif Seen_Count = Seen'Last then
                     return False;
                  end if;
                  Seen_Count := Seen_Count + 1;
                  Seen (Seen_Count) := (Answer.Tag, Answer.Answer.Status, Answer.Answer.Value);
               end if;
            end loop;
         end Await;

         Source, Target, Big, Small, Work, Reader : Unsigned_64;
         Status : Unsigned_32;
         Value : Unsigned_64;
      begin
         --  The source: 1.5 MiB of a pattern.
         if not Open (Source_Path, OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE, Source) then
            debugPrint ("QUEUE-COPY-CHECK: source" & LF);
            return False;
         end if;
         for C in 0 .. Source_Bytes / Chunk - 1 loop
            declare
               View : String (1 .. Chunk) with Import, Address => FS.Arena (Session);
            begin
               for I in View'Range loop
                  View (I) := Pattern (Unsigned_64 (C * Chunk + I - 1));
               end loop;
            end;
            if Call ((Operation => FQ.Queue_Write_At, Handle => Source, Position => Unsigned_64 (C * Chunk),
                      Length => Chunk, others => <>), Ignore) /= REPLY_OK
            then
               return False;
            end if;
         end loop;
         --  Whole file, to the source's end: one answer, every byte.
         if not Open ("@nvme:0/copy-target.bin", OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE, Target)
           or else Call (Copy_Request (Source, Target, 0, 0, FQ.Copy_To_End), Value) /= REPLY_OK
           or else Value /= Source_Bytes or else not Holds (Target, 0, 0, Source_Bytes)
         then
            debugPrint ("QUEUE-COPY-CHECK: whole" & Value'Image & LF);
            return False;
         end if;
         --  A range at odd offsets, across volumes (rename would answer
         --  CROSS_VOLUME; the copy goes through the service).
         if not Open ("@mem:0/work/copy-range.bin", OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE, Small)
           or else Call (Copy_Request (Source, Small, 4_099, 10, 70_001), Value) /= REPLY_OK
           or else Value /= 70_001 or else not Holds (Small, 10, 4_099, 70_001)
         then
            debugPrint ("QUEUE-COPY-CHECK: range" & Value'Image & LF);
            return False;
         end if;
         --  A file three times as long, by three copies; then copies of it
         --  that a cancel and a passed deadline end early.
         if not Open ("@nvme:0/copy-big.bin", OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE, Big) then
            return False;
         end if;
         for Part in 0 .. 2 loop
            if Call (Copy_Request (Source, Big, 0, Unsigned_64 (Part) * Source_Bytes, Source_Bytes), Value)
              /= REPLY_OK or else Value /= Source_Bytes
            then
               debugPrint ("QUEUE-COPY-CHECK: big" & LF);
               return False;
            end if;
         end loop;
         if not Open ("@nvme:0/copy-work.bin", OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE, Work) then
            return False;
         end if;
         declare
            Copy_Tag, Cancel_Tag : FS.Token;
            Cancel_Status : Unsigned_32;
         begin
            FS.Submit (Session, Copy_Request (Big, Work, 0, 0, FQ.Copy_To_End), Copy_Tag);
            FS.Submit (Session, (Operation => FQ.Queue_Cancel, Handle => Unsigned_64 (Copy_Tag), others => <>),
                       Cancel_Tag);
            if Copy_Tag = 0 or else Cancel_Tag = 0
              or else not Await (Cancel_Tag, Cancel_Status, Ignore) or else Cancel_Status /= REPLY_OK
              or else not Await (Copy_Tag, Status, Value) or else Status /= REPLY_CANCELLED
              or else Value >= Big_Bytes or else not Holds (Work, 0, 0, Value)
            then
               debugPrint ("QUEUE-COPY-CHECK: cancel" & Value'Image & LF);
               return False;
            end if;
         end;
         if Call (Copy_Request (Big, Work, 0, 0, FQ.Copy_To_End, Deadline => syscall (SYSCALL_GETTIME)), Value)
           /= REPLY_DEADLINE or else Value >= Big_Bytes or else not Holds (Work, 0, 0, Value)
         then
            debugPrint ("QUEUE-COPY-CHECK: deadline" & Value'Image & LF);
            return False;
         end if;
         --  Past Maximum_Copies running: busy. The others complete.
         declare
            Tags : array (1 .. FQ.Maximum_Copies + 1) of FS.Token;
            Busy, Done : Natural := 0;
         begin
            for T of Tags loop
               FS.Submit (Session, Copy_Request (Big, Work, 0, 0, FQ.Copy_To_End), T);
            end loop;
            for T of Tags loop
               if T = 0 or else not Await (T, Status, Value) then
                  return False;
               elsif Status = REPLY_BUSY then
                  Busy := Busy + 1;
               elsif Status = REPLY_OK and then Value = Big_Bytes then
                  Done := Done + 1;
               end if;
            end loop;
            if Busy /= 1 or else Done /= FQ.Maximum_Copies or else not Holds (Work, 0, 0, Big_Bytes) then
               debugPrint ("QUEUE-COPY-CHECK: busy" & Busy'Image & Done'Image & LF);
               return False;
            end if;
         end;
         --  Refusals: a read-only target, ranges of one file that overlap,
         --  a handle that is not one.
         if not Open (Source_Path, OPEN_READ_ONLY, Reader)
           or else Call (Copy_Request (Source, Reader, 0, 0, 10), Ignore) /= REPLY_ACCESS_DENIED
           or else Call (Copy_Request (Big, Big, 0, 100, 200), Ignore) /= REPLY_ERR
           or else Call (Copy_Request (Big, Big, 0, Big_Bytes, 200), Value) /= REPLY_OK
           or else Call (Copy_Request (Source, 16#DEAD#, 0, 0, 10), Ignore) /= REPLY_WRONG_OBJECT_TYPE
           or else not Close (Reader)
         then
            debugPrint ("QUEUE-COPY-CHECK: refusals" & LF);
            return False;
         end if;
         if not (Close (Source) and then Close (Target) and then Close (Small) and then Close (Big)
                 and then Close (Work))
           or else Path_Call (FQ.Queue_Unlink, Source_Path) /= REPLY_OK
           or else Path_Call (FQ.Queue_Unlink, "@nvme:0/copy-target.bin") /= REPLY_OK
           or else Path_Call (FQ.Queue_Unlink, "@mem:0/work/copy-range.bin") /= REPLY_OK
           or else Path_Call (FQ.Queue_Unlink, "@nvme:0/copy-big.bin") /= REPLY_OK
           or else Path_Call (FQ.Queue_Unlink, "@nvme:0/copy-work.bin") /= REPLY_OK
         then
            debugPrint ("QUEUE-COPY-CHECK: cleanup" & LF);
            return False;
         end if;
         debugPrint ("QUEUE-COPY-CHECK: PASS" & LF);
         return True;
      end Copy_Check;

      Good : Boolean;
   begin
      FS.Open (Session, CAP_SLOT_FS, 2, Opened);
      if not Opened then
         debugPrint ("QUEUE-WAKE-CHECK: queue refused" & LF);
         return False;
      end if;
      --  Every section runs, so each reports its own marker.
      Good := Wake_Check and then Rename_Check and then Directory_Check;
      Good := Events_Check and then Good;
      Good := Scopes_Check and then Good;
      Good := Volume_Check and then Good;
      Good := Copy_Check and then Good;
      FS.Close (Session);
      --  The session's pages are released once the service has let go of
      --  them (OWNED-EXIT-CHECK counts what the process leaves behind).
      declare
         Deadline : constant Unsigned_64 := Deadline_After (PROMPT_MS);
         Ignore : Message;
         Yielded : Unsigned_64 with Unreferenced;
      begin
         while CuBit.Channels.Retiring_Regions > 0 loop
            if syscall (SYSCALL_GETTIME) >= Deadline then
               debugPrint ("QUEUE-CHECK: session memory not released" & LF);
               return False;
            end if;
            --  Kernel notices (the grants' ends) are not needed here.
            while Poll_Event (Ignore) loop
               null;
            end loop;
            Yielded := syscall (SYSCALL_YIELD);
         end loop;
      end;
      return Good;
   end exerciseQueue;

begin
   raw := syscall (SYSCALL_SBRK, 2 * PAGE_SIZE);
   if raw = Unsigned_64'Last then
      debugPrint ("STORAGE-CHECK: buffer allocation failed" & LF);
      return;
   end if;

   aligned := (raw + PAGE_SIZE - 1) and not (PAGE_SIZE - 1);

   if exerciseOwnedMemory then
      debugPrint ("OWNED-MEMORY-CHECK: PASS" & LF);
   else
      debugPrint ("OWNED-MEMORY-CHECK: FAIL" & LF);
      return;
   end if;

   if exerciseOwnedGrantRetention then
      debugPrint ("OWNED-GRANT-RETENTION-CHECK: PASS" & LF);
   else
      debugPrint ("OWNED-GRANT-RETENTION-CHECK: FAIL" & LF);
      return;
   end if;

   if exerciseGrantReferences then
      debugPrint ("GRANT-REFERENCE-CHECK: PASS" & LF);
   else
      debugPrint ("GRANT-REFERENCE-CHECK: FAIL" & LF);
      return;
   end if;

   -- Reuse one backing frame (and usually the same grant slots) beyond the pin-count
   -- limit. Leaked mapping pins fail this run; stale epochs must not satisfy
   -- later shootdowns. The headless guest runs with four online CPUs.
   for Round in 1 .. 128 loop
      if not exerciseGrantReferences then
         debugPrint ("GRANT-RECLAMATION-CHECK: FAIL" & LF);
         return;
      end if;
   end loop;
   debugPrint ("GRANT-RECLAMATION-CHECK: PASS" & LF);

   CuBit.Memory_Grants.Create_Via_Capability
     (slot      => CAP_SLOT_FS,
      localAddr => To_Address (Integer_Address (aligned)),
      numPages  => 1,
      readWrite => True,
      reference => grantRef,
      success   => grantOk);

   if not grantOk then
      debugPrint ("STORAGE-CHECK: grant failed" & LF);
      return;
   end if;

   if exerciseRAMVolume then
      debugPrint ("RAM-VOLUME-CHECK: PASS" & LF);
   else
      debugPrint ("RAM-VOLUME-CHECK: FAIL" & LF);
      return;
   end if;

   if exerciseVolumeIsolation then
      debugPrint ("VOLUME-ISOLATION-CHECK: PASS" & LF);
   else
      debugPrint ("VOLUME-ISOLATION-CHECK: FAIL" & LF);
      return;
   end if;

   if exerciseRejectedObjects then
      debugPrint ("LINK-POLICY-CHECK: PASS" & LF);
   else
      debugPrint ("LINK-POLICY-CHECK: FAIL" & LF);
      return;
   end if;

   if not exerciseQueue then
      debugPrint ("QUEUE-CHECK: FAIL" & LF);
      return;
   end if;

   if exerciseStorage and then exercisePositioned and then exerciseCoherence and then
     exerciseDoubleOverwrite and then exerciseExclusive
   then
      debugPrint ("STORAGE-CHECK: PASS" & LF);
   else
      debugPrint ("STORAGE-CHECK: FAIL" & LF);
   end if;

   CuBit.Memory_Grants.Revoke (grantRef, grantOk);
   -- Exercise reaper cleanup rather than explicit release. These allocations
   -- have no outstanding grants; a separate test covers retained grant pins.
   declare
      First : constant Unsigned_64 := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 4096);
      Second : constant Unsigned_64 := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 12288);
   begin
      if First = 0 or Second = 0 then
         debugPrint ("OWNED-EXIT-CHECK: FAIL allocation" & LF);
      else
         debugPrint ("OWNED-EXIT-CHECK: leaving two allocations" & LF);
      end if;
   end;
end main;
