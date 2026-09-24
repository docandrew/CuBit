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
         M.tag := capCall (CAP_SLOT_FS, M);
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
      M.tag := capCall (CAP_SLOT_FS, M);
      if M.tag.label /= REPLY_OK or else M.words (0) /= PAYLOAD'Length then
         return False;
      end if;
      M := Flush_Request (Handle);
      M.tag := capCall (CAP_SLOT_FS, M);
      if M.tag.label /= REPLY_DURABILITY_UNSUPPORTED then
         return False;
      end if;
      M := Close_Request (Handle);
      M.tag := capCall (CAP_SLOT_FS, M);
      if M.tag.label /= REPLY_OK or else not Open_File (OPEN_READ_ONLY) then
         return False;
      end if;
      Buffer := [others => '?'];
      M := Read_At_Request (Handle, grantRef, 509 + PAYLOAD'Length, 0);
      M.tag := capCall (CAP_SLOT_FS, M);
      if M.tag.label /= REPLY_OK or else
        M.words (0) /= 509 + PAYLOAD'Length or else
        Buffer (1 .. 509) /= [1 .. 509 => Character'Val (0)] or else
        Buffer (510 .. 509 + PAYLOAD'Length) /= PAYLOAD
      then
         return False;
      end if;
      M := Close_Request (Handle);
      M.tag := capCall (CAP_SLOT_FS, M);
      if M.tag.label /= REPLY_OK or else
        not Open_File (OPEN_READ_WRITE or OPEN_TRUNCATE)
      then
         return False;
      end if;
      M := Read_Request (Handle, grantRef, 1);
      M.tag := capCall (CAP_SLOT_FS, M);
      if M.tag.label /= REPLY_OK or else M.words (0) /= 0 then
         return False;
      end if;
      M := Close_Request (Handle);
      M.tag := capCall (CAP_SLOT_FS, M);
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
         M.tag := capCall (CAP_SLOT_FS, M);
         Handle := File_Handle (M.words (0));
         return M.tag.label = REPLY_OK;
      end Open_File;

      function Write_File (Handle : File_Handle; Value : String) return Boolean is
      begin
         Buffer (1 .. Value'Length) := Value;
         M := Write_At_Request (Handle, grantRef, Value'Length, 0);
         M.tag := capCall (CAP_SLOT_FS, M);
         return M.tag.label = REPLY_OK and then M.words (0) = Value'Length;
      end Write_File;

      function Read_File (Handle : File_Handle; Value : String) return Boolean is
      begin
         Buffer := [others => '?'];
         M := Read_At_Request (Handle, grantRef, 16, 0);
         M.tag := capCall (CAP_SLOT_FS, M);
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
      M.tag := capCall (CAP_SLOT_FS, M);
      if M.tag.label /= REPLY_OK or else M.words (0) /= 0 or else
        not Read_File (Disk, "DISK")
      then
         return False;
      end if;
      for Handle of File_Handle_Array'[RAM, Disk, Alias] loop
         M := Close_Request (Handle);
         M.tag := capCall (CAP_SLOT_FS, M);
         if M.tag.label /= REPLY_OK then
            return False;
         end if;
      end loop;
      return True;
   end exerciseVolumeIsolation;

   function exerciseGrantReferences return Boolean is
      use CuBit.Memory_Grants;
      pid         : constant ProcessID := syscall (SYSCALL_GETPID);
      wrongOwner  : constant ProcessID := (if pid = 1 then 2 else 1);
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
      if not ok or else secondRef.slot /= firstRef.slot or else
         secondRef.generation = firstRef.generation
      then
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
         Response.tag := capCall (CAP_SLOT_FS, Response);
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
      if not Check (Write_At_Request (A, grantRef, 1, 16#0100_0000#),
                    0, REPLY_FILE_RANGE_UNSUPPORTED) or else
        not Check (Seek_Request (B, 0, From_End), PAGE_SIZE + Text'Length) or else
        not Check (Seek_Request (A, 0, From_End), PAGE_SIZE + Text'Length)
      then return False; end if;
      if not Open (OPEN_READ_ONLY, C) or else
        not Check (Write_At_Request (C, grantRef, 1, 0), 0, REPLY_ACCESS_DENIED)
        or else not Check (Close_Request (C))
      then return False; end if;
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
         Response.tag := capCall (CAP_SLOT_FS, Response);
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
           (item : Directory_Entry;
            expected : String) return Boolean
         is
         begin
            if Natural (item.nameLength) /= expected'Length then
               return False;
            end if;
            for index in 1 .. expected'Length loop
               if Character'Val (item.name (index)) /=
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
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_WRONG_OBJECT_TYPE then
            debugPrint
              ("STORAGE-CHECK: directory accepted as file handle" & LF);
            return False;
         end if;

         msg := Open_Directory_Request (grantRef, directoryPath'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK then
            debugPrint ("STORAGE-CHECK: directory open failed" & LF);
            return False;
         end if;
         directory := Directory_Handle (msg.words (0));

         loop
            msg := Read_Directory_Page_Request (directory, grantRef);
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_OK then
               debugPrint ("STORAGE-CHECK: directory page failed" & LF);
               return False;
            end if;

            declare
               pageAddress : constant System.Address :=
                 To_Address (Integer_Address (aligned));
               header : Directory_Page_Header
                 with Import, Address => pageAddress;
               entries : Directory_Entries
                 with Import,
                      Address => pageAddress + DIRECTORY_PAGE_HEADER_BYTES;
            begin
               if header.version /= PROTOCOL_VERSION or else
                 header.headerBytes /= DIRECTORY_PAGE_HEADER_BYTES or else
                 header.entryBytes /= DIRECTORY_ENTRY_BYTES or else
                 header.entryCount > MAXIMUM_DIRECTORY_PAGE_ENTRIES
               then
                  debugPrint ("STORAGE-CHECK: invalid directory page" & LF);
                  return False;
               end if;

               if header.entryCount > 0 then
                  for index in 0 .. Natural (header.entryCount) - 1 loop
                     if entries (index).nameLength >
                       MAXIMUM_DIRECTORY_NAME_BYTES
                     then
                        return False;
                     end if;
                     foundConfig := foundConfig or else
                       entryEquals (entries (index), "config.dat");
                     foundCreated := foundCreated or else
                       entryEquals (entries (index), "cubit-new.dat");
                  end loop;
               end if;

               exit when (header.flags and DIRECTORY_PAGE_END) /= 0;
            end;
         end loop;

         msg := Close_Directory_Request (directory);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         return msg.tag.label = REPLY_OK and foundConfig and foundCreated;
      end directoryContainsExpectedFiles;

      function exerciseDirectoryNavigation return Boolean is
         root, child, reopened : Directory_Handle;
         firstCursor : Unsigned_64;
         header : Directory_Page_Header
           with Import, Address => To_Address (Integer_Address (aligned));

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
            msg.tag := capCall (CAP_SLOT_FS, msg);
            return msg.tag.label /= REPLY_OK;
         end Reject_Name;
      begin
         Put_Name ("@nvme:0/");
         msg := Open_Directory_Request (grantRef, 8);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         root := Directory_Handle (msg.words (0));
         msg := Read_Directory_Page_Request (root, grantRef);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         firstCursor := header.nextCursor;
         msg := Rewind_Directory_Request (root);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         msg := Read_Directory_Page_Request (root, grantRef);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK or else header.nextCursor /= firstCursor then
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
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_ERR then
            return False;
         end if;
         msg := Child_Request (root, "lost+found");
         msg.words (1) := 256;
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_ERR then
            return False;
         end if;
         msg := Child_Request (root, "lost+found");
         msg.words (3) := staleGeneration;
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_ACCESS_DENIED then
            return False;
         end if;
         msg := Read_Request (File_Handle (root), grantRef, 1);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label = REPLY_OK then
            return False;
         end if;

         --  Repeated visits must release slots and reject stale generations.
         for visit in 1 .. 64 loop
            msg := Child_Request (root, "lost+found");
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            child := Directory_Handle (msg.words (0));
            msg := Read_Directory_Page_Request (child, grantRef);
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            msg := Close_Directory_Request (child);
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            msg := Child_Request (root, "lost+found");
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            reopened := Directory_Handle (msg.words (0));
            if child = reopened then
               return False;
            end if;
            msg := Rewind_Directory_Request (child);
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_WRONG_OBJECT_TYPE then
               return False;
            end if;
            msg := Child_Request (child, "lost+found");
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_WRONG_OBJECT_TYPE then
               return False;
            end if;
            msg := Close_Directory_Request (reopened);
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
         end loop;
         msg := Close_Directory_Request (root);
         msg.tag := capCall (CAP_SLOT_FS, msg);
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
         msg.tag := capCall (CAP_SLOT_FS, msg);
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
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_MALFORMED_FILESYSTEM then
            debugPrint ("STORAGE-CHECK: malformed child lookup misreported" & LF);
            return False;
         end if;

         msg := Read_Directory_Page_Request (directory, grantRef);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         closeMessage := Close_Directory_Request (directory);
         closeMessage.tag := capCall (CAP_SLOT_FS, closeMessage);
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
               msg.tag := capCall (CAP_SLOT_FS, msg);
               if msg.tag.label /= expected then
                  debugPrint ("STORAGE-CHECK: metadata rename misreported" & LF);
               end if;
            end Check_Metadata_Rename;
         begin
            Check_Metadata_Rename (corruptPath, REPLY_MALFORMED_FILESYSTEM);
            if msg.tag.label /= REPLY_MALFORMED_FILESYSTEM then
               return False;
            end if;
            Check_Metadata_Rename
              ("@nvme:0/indexed-dir", REPLY_FILE_RANGE_UNSUPPORTED);
            if msg.tag.label /= REPLY_FILE_RANGE_UNSUPPORTED then
               return False;
            end if;
         end;
         debugPrint ("MALFORMED-DIRECTORY-CHECK: PASS" & LF);
         return True;
      end rejectsMalformedDirectory;

      function exerciseRename return Boolean is
         renamed : constant String := "@nvme:0/cubit-renamed-longer.dat";
         nested : constant String := "@nvme:0/lost+found/rename-before.dat";
         nestedAfter : constant String := "@nvme:0/lost+found/rename-after.dat";

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
            msg.tag := capCall (CAP_SLOT_FS, msg);
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
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_OK then
               return False;
            end if;
            file := File_Handle (msg.words (0));
            msg := Read_Request (file, grantRef, PAYLOAD'Length);
            msg.tag := capCall (CAP_SLOT_FS, msg);
            good := msg.tag.label = REPLY_OK and then
              msg.words (0) = PAYLOAD'Length and then bytes = PAYLOAD;
            msg := Close_Request (file);
            msg.tag := capCall (CAP_SLOT_FS, msg);
            return good and then msg.tag.label = REPLY_OK;
         end Has_Payload;
      begin
         Put (CREATE_PATH);
         msg := Open_Request
           (grantRef, CREATE_PATH'Length, OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_ALREADY_EXISTS or else not Has_Payload (CREATE_PATH) then
            debugPrint ("STORAGE-CHECK: exclusive create reused existing file" & LF);
            return False;
         end if;
         if not Rename_Is (CREATE_PATH, PATH, REPLY_ALREADY_EXISTS) or else
           not Has_Payload (CREATE_PATH) or else not Has_Payload (PATH) or else
           not Rename_Is (CREATE_PATH, CREATE_PATH, REPLY_OK) or else
           not Rename_Is (CREATE_PATH, renamed, REPLY_OK) or else
           not Has_Payload (renamed) or else
           not Rename_Is (CREATE_PATH, renamed, REPLY_NOT_FOUND) or else
           not Rename_Is (renamed, nested, REPLY_FILE_RANGE_UNSUPPORTED) or else
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
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         handle := File_Handle (msg.words (0));
         Put (PAYLOAD);
         msg := Write_Request (handle, grantRef, PAYLOAD'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK or else
           not Rename_Is (nested, nestedAfter, REPLY_OK)
         then
            return False;
         end if;
         msg := Seek_Request (handle, 0, From_Start);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK then
            return False;
         end if;
         msg := Read_Request (handle, grantRef, PAYLOAD'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         declare
            bytes : String (PAYLOAD'Range)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            if msg.tag.label /= REPLY_OK or else bytes /= PAYLOAD then
               return False;
            end if;
         end;
         msg := Close_Request (handle);
         msg.tag := capCall (CAP_SLOT_FS, msg);
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
      staleTag := capCall (CAP_SLOT_FS, msg);
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
      staleTag := capCall (CAP_SLOT_FS, msg);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: stale grant accepted by FS" & LF);
         return False;
      end if;

      --  Unknown flags must not silently alter the authority installed on a
      --  returned handle.
      msg := Open_Request (grantRef, PATH'Length, Open_Options (4));
      staleTag := capCall (CAP_SLOT_FS, msg);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: invalid open options accepted" & LF);
         return False;
      end if;

      --  Exercise the distinct read-write mode against a sparse scratch file.
      --  Its first write must allocate outside a full block group in the
      --  headless fixture.
      msg := Open_Request
        (grantRef, PATH'Length, OPEN_READ_WRITE);
      msg.tag := capCall (CAP_SLOT_FS, msg);
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
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint
           ("STORAGE-CHECK: write failed label=" &
            Unsigned_32'Image (msg.tag.label) & " count=" &
            Unsigned_64'Image (msg.words (0)) & LF);
         return False;
      end if;

      --  A flush must acknowledge the real NVMe barrier, not close() or RAM.
      msg := Flush_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK or else msg.tag.length /= 1 or else
        msg.words (0) /= 0
      then
         debugPrint ("STORAGE-FLUSH-CHECK: write flush failed" & LF);
         return False;
      end if;
      msg := Flush_Request (handle);
      msg.tag.length := 0;
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_ERR then
         debugPrint ("STORAGE-FLUSH-CHECK: malformed request accepted" & LF);
         return False;
      end if;

      --  The current writer deliberately supports direct and single-indirect
      --  blocks only.  A range it cannot represent must be reported as such,
      --  never as a successful zero-byte write.
      msg := Seek_Request (handle, 16#0100_0000#, From_Start);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: large seek failed" & LF);
         return False;
      end if;

      msg := Write_Request (handle, grantRef, 1);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_FILE_RANGE_UNSUPPORTED or else
         msg.words (0) /= 0
      then
         debugPrint
           ("STORAGE-CHECK: unsupported write range was not explicit" & LF);
         return False;
      end if;

      msg := Seek_Request (handle, 0, From_Start);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: read-write seek failed" & LF);
         return False;
      end if;

      msg := Read_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg);
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
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: close failed" & LF);
         return False;
      end if;

      msg := Flush_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_ERR then
         debugPrint ("STORAGE-FLUSH-CHECK: stale handle accepted" & LF);
         return False;
      end if;

      --  The just-closed handle must remain invalid even though the next open
      --  is likely to reuse the same table slot.
      msg := Read_Request (handle, grantRef, 1);
      staleTag := capCall (CAP_SLOT_FS, msg);
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
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: reopen failed" & LF);
         return False;
      end if;
      handle := File_Handle (msg.words (0));

      msg := Flush_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_ACCESS_DENIED then
         debugPrint ("STORAGE-FLUSH-CHECK: read-only handle accepted" & LF);
         return False;
      end if;
      debugPrint ("STORAGE-FLUSH-CHECK: PASS" & LF);

      --  One page was granted.  The service must not trust the protocol's
      --  requested count as if the entire 16 MiB aperture slot were mapped.
      msg := Read_Request (handle, grantRef, PAGE_SIZE + 1);
      staleTag := capCall (CAP_SLOT_FS, msg);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: oversized grant range accepted" & LF);
         return False;
      end if;

      msg := Read_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg);
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
      msg.tag := capCall (CAP_SLOT_FS, msg);
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
      msg.tag := capCall (CAP_SLOT_FS, msg);
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
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint ("STORAGE-CHECK: created-file write failed" & LF);
         return False;
      end if;

      msg := Seek_Request (handle, 0, From_Start);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         return False;
      end if;

      msg := Read_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg);
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
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         return False;
      end if;

      return directoryContainsExpectedFiles and then
        exerciseDirectoryNavigation and then rejectsMalformedDirectory and then
        exerciseRename;
   end exerciseStorage;

begin
   raw := syscall (SYSCALL_SBRK, 2 * PAGE_SIZE);
   if raw = Unsigned_64'Last then
      debugPrint ("STORAGE-CHECK: buffer allocation failed" & LF);
      return;
   end if;

   aligned := (raw + PAGE_SIZE - 1) and not (PAGE_SIZE - 1);

   if exerciseGrantReferences then
      debugPrint ("GRANT-REFERENCE-CHECK: PASS" & LF);
   else
      debugPrint ("GRANT-REFERENCE-CHECK: FAIL" & LF);
      return;
   end if;

   -- Reuse one backing frame and the same grant slots beyond the pin-count
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

   if exerciseStorage and then exercisePositioned and then exerciseCoherence then
      debugPrint ("STORAGE-CHECK: PASS" & LF);
   else
      debugPrint ("STORAGE-CHECK: FAIL" & LF);
   end if;

   CuBit.Memory_Grants.Revoke (grantRef, grantOk);
end main;
