with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Filesystems;
with CuBit.Memory_Grants;
with Intel_GPU_Firmware_Reader;
package body Intel_GPU_Firmware_File is
   use Interfaces;
   use CuBit.Messages;
   use type System.Address;
   use type CuBit.Filesystems.File_Handle;
   use type Intel_GPU_Firmware_Reader.Read_Status;
   Attempted : Boolean := False;
   Scratch : Intel_GPU_Firmware_Reader.Byte_Array (0 .. 4095)
     with Alignment => 4096;
   Loan : CuBit.Memory_Grants.Grant_Reference;
   procedure Load
     (Slot : CapabilitySlot; Address : out System.Address;
      Bytes : out Unsigned_64; Plan : out Intel_GPU_Firmware.Layout;
      Status : out Load_Status)
   is
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Token : Unsigned_64 := 16#4947_1000#;
      Pending : Boolean := False;
      Failed : Boolean := False;
      Granted, Revoked : Boolean;
      Raw, File_Bytes : Unsigned_64;
      Handle : CuBit.Filesystems.File_Handle := CuBit.Filesystems.INVALID_FILE_HANDLE;
      Request : Message;
      Read_Result : Intel_GPU_Firmware_Reader.Read_Status;
      Candidate : Intel_GPU_Firmware.Layout;
      Metadata_Matches : Boolean := False;
      function Exchange (Msg : in out Message; Length : Unsigned_8) return Boolean is
         Receipt : aliased CompletionEntry;
         Now : Unsigned_64;
         Activity : Activity_Result;
         pragma Unreferenced (Activity);
      begin
         if Failed then return False; end if;
         Now := syscall (SYSCALL_GETTIME);
         if Now < Started or else Now - Started >= 30_000 then
            Failed := True; return False;
         end if;
         Token := Token + 1;
         if not capSubmit (Slot, Msg, Token) then
            Failed := True; return False;
         end if;
         Pending := True;
         loop
            Now := syscall (SYSCALL_GETTIME);
            if Now < Started or else Now - Started >= 30_000 then
               Failed := True; return False;
            end if;
            if Poll_Completion (Receipt'Address) /= 0 then
               --  This phase is the process's sole async producer. Never
               --  mistake an unrelated completion for return of our loan.
               if Receipt.token = Token then
                  Pending := False;
                  if Receipt.status /= COMPLETION_OK then
                     Failed := True; return False;
                  end if;
                  Msg := Receipt.msg;
                  return Msg.tag = (CuBit.Filesystems.REPLY_OK, Length, 0, 0);
               end if;
            end if;
            Activity := Wait_For_Activity_Until
              (if Now < Unsigned_64'Last then Now + 1 else Now);
         end loop;
      end Exchange;
      procedure Read_At
        (Offset : Unsigned_64;
         Destination : out Intel_GPU_Firmware_Reader.Byte_Array;
         Count : out Unsigned_64; Success : out Boolean)
      is
         Msg : Message := CuBit.Filesystems.Read_At_Request
           (Handle, Loan, Unsigned_64 (Destination'Length), Offset);
      begin
         Count := 0;
         Success := Exchange (Msg, 1);
         if not Success then return; end if;
         if Msg.words (1 .. 3) /= [0, 0, 0] or else
           Msg.words (0) > Unsigned_64 (Destination'Length)
         then Success := False; return; end if;
         Count := Msg.words (0);
         for Index in 1 .. Natural (Count) loop
            Destination (Destination'First + Index - 1) := Scratch (Index - 1);
         end loop;
      end Read_At;
      procedure Read_File is new Intel_GPU_Firmware_Reader.Load (Read_At);
   begin
      Address := System.Null_Address; Bytes := 0; Plan := (others => <>);
      Status := Already_Attempted;
      if Attempted then return; end if;
      Attempted := True;
      Raw := syscall (SYSCALL_SBRK, Intel_GPU_Firmware_Reader.Maximum_Blob_Bytes);
      if Raw = Unsigned_64'Last or else Raw = 0 or else
        Raw > Unsigned_64'Last - Intel_GPU_Firmware_Reader.Maximum_Blob_Bytes
      then Status := Allocation_Failed; return; end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (Slot, Scratch'Address, 1, True, Loan, Granted);
      if not Granted then Status := Grant_Failed; return; end if;
      Scratch := [others => 0];
      for Index in Path'Range loop
         Scratch (Index - Path'First) := Character'Pos (Path (Index));
      end loop;
      Request := CuBit.Filesystems.Open_Request (Loan, Path'Length);
      Status := Open_Failed;
      if Exchange (Request, 2) and then Request.words (0) /= 0 and then
        Request.words (2 .. 3) = [0, 0]
      then
         Handle := CuBit.Filesystems.File_Handle (Request.words (0));
         File_Bytes := Request.words (1);
         declare
            Buffer : Intel_GPU_Firmware_Reader.Byte_Array
              (0 .. Intel_GPU_Firmware_Reader.Maximum_Blob_Bytes - 1)
              with Import, Address => To_Address (Integer_Address (Raw));
            Header : Intel_GPU_Firmware.CSS_Header;
         begin
            Read_File (File_Bytes, Buffer, Read_Result, Candidate);
            if Read_Result = Intel_GPU_Firmware_Reader.Loaded then
               for Index in Header'Range loop
                  Header (Index) := Buffer (Index);
               end loop;
               Metadata_Matches := Intel_GPU_Firmware.Matches_Selected_ADLN_GuC
                 (Header, File_Bytes);
            end if;
         end;
         Status := Contents_Rejected;
         if not Failed then
            Request := CuBit.Filesystems.Close_Request (Handle);
            if not Exchange (Request, 1) or else Request.words /= [0, 0, 0, 0] then
               Status := Close_Failed;
            elsif Read_Result = Intel_GPU_Firmware_Reader.Loaded then
               if Metadata_Matches then
                  Status := Loaded;
                  Address := To_Address (Integer_Address (Raw));
                  Bytes := File_Bytes; Plan := Candidate;
               else
                  Status := Metadata_Rejected;
               end if;
            end if;
         end if;
      end if;
      if Failed then Status := Transport_Failed; end if;
      if not Failed and then
        Request.tag = (CuBit.Filesystems.REPLY_ACCESS_DENIED, 1, 0, 0) and then
        Request.words (1 .. 3) = [0, 0, 0] and then
        (Request.words (0) = 0 or else Request.words (0) = Unsigned_64'Last)
      then Status := Access_Denied; end if;
      --  Revocation is not cancellation. Neither the scratch page nor the
      --  private allocation is reclaimed, even if retirement is pending.
      if not Pending then
         CuBit.Memory_Grants.Revoke (Loan, Revoked);
      end if;
   end Load;
end Intel_GPU_Firmware_File;
