with Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with Native_Storage;
with Storage_Channel;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Filesystems; use CuBit.Filesystems;

-- Actual FFI implementation, modeled submit/wait/grants. No native syscall or
-- Rust pointer-validity proof: callers still own valid C descriptor/data arrays.
procedure Native_Bridge_Tests is
   package Grants renames CuBit.Memory_Grants;
   type Part is record
      Data : System.Address;
      Length : Unsigned_64;
   end record with Convention => C;
   type Parts is array (Positive range <>) of Part with Convention => C;
   Header : String (1 .. 24) := [others => 'H'];
   Payload : String (1 .. Storage_Channel.Transfer_Bytes - 24) := [others => 'P'];
   Vector : Parts (1 .. 2) := [(Header'Address, Header'Length), (Payload'Address, Payload'Length)];
   Value : aliased Unsigned_64 := 0;
   Code : Unsigned_32;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "native bridge check" & Checks'Image; end if;
   end Check;
   procedure Denied (Count : Unsigned_64 := 2; Position : Unsigned_64 := 0) is
      Before : constant Natural := Submissions;
      Wait_Before : constant Natural := Waits;
   begin
      Value := Unsigned_64'Last;
      Code := Native_Storage.Write_Vector (23, Position, Vector'Address, Count, Value'Access);
      Check (Code = REPLY_ERR and Value = 0);
      Check (Submissions = Before and Waits = Wait_Before);
   end Denied;
begin
   Grants.Expected_Pages := Storage_Channel.Transfer_Bytes / 4096;
   Check (Native_Storage.Transfer_Capacity = Storage_Channel.Transfer_Bytes);
   Check (Native_Storage.Initialize (64) = REPLY_ERR);
   Check (Native_Storage.Initialize (12) = REPLY_OK);
   Denied (0);
   Denied (Storage_Channel.Transfer_Bytes + 1);
   Denied (Position => Unsigned_64'Last);
   Vector (1).Data := System.Null_Address; Denied;
   Vector (1) := (Header'Address, 0); Denied;
   -- Total overflow must reject before dereferencing the undersized header.
   Vector (1) := (Header'Address, Storage_Channel.Transfer_Bytes); Denied;
   Vector (1) := (Header'Address, Unsigned_64'Last); Denied;
   Vector (1) := (Header'Address, Header'Length);
   Wait_Reply := ((REPLY_OK, 1, 0, 0), 0, [0 => Storage_Channel.Transfer_Bytes, others => 0]);
   Code := Native_Storage.Write_Vector (23, 17, Vector'Address, 2, Value'Access);
   Check (Code = REPLY_OK and Value = Storage_Channel.Transfer_Bytes);
   Check (Submissions = 1 and Waits = 1 and Last_Request.words (3) = 17);
   declare
      Loan : String (1 .. Storage_Channel.Transfer_Bytes) with Import, Address => Grants.Mapping;
      Output : String (1 .. Storage_Channel.Transfer_Bytes) := [others => '?'];
   begin
      Check (Loan (1 .. 24) = Header and Loan (25 .. Loan'Last) = Payload);
      Header := [others => 'X']; Payload := [others => 'Y'];
      Check ((for all C of Loan (1 .. 24) => C = 'H') and
             (for all C of Loan (25 .. Loan'Last) => C = 'P'));
      Code := Native_Storage.Execute
        (Storage_Channel.Operation'Enum_Rep (Storage_Channel.Read_Data), 23, 0,
         Output'Address, Output'Length, Value'Access);
      Check (Code = REPLY_OK and Value = Output'Length and Output = Loan);
      Wait_Reply.words (0) := Header'Length;
      Code := Native_Storage.Execute
        (Storage_Channel.Operation'Enum_Rep (Storage_Channel.Write_Data), 23, 0,
         Header'Address, Header'Length, Value'Access);
      Check (Code = REPLY_OK and Value = Header'Length and Loan (1 .. 24) = Header);
   end;
   -- A malformed oversized completion poisons the channel and subsequent
   -- vectors cannot touch the loan or submit another request.
   Wait_Reply.words (0) := Storage_Channel.Transfer_Bytes + 1;
   Code := Native_Storage.Write_Vector (23, 0, Vector'Address, 2, Value'Access);
   Check (Code = REPLY_IO_ERROR and Value = 0);
   declare
      Before : constant Natural := Submissions;
   begin
      Code := Native_Storage.Write_Vector (23, 0, Vector'Address, 2, Value'Access);
      Check (Code = REPLY_RECOVERY_REQUIRED and Submissions = Before);
   end;
   Check (Native_Storage.Shutdown = REPLY_OK);
   Ada.Text_IO.Put_Line ("NATIVE-STORAGE-BRIDGE: PASS" & Checks'Image & " checks (modeled syscalls)");
end Native_Bridge_Tests;
