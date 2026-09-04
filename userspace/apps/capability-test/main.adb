------------------------------------------------------------------------------
--  CuBit capability non-amplification regression app
--
--  This deliberately authorityless process attempts the operations that once
--  made the self CAP_PROCESS an ambient privilege-escalation path.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;

with CuBit.Messages; use CuBit.Messages;

procedure Main is
   use ASCII;

   CAP_NULL     : constant Unsigned_64 := 0;
   CAP_ENDPOINT : constant Unsigned_64 := 1;
   RIGHT_RW     : constant Unsigned_64 := 3;
   TEST_SLOT    : constant Unsigned_64 := 32;
   KBD_SLOT     : constant Unsigned_64 := 13;
   MOUSE_SLOT   : constant Unsigned_64 := 16;
   PROCESS_SLOT : constant Unsigned_64 := 5;
   ERROR_RESULT : constant Unsigned_64 := Unsigned_64'Last;

   type Inspection_Record is array (Natural range 0 .. 5) of Unsigned_64;
   Inspection : aliased Inspection_Record := (others => 0);

   function Address_Number is new Ada.Unchecked_Conversion
     (System.Address, Unsigned_64);

   PID    : Unsigned_64;
   Result : Unsigned_64;
   Passed : Boolean := True;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("capability-test: " & Name & " PASS" & LF);
      else
         debugPrint ("capability-test: " & Name & " FAIL" & LF);
         Passed := False;
      end if;
   end Check;

   procedure Check_Empty_Slot (Slot : Unsigned_64; Name : String) is
   begin
      Inspection := (others => 0);
      Result := syscall
        (SYSCALL_INSPECT_CAPABILITY, PID, Slot,
         Address_Number (Inspection'Address));
      Check
        (Result = 1 and then Inspection (0) = CAP_NULL,
         Name);
   end Check_Empty_Slot;
begin
   PID := syscall (SYSCALL_GETPID);
   Check (PID > 0 and then PID /= ERROR_RESULT, "getpid");

   Inspection := (others => 0);
   Result := syscall
     (SYSCALL_INSPECT_CAPABILITY, PID, CAP_SLOT_FS,
      Address_Number (Inspection'Address));
   Check
     (Result = 1 and then Inspection (0) = CAP_NULL,
      "no ambient filesystem");

   Inspection := (others => 0);
   Result := syscall
     (SYSCALL_INSPECT_CAPABILITY, PID, CAP_SLOT_SELF_PROC,
      Address_Number (Inspection'Address));
   Check
     (Result = 1 and then Inspection (1) = RIGHT_RW,
      "self process rights attenuated");

   Check_Empty_Slot (KBD_SLOT, "no ambient keyboard");
   Check_Empty_Slot (MOUSE_SLOT, "no ambient mouse");
   Check_Empty_Slot (PROCESS_SLOT, "no ambient process management");

   Result := syscall
     (SYSCALL_POLICY_MINT_CAPABILITY, PID, CAP_ENDPOINT, PID, 0, RIGHT_RW,
      TEST_SLOT);
   Check (Result = ERROR_RESULT, "self mint denied");

   Inspection := (others => 0);
   Result := syscall
     (SYSCALL_INSPECT_CAPABILITY, PID, TEST_SLOT,
      Address_Number (Inspection'Address));
   Check
     (Result = 1 and then Inspection (0) = CAP_NULL,
      "mint denial leaves slot empty");

   --  A null ELF is never dereferenced because authority admission precedes
   --  ELF validation. An authorityless process cannot spawn another process.
   Result := syscall (SYSCALL_SPAWN);
   Check (Result = ERROR_RESULT, "ambient spawn denied");

   if Passed then
      debugPrint ("capability-test: all tests passed" & LF);
   else
      debugPrint ("capability-test: test failure" & LF);
   end if;

   loop
      Result := syscall (SYSCALL_SLEEP, 1_000);
   end loop;
end Main;
