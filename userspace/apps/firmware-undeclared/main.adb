------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  FIRMWARE_SNAPSHOT regression: without a declared reader the program has
--  no firmware snapshot handle, and the call refuses every word it might
--  pass as one (no handle, its own endpoint, its own process, a forged
--  word) with Not_Authorized, before looking at the request.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Startup_State;
with CuBit.Startup_Tables;
with Firmware_Snapshot_ABI;

procedure Main is
   use ASCII;
   package ABI renames Firmware_Snapshot_ABI;
   package ST renames CuBit.Startup_Tables;
   use type ABI.Outcome;

   Failures : Natural := 0;
   Forged_Generation : constant Unsigned_64 := 2 ** 32;

   procedure Try (Handle : Unsigned_64; Name : String) is
      Op : constant Unsigned_64 := ABI.Operation'Enum_Rep (ABI.Catalog_Info);
      Result : constant Unsigned_64 := syscall (ABI.System_Call_Number, Handle, Op);
   begin
      if ABI.To_Outcome (Result) = ABI.Not_Authorized then
         debugPrint ("firmware-undeclared: " & Name & " not authorized PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("firmware-undeclared: " & Name & " FAIL" & LF);
      end if;
   end Try;
begin
   Try (0, "no handle");
   Try (CuBit.Startup_State.Granted (ST.Self_Text, ST.Endpoint), "own endpoint");
   Try (CuBit.Startup_State.Granted (ST.Self_Process_Text, ST.Process), "own process");
   Try (Forged_Generation + 1, "forged handle word");
   --  No (service "acpi" ...) declared: no ACPI client endpoint either.
   if CuBit.Startup_State.Granted ("acpi", ST.Endpoint) = ST.No_Handle then
      debugPrint ("firmware-undeclared: no acpi endpoint PASS" & LF);
   else
      Failures := Failures + 1;
      debugPrint ("firmware-undeclared: acpi endpoint FAIL" & LF);
   end if;
   if Failures = 0 then
      debugPrint ("TEST: PASS firmware-undeclared" & LF);
   else
      debugPrint ("TEST: FAIL firmware-undeclared" & LF);
   end if;
end Main;
