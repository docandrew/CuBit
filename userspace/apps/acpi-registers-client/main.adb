------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A program that declares the role-35 "acpi-registers" service without the
--  startup plan's (acpi-registers approve-declared): procmgr must give it no
--  endpoint (tests/headless, acpi-hardware). The service is optional in its
--  manifest so the program runs and can report what it got.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles.Startup;
with Startup;

procedure Main is
   use ASCII;
   Registers : constant CuBit.Handles.Startup.Optional_Endpoint := Startup.Registers;
begin
   if Registers.Present then
      debugPrint ("acpi-registers-client: no registers endpoint without approval FAIL" & LF);
      debugPrint ("TEST: FAIL acpi-registers-client" & LF);
   else
      debugPrint ("acpi-registers-client: no registers endpoint without approval PASS" & LF);
      debugPrint ("TEST: PASS acpi-registers-client" & LF);
   end if;
end Main;
