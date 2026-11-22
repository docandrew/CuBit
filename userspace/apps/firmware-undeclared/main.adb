------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Firmware snapshot regression: without a declared reader the program has
--  no firmware snapshot handle, and no word it might pass as one (no
--  handle, its own process, a forged word) is answered as a snapshot: the
--  Call reads Not_Authorized. (Its own endpoint is not tried: a Call on an
--  endpoint is a message to it, here to itself.)
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Kernel_ABI;
with System.Storage_Elements;
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

   --  The Call a firmware snapshot reader makes, on a raw handle word:
   --  answered as a snapshot only when the word names one (the reply's
   --  label an Outcome word). Anything else, refused or answered otherwise,
   --  is Not_Authorized to the runtime.
   procedure Try (Handle : Unsigned_64; Name : String) is
      Msg : aliased Message := NULL_MESSAGE;
      Result : Unsigned_64;
   begin
      Msg.tag.label := ABI.Operation'Enum_Rep (ABI.Catalog_Info);
      Msg.tag.length := ABI.Message_Words;
      Result := syscall (CuBit.Kernel_ABI.Call_Via_Endpoint_Capability, Handle,
                         Unsigned_64 (System.Storage_Elements.To_Integer (Msg'Address)),
                         Wait_Forever);
      if Result = 0 or else Result = Unsigned_64'Last
        or else not ABI.Is_Outcome_Word (Unsigned_64 (Msg.tag.label))
        or else ABI.To_Outcome (Unsigned_64 (Msg.tag.label)) = ABI.Not_Authorized
      then
         debugPrint ("firmware-undeclared: " & Name & " not authorized PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("firmware-undeclared: " & Name & " FAIL" & LF);
      end if;
   end Try;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   Try (0, "no handle");
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
