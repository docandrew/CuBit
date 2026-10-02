with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants; use CuBit.Capability_Grants;
procedure Recipient_Tests is
   Target : Recipient;
   Result : Unsigned_64;
   Identity : constant Unsigned_64 := 7 * 2 ** 32 + 42;
   Kinds : constant array (1 .. 3) of Unsigned_64 := [1, 6, 10];
   Invalid_Fields : constant array (1 .. 3) of Natural := [0, 3, 5];
begin
   pragma Assert (not Valid (Target) and Incarnation (Target) = 0);
   for Kind of Kinds loop
      Inspection := [Kind, 1, 0, 42, 0, 7];
      Target := Capture (7);
      pragma Assert (Valid (Target) and Process_ID (Target) = 42);
      pragma Assert (Incarnation (Target) = Identity);
      -- Same numeric PID now names another process; no new inspection may
      -- replace the previously captured incarnation during installation.
      Inspection (5) := 8;
      Grant_Result := Unsigned_64'Last;
      Result := Delegate_Endpoint (Target, 31, 4, 3, 123);
      pragma Assert (Result = Unsigned_64'Last);
      pragma Assert (Last_Operation = SYSCALL_POLICY_DELEGATE_ENDPOINT);
      pragma Assert (Last_Arguments = [Identity, 31, 4, 3, 123, 0]);
      pragma Assert (Incarnation (Target) = Identity);
      Result := Install (Target, 1, 99, 123, 3, 4);
      pragma Assert (Result = Unsigned_64'Last);
      pragma Assert (Last_Arguments = [Identity, 1, 99, 123, 3, 4]);
   end loop;
   for Field of Invalid_Fields loop
      Inspection := [1, 1, 0, 42, 0, 7];
      Inspection (Field) := 0;
      Target := Capture (7);
      pragma Assert (not Valid (Target) and Incarnation (Target) = 0);
      Last_Operation := 0;
      Result := Delegate_Endpoint (Target, 31, 4, 3, 123);
      pragma Assert (Result = Unsigned_64'Last and Last_Operation = 0);
   end loop;
   Inspection := [2, 1, 0, 42, 0, 7]; -- reply/thread capability rejected
   Target := Capture (7);
   pragma Assert (not Valid (Target) and Incarnation (Target) = 0);
   Put_Line ("CAPTURED-RECIPIENT: PASS (mock syscalls, not kernel race proof)");
end Recipient_Tests;
