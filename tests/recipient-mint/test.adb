with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants; use CuBit.Capability_Grants;
procedure Test is
   Target : Recipient;
   Result : Unsigned_64;
begin
   for Kind in Unsigned_64 range 0 .. 12 loop
      Inspection (0) := Kind;
      Target := Capture (4);
      pragma Assert (Valid (Target) = (Kind in 1 | 6 | 10));
   end loop;
   Inspection := [1, 0, 0, 42, 0, 7];
   Target := Capture (4);
   -- A later observation must not silently replace the approved incarnation.
   Inspection (5) := 8;
   Result := Install (Target, 1, 99, 123, 3, 12);
   pragma Assert (Result = 0 and Mint_Calls = 1);
   pragma Assert (Arguments = Words'[7 * 2 ** 32 + 42, 1, 99, 123, 3, 12]);
   Mint_Result := Unsigned_64'Last;
   Result := Install (Target, 1, 99, 123, 3, 12);
   pragma Assert (Result = Unsigned_64'Last and Mint_Calls = 2);
   Result := Install (Target, 1, 99, 123, 32, 12);
   pragma Assert (Result = Unsigned_64'Last and Mint_Calls = 2);
   for Field in 3 .. 5 loop
      if Field /= 4 then
         for Invalid in 0 .. 1 loop
            Inspection := [1, 0, 0, 42, 0, 7];
            Inspection (Field) := (if Invalid = 0 then 0 else 2 ** 32);
            Target := Capture (4);
            pragma Assert (not Valid (Target));
            Result := Install (Target, 1, 99, 123, 3, 12);
            pragma Assert (Result = Unsigned_64'Last and Mint_Calls = 2);
         end loop;
      end if;
   end loop;
   Inspection := [1, 0, 0, 42, 0, 7];
   Inspect_Result := 0;
   pragma Assert (not Valid (Capture (4)));
   Inspect_Result := 1;
   Current_PID := 0;
   pragma Assert (not Valid (Capture (4)));
   Current_PID := Unsigned_64'Last;
   pragma Assert (not Valid (Capture (4)));
   Current_PID := 10;
   Inspection := [1, 0, 0, 42, 0, 7];
   Target := Capture (4);
   pragma Assert (Process_ID (Target) = 42);
   Mint_Result := 0;
   Ada.Text_IO.Put_Line ("recipient mint wrapper PASS (mock transport)");
   Inspection := [1, 0, 0, 42, 0, 7];
   Target := Capture (4);
   Inspection (5) := 8;
   Result := Delegate_Endpoint (Target, 5, 12, 3, 999);
   pragma Assert (Result = 0 and Last_Number = 121);
   pragma Assert (Arguments = Words'[7 * 2 ** 32 + 42, 5, 12, 3, 999, 0]);
   declare
      Before : constant Natural := Mint_Calls;
   begin
      Result := Delegate_Endpoint (Target, 5, 12, 32, 999);
      pragma Assert (Result = Unsigned_64'Last and Mint_Calls = Before);
      Inspection (5) := 0;
      Target := Capture (4);
      Result := Delegate_Endpoint (Target, 5, 12, 3, 999);
      pragma Assert (Result = Unsigned_64'Last and Mint_Calls = Before);
   end;
   Inspection (5) := 7;
   Target := Capture (4);
   Mint_Result := Unsigned_64'Last;
   Result := Delegate_Endpoint (Target, 5, 12, 3, 999);
   pragma Assert (Result = Unsigned_64'Last);
   Ada.Text_IO.Put_Line ("endpoint delegation wrapper PASS (mock transport)");
   declare
      Identity : constant Unsigned_64 := 7 * 2 ** 32 + 42;
      Before : constant Natural := Mint_Calls;
   begin
      for Kind in Unsigned_64 range 0 .. 12 loop
         Inspection := [Kind, 1, 0, 42, 0, 7];
         pragma Assert (Endpoint_Matches (4, Identity) = (Kind = 1));
      end loop;
      for Rights in Unsigned_64 range 0 .. 31 loop
         Inspection := [1, Rights, 0, 42, 0, 7];
         pragma Assert (Endpoint_Matches (4, Identity) = ((Rights and 1) /= 0));
      end loop;
      Inspection := [1, 1, 0, 42, 0, 7];
      pragma Assert (not Endpoint_Matches (4, Identity + 2 ** 32));
      pragma Assert (not Endpoint_Matches (4, Identity + 1));
      pragma Assert (not Endpoint_Matches (4, 0));
      pragma Assert (not Endpoint_Matches (4, 42));
      pragma Assert (not Endpoint_Matches (4, 7 * 2 ** 32));
      Inspect_Result := 0;
      pragma Assert (not Endpoint_Matches (4, Identity));
      Inspect_Result := 1;
      Current_PID := 0;
      pragma Assert (not Endpoint_Matches (4, Identity));
      Current_PID := 10;
      pragma Assert (Mint_Calls = Before);
   end;
   Ada.Text_IO.Put_Line ("recipient endpoint identity/rights checks PASS (mock transport)");
end Test;
