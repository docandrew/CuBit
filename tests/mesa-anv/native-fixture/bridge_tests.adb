with Native_GPU_Query; use Native_GPU_Query;
with CuBit.Messages; use CuBit.Messages;
with Interfaces; use Interfaces;
with CuBit.Handles.C_ABI; use CuBit.Handles.C_ABI;
procedure Bridge_Tests is
   Service : constant C_Endpoint := To_C (GPU);
   Output : aliased Reply_Words := [others => Unsigned_64'Last];
   Result : Unsigned_32;
begin
   pragma Assert (Execute (Service, 0, null) = 1 and Calls = 0);
   pragma Assert (Execute (No_C_Endpoint, 0, Output'Access) = 1 and Calls = 0);
   pragma Assert (Output = [0, 0, 0, 0]);
   pragma Assert (Execute (Service, 5, Output'Access) = 1 and Calls = 0);
   for Mode in 0 .. 2 loop
      Fault := Mode;
      Output := [others => Unsigned_64'Last];
      Result := Execute (Service, 1, Output'Access);
      pragma Assert (Calls = Mode + 1);
      pragma Assert ((Result = 0) = (Mode = 0));
      pragma Assert (Output = (if Mode = 0 then [2, 1, 0, 0] else [0, 0, 0, 0]));
   end loop;
   Fault := 3;
   for Returned in Boolean loop
      Corrupt_Return := Returned;
      for Bit in 0 .. 63 loop
         Envelope_Bit := Bit;
         Output := [others => Unsigned_64'Last];
         pragma Assert (Execute (Service, 0, Output'Access) = 1);
         pragma Assert (Output = [0, 0, 0, 0]);
      end loop;
   end loop;
   pragma Assert (Calls = 131);
   Budget_Mode := True;
   pragma Assert (Budget (Service, null) = 1 and Calls = 131);
   pragma Assert (Budget (No_C_Endpoint, Output'Access) = 1 and Calls = 131);
   pragma Assert (Output = [0,0,0,0]);
   for Mode in 0 .. 2 loop
      Fault := Mode;
      Result := Budget (Service, Output'Access);
      pragma Assert ((Result = 0) = (Mode = 0));
      pragma Assert (Output = (if Mode = 0 then [0,33554432,4096,15] else [0,0,0,0]));
   end loop;
   Fault := 3;
   for Returned in Boolean loop
      Corrupt_Return := Returned;
      for Bit in 0 .. 63 loop
         Envelope_Bit := Bit;
         pragma Assert (Budget (Service, Output'Access) = 1);
         pragma Assert (Output = [0,0,0,0]);
      end loop;
   end loop;
   pragma Assert (Calls = 262);
   Budget_Mode := False;
   Fault := 0;
   for Selector in 3 .. 4 loop
      pragma Assert (Execute (Service, Unsigned_64 (Selector), Output'Access) = 0);
      pragma Assert (Output = [2, 1, 0, 0]);
   end loop;
   pragma Assert (Calls = 264);
end Bridge_Tests;
