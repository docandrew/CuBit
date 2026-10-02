with Native_GPU_Query; use Native_GPU_Query;
with CuBit.Messages; use CuBit.Messages;
with Interfaces; use Interfaces;
procedure Bridge_Tests is
   Output : aliased Reply_Words := [others => Unsigned_64'Last];
   Result : Unsigned_32;
begin
   pragma Assert (Execute (63, 0, null) = 1 and Calls = 0);
   pragma Assert (Execute (64, 0, Output'Access) = 1 and Calls = 0);
   pragma Assert (Output = [0, 0, 0, 0]);
   pragma Assert (Execute (Unsigned_64'Last, 0, Output'Access) = 1 and Calls = 0);
   pragma Assert (Execute (63, 3, Output'Access) = 1 and Calls = 0);
   for Mode in 0 .. 2 loop
      Fault := Mode;
      Output := [others => Unsigned_64'Last];
      Result := Execute (63, 1, Output'Access);
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
         pragma Assert (Execute (63, 0, Output'Access) = 1);
         pragma Assert (Output = [0, 0, 0, 0]);
      end loop;
   end loop;
   pragma Assert (Calls = 131);
   Budget_Mode := True;
   pragma Assert (Budget (63, null) = 1 and Calls = 131);
   pragma Assert (Budget (64, Output'Access) = 1 and Calls = 131);
   pragma Assert (Output = [0,0,0,0]);
   for Mode in 0 .. 2 loop
      Fault := Mode;
      Result := Budget (63, Output'Access);
      pragma Assert ((Result = 0) = (Mode = 0));
      pragma Assert (Output = (if Mode = 0 then [0,33554432,4096,15] else [0,0,0,0]));
   end loop;
   Fault := 3;
   for Returned in Boolean loop
      Corrupt_Return := Returned;
      for Bit in 0 .. 63 loop
         Envelope_Bit := Bit;
         pragma Assert (Budget (63, Output'Access) = 1);
         pragma Assert (Output = [0,0,0,0]);
      end loop;
   end loop;
   pragma Assert (Calls = 262);
end Bridge_Tests;
