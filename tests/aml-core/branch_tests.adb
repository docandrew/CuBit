with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
procedure Branch_Tests is
   use type Integer_Value;
   Args : Arguments := [others => 0];
   R : Execution_Result;
   Checks : Natural := 0;
   Code : Bytes (1 .. 1024) := [others => 0];
   Used : Natural := 2;
   Extent : Natural;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for Predicate in 0 .. 255 loop
      Args (0) := Integer_Value (Predicate);
      R := Run ([16#A0#,4,16#68#,16#A4#,1,16#A1#,3,16#A4#,0],
                Bits_64, Args, 1, 20);
      Check (R.Status = Returned and then R.Value = (if Predicate = 0 then 0 else 1));
      R := Run ([16#A0#,5,16#68#,16#70#,1,16#60#,
                 16#A1#,4,16#70#,0,16#60#,16#A4#,16#60#], Bits_64, Args, 1, 20);
      Check (R.Status = Returned and then R.Value = (if Predicate = 0 then 0 else 1));
   end loop;
   Args (0) := 16#8000_0000_0000_0000#;
   for W in Integer_Width loop
      R := Run ([16#A0#,4,16#68#,16#A4#,1,16#A1#,3,16#A4#,0],
                W, Args, 1, 20);
      Check (R.Status = Returned and then
             R.Value = (if W = Bits_32 then 0 else 1));
   end loop;
   Code (1 .. 2) := [16#A4#,1];
   for Depth in 1 .. 65 loop
      for I in reverse 1 .. Used loop Code (I + 4) := Code (I); end loop;
      Extent := Used + 3;
      Code (1 .. 4) := [16#A0#,Byte (16#40# + Extent mod 16), Byte (Extent / 16),1];
      Used := Used + 4;
      R := Run (Code (1 .. Used), Bits_64, Args, 0, 1000);
      Check ((if Depth <= 64 then R.Status = Returned and then R.Value = 1
              else R.Status = Block_Limit));
   end loop;
   R := Run ([16#A0#,2,16#0A#,1,16#A4#,1], Bits_64, Args, 0, 20);
   Check (R.Status = Truncated); -- predicate cannot consume byte outside package
   R := Run ([16#A0#,4,0,16#5B#,16#80#,16#A4#,1], Bits_64, Args, 0, 20);
   Check (R.Status = Returned and then R.Value = 1); -- untaken unsupported body
   R := Run ([16#A0#,4,1,16#5B#,16#80#,16#A4#,1], Bits_64, Args, 0, 20);
   Check (R.Status = Unsupported);
   R := Run ([16#A1#,1], Bits_64, Args, 0, 20);
   Check (R.Status = Unsupported);
   for Fuel in 0 .. 6 loop
      R := Run ([16#A0#,4,1,16#A4#,1], Bits_64, Args, 0, Fuel);
      Check ((if Fuel < 4 then R.Status = Budget_Exceeded else R.Status = Returned));
   end loop;
   Ada.Text_IO.Put_Line ("AML-BRANCH-CHECK: PASS" & Checks'Image);
end Branch_Tests;
