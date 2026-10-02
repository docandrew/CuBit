with Ada.Text_IO;
with AML_Decode;
with AML_Execute; use AML_Execute;
procedure Execute_Tests is
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Integer_Width;
   Args : Arguments := [others => 0];
   R : Execution_Result;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for W in AML_Decode.Integer_Width loop
      for A in 0 .. 6 loop
         Args (A) := 16#1234_5678_9ABC_DEF0#;
         R := Run ([16#A4#, AML_Decode.Byte (16#68# + A)], W, Args, A + 1, 2);
         Check (R.Status = Returned and then R.Value =
           Args (A));
         R := Run ([16#A4#, AML_Decode.Byte (16#68# + A)], W, Args, A, 2);
         Check (R.Status = Missing_Argument);
      end loop;
   end loop;
   for L in 0 .. 7 loop
      for B in 0 .. 255 loop
         R := Run ([16#70#,16#0A#,AML_Decode.Byte (B),AML_Decode.Byte (16#60# + L),
                    16#A4#,AML_Decode.Byte (16#60# + L)],
                    AML_Decode.Bits_64, Args, 0, 4);
         Check (R.Status = Returned and then R.Value = AML_Decode.Integer_Value (B)
                and then R.Charged = 4);
      end loop;
      R := Run ([16#A4#,AML_Decode.Byte (16#60# + L)], AML_Decode.Bits_64, Args, 0, 10);
      Check (R.Status = Uninitialized);
   end loop;
   for Fuel in 0 .. 10 loop
      R := Run ([16#A3#,16#70#,1,16#60#,16#A4#,16#60#],
                AML_Decode.Bits_64, Args, 0, Fuel);
      Check (R.Charged <= Fuel and then
        (if Fuel < 5 then R.Status = Budget_Exceeded else R.Status = Returned and then R.Value = 1));
   end loop;
   R := Run ([16#A4#], AML_Decode.Bits_64, Args, 0, 10);
   Check (R.Status = Truncated);
   R := Run ([16#5B#,16#80#], AML_Decode.Bits_64, Args, 0, 10);
   Check (R.Status = Unsupported);
   R := Run ([16#A4#,1,16#5B#,16#80#], AML_Decode.Bits_64, Args, 0, 10);
   Check (R.Status = Returned and then R.Value = 1);
   R := Run ([Positive'Last - 1 => 16#A4#, Positive'Last => 1],
             AML_Decode.Bits_64, Args, 0, 2);
   Check (R.Status = Returned and then R.Value = 1);
   Ada.Text_IO.Put_Line ("AML-EXECUTE-CHECK: PASS" & Checks'Image);
end Execute_Tests;
