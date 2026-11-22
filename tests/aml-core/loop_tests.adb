with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
procedure Loop_Tests is
   use type Integer_Value;
   Args : Arguments := [others => 0];
   R : Execution_Result;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   Countdown : constant Bytes :=
     [16#70#,16#68#,16#60#,16#A2#,6,16#60#,16#74#,16#60#,1,16#60#,16#A4#,16#60#];
begin
   for N in 0 .. 100 loop
      Args (0) := Integer_Value (N);
      R := Run (Countdown, Bits_64, Args, 1, 6 * N + 6);
      Check (R.Status = Returned and then R.Value = 0 and then R.Charged = 6 * N + 6);
      R := Run (Countdown, Bits_64, Args, 1, 6 * N + 5);
      Check (R.Status = Budget_Exceeded);
   end loop;
   for Fuel in 0 .. 100 loop
      R := Run ([16#A2#,2,1], Bits_64, Args, 0, Fuel);
      Check (R.Status = Budget_Exceeded and then R.Charged = Fuel);
   end loop;
   -- Break exits the loop through a nested conditional, then returns.
   R := Run ([16#A2#,6,1,16#A0#,3,1,16#A5#,16#A4#,1], Bits_64, Args, 0, 20);
   Check (R.Status = Returned and then R.Value = 1);
   -- Inner break must not exit the outer loop; its following Return wins.
   R := Run ([16#A2#,8,1,16#A2#,3,1,16#A5#,16#A4#,1,16#A4#,0], Bits_64, Args, 0, 30);
   Check (R.Status = Returned and then R.Value = 1);
   Args (0) := 10;
   R := Run ([16#70#,16#68#,16#60#,16#A2#,9,16#60#,
              16#74#,16#60#,1,16#60#,16#9F#,16#5B#,16#80#,16#A4#,16#60#],
             Bits_64, Args, 1, 200);
   Check (R.Status = Returned and then R.Value = 0);
   R := Run ([16#A5#], Bits_64, Args, 0, 10);
   Check (R.Status = Invalid_Control);
   R := Run ([16#9F#], Bits_64, Args, 0, 10);
   Check (R.Status = Invalid_Control);
   R := Run ([16#A2#,2,16#0A#,1], Bits_64, Args, 0, 10);
   Check (R.Status = Truncated);
   Ada.Text_IO.Put_Line ("AML-LOOP-CHECK: PASS" & Checks'Image);
end Loop_Tests;
