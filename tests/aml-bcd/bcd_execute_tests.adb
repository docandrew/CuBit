with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
procedure BCD_Execute_Tests is
   use type Integer_Value;
   Args : Arguments := [others => 0];
   R : Execution_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "BCD check" & Checks'Image; end if;
   end Check;
   Code : constant Bytes := [16#A4#, 16#5B#, 16#28#, 16#0B#, 16#34#, 16#12#, 0];
begin
   for W in Integer_Width loop
      R := Run (Code, W, Args, 0, 10);
      Check (R.Status = Returned and then R.Value = 1234 and then R.Origin = Ordinary_Integer);
      for Fuel in 0 .. 6 loop
         R := Run (Code, W, Args, 0, Fuel);
         Check (R.Charged <= Fuel);
         Check (if Fuel < 3 then R.Status = Budget_Exceeded else R.Status = Returned and then R.Value = 1234);
      end loop;
      for N in 1 .. Code'Length - 1 loop
         R := Run (Code (1 .. N), W, Args, 0, 20);
         Check (R.Status = Truncated);
      end loop;
      R := Run ([16#5B#,16#29#,16#0A#,12,16#60#,16#A4#,16#60#],W,Args,0,20);
      Check (R.Status = Returned and then R.Value = 16#12#);
      R := Run ([16#A4#,16#5B#,16#28#,16#0A#,16#FA#,0],W,Args,0,20);
      Check (R.Status = Numeric_Overflow);
      Args (0) := (if W = Bits_32 then 100_000_000 else 10_000_000_000_000_000);
      R := Run ([16#A4#,16#5B#,16#29#,16#68#,0],W,Args,1,20);
      Check (R.Status = Numeric_Overflow);
      R := Run ([16#A4#,16#5B#,16#28#,16#68#,0],W,Args,0,20);
      Check (R.Status = Missing_Argument);
      R := Run ([16#A4#,16#5B#,16#28#,16#60#,0],W,Args,0,20);
      Check (R.Status = Uninitialized);
      declare
         High : constant Bytes (Positive'Last - Code'Length + 1 .. Positive'Last) := Code;
      begin
         R := Run (High,W,Args,0,20);
         Check (R.Status = Returned and then R.Value = 1234);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("BCD executor checks" & Checks'Image);
end BCD_Execute_Tests;
