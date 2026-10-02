with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
procedure Division_Tests is
   use type Integer_Value;
   use type Byte;
   Checks : Natural := 0;
   R : Execution_Result;
   Args : constant Arguments := [0 => 19, 1 => 7, others => 0];
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for W in Integer_Width loop
      for Rem_Target in Byte range 16#60# .. 16#67# loop
         for Quot_Target in Byte range 16#60# .. 16#67# loop
            declare
               Code : constant Bytes := [16#78#,16#68#,16#69#,Rem_Target,Quot_Target];
            begin
               R := Run (Code & [16#A4#,Rem_Target], W, Args, 2, 20);
               Check (R.Status = Returned and then R.Value =
                        (if Rem_Target = Quot_Target then 2 else 5));
               R := Run (Code & [16#A4#,Quot_Target], W, Args, 2, 20);
               Check (R.Status = Returned and then R.Value = 2);
               for Fuel in 0 .. R.Charged - 1 loop
                  R := Run (Code & [16#A4#,Quot_Target], W, Args, 2, Fuel);
                  Check (R.Status = Budget_Exceeded);
               end loop;
            end;
         end loop;
      end loop;
      R := Run ([16#A4#,16#72#,16#78#,16#68#,16#69#,16#60#,0,16#60#,0], W, Args, 2, 20);
      Check (R.Status = Returned and then R.Value = 7);
      R := Run ([16#85#,16#68#,16#69#,16#60#,16#A4#,16#60#], W, Args, 2, 20);
      Check (R.Status = Returned and then R.Value = 5);
      for Op of Bytes'[16#78#,16#85#] loop
         R := Run ([16#A4#,Op,16#68#,0,0,0], W, Args, 2, 20);
         Check (R.Status = Division_By_Zero);
      end loop;
      declare
         Code : constant Bytes := [16#A4#,16#78#,16#68#,16#69#,0,0];
      begin
         for Last in 1 .. Code'Length - 1 loop
            R := Run (Code (1 .. Last), W, Args, 2, 20);
            Check (R.Status = Truncated);
         end loop;
      end;
      R := Run ([16#A4#,16#78#,16#68#,16#69#,16#6F#,0], W, Args, 2, 20);
      Check (R.Status = Unsupported);
      R := Run ([16#A4#,16#78#,16#68#,16#69#,0,16#6F#], W, Args, 2, 20);
      Check (R.Status = Unsupported);
      R := Run ([16#78#,16#68#,16#69#,16#60#,0,16#A4#,16#60#], W,
                [0 => 16#1_FFFF_FFFF#, 1 => 16#2_0000_0000#, others => 0], 2, 20);
      Check (R.Status = Returned and then R.Value = 16#1_FFFF_FFFF#);
   end loop;
   Ada.Text_IO.Put_Line ("AML-DIVISION-CHECK: PASS" & Checks'Image);
end Division_Tests;
