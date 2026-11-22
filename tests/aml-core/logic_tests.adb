with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
procedure Logic_Tests is
   use type Integer_Value;
   use type Byte;
   Values : constant array (Positive range <>) of Integer_Value :=
     [0, 1, 2, 255, 16#7FFF_FFFF#, 16#FFFF_FFFF#,
      16#1_0000_0000#, 16#8000_0000_0000_0000#, Integer_Value'Last];
   Checks : Natural := 0;
   R : Execution_Result;
   Expected : Integer_Value;
   Truth : Boolean;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for W in Integer_Width loop
      for Op in Byte range 16#90# .. 16#95# loop
         for Left of Values loop
            for Right of Values loop
               Truth := (case Op is
                 when 16#90# => Left /= 0 and Right /= 0,
                 when 16#91# => Left /= 0 or Right /= 0,
                 when 16#92# => Right = 0,
                 when 16#93# => Left = Right,
                 when 16#94# => Left > Right,
                 when others => Left < Right);
               Expected := (if not Truth then 0 elsif W = Bits_32 then
                              16#FFFF_FFFF# else Integer_Value'Last);
               declare
                  Code : constant Bytes := (if Op = 16#92# then
                    [16#A4#,Op,16#69#] else [16#A4#,Op,16#68#,16#69#]);
                  Cost : constant Natural := (if Op = 16#92# then 3 else 4);
               begin
                  R := Run (Code, W, [0 => Left, 1 => Right, others => 0], 2, Cost);
                  Check (R.Status = Returned and then R.Value = Expected
                         and then R.Charged = Cost);
                  R := Run (Code, W, [0 => Left, 1 => Right, others => 0], 2, Cost - 1);
                  Check (R.Status = Budget_Exceeded);
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   --  AML logical binary evaluation is eager, not short-circuiting.
   R := Run ([16#A4#,16#90#,0,16#60#], Bits_64, [others => 0], 0, 20);
   Check (R.Status = Uninitialized);
   R := Run ([16#A4#,16#91#,1,16#60#], Bits_64, [others => 0], 0, 20);
   Check (R.Status = Uninitialized);
   --  Unary expressions unwind without looking for a second source or target.
   declare
      Code : Bytes (1 .. 67) := [others => 16#92#];
   begin
      Code (1) := 16#A4#;
      for Depth in 1 .. 65 loop
         Code (Depth + 2) := 0;
         R := Run (Code (1 .. Depth + 2), Bits_64, [others => 0], 0, 100);
         Check ((if Depth > 64 then R.Status = Expression_Limit else
                   R.Status = Returned and then R.Value =
                     (if Depth mod 2 = 1 then Integer_Value'Last else 0)));
         Code (Depth + 2) := 16#92#;
      end loop;
   end;
   R := Run ([16#A4#,16#92#], Bits_64, [others => 0], 0, 20);
   Check (R.Status = Truncated);
   R := Run ([16#A4#,16#93#,1], Bits_64, [others => 0], 0, 20);
   Check (R.Status = Truncated);
   Ada.Text_IO.Put_Line ("AML-LOGIC-CHECK: PASS" & Checks'Image);
end Logic_Tests;
