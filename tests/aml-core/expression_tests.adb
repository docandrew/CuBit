with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
procedure Expression_Tests is
   use type Integer_Value;
   use type Byte;
   Ops : constant Bytes := [16#72#,16#74#,16#77#,16#7B#,16#7D#,16#7F#];
   Args : Arguments := [others => 0];
   R : Execution_Result;
   Expected : Integer_Value;
   Checks : Natural := 0;
   Code : Bytes (1 .. 1000) := [others => 0];
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for W in Integer_Width loop
      for Op of Ops loop
         for A in 0 .. 31 loop
            for B in 0 .. 31 loop
               Args (0) := Integer_Value (A);
               Args (1) := Integer_Value (B);
               case Op is
                  when 16#72# => Expected := Args (0) + Args (1);
                  when 16#74# => Expected := Args (0) - Args (1);
                  when 16#77# => Expected := Args (0) * Args (1);
                  when 16#7B# => Expected := Args (0) and Args (1);
                  when 16#7D# => Expected := Args (0) or Args (1);
                  when others => Expected := Args (0) xor Args (1);
               end case;
               if W = Bits_32 then Expected := Expected and 16#FFFF_FFFF#; end if;
               R := Run ([16#A4#,Op,16#68#,16#69#,0], W, Args, 2, 4);
               Check (R.Status = Returned and then R.Value = Expected and then R.Charged = 4);
            end loop;
         end loop;
      end loop;
   end loop;
   for D in 1 .. 65 loop
      Code := [others => 0];
      Code (1) := 16#A4#;
      for I in 1 .. D loop
         Code (2 * I) := 16#72#;
         Code (2 * I + 1) := 1;
      end loop;
      R := Run (Code (1 .. 2 + 3 * D), Bits_64, Args, 0, 1000);
      Check ((if D <= 64 then R.Status = Returned and then R.Value = Integer_Value (D)
              else R.Status = Expression_Limit));
   end loop;
   -- New integer operators share target handling, including all local slots.
   for W in Integer_Width loop
      for Op of Bytes'[16#79#,16#7A#,16#7C#,16#7E#,16#80#] loop
         for Target in Byte range 16#60# .. 16#67# loop
            declare
               Unary : constant Boolean := Op = 16#80#;
               Source : constant Byte := (if Op = 16#7A# then 16#0A# else 1);
               Operand_Data : constant Bytes := (if Op = 16#7A# then [Source,4] else [1 => Source]);
               Expression : constant Bytes := [Op] & Operand_Data &
                 (if Unary then Bytes'[1 .. 0 => 0] else Bytes'[1 => 1]) & [Target];
            begin
               Expected := (if Op in 16#79# | 16#7A# then 2 else not Integer_Value'(1));
               if W = Bits_32 then Expected := Expected and 16#FFFF_FFFF#; end if;
               R := Run (Expression & [16#A4#,Target], W, Args, 0, 20);
               Check (R.Status = Returned and then R.Value = Expected);
               R := Run (Expression & [16#A4#,Target], W, Args, 0, R.Charged - 1);
               Check (R.Status = Budget_Exceeded);
               for Last in 1 .. Expression'Length - 1 loop
                  R := Run ([16#A4#] & Expression (1 .. Last), W, Args, 0, 20);
                  Check (R.Status = Truncated);
               end loop;
            end;
         end loop;
      end loop;
      R := Run ([16#A4#,16#80#,16#80#,16#FF#,0,0], W, Args, 0, 20);
      Check (R.Status = Returned and then R.Value =
               (if W = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
   end loop;
   --  A left subexpression stores before the right reads the same local.
   R := Run ([16#A4#,16#72#,16#72#,1,1,16#60#,16#60#,0], Bits_64, Args, 0, 20);
   Check (R.Status = Returned and then R.Value = 4);
   R := Run ([16#72#,1,1,16#60#,16#A4#,16#60#], Bits_64, Args, 0, 20);
   Check (R.Status = Returned and then R.Value = 2);
   for Fuel in 0 .. 6 loop
      R := Run ([16#A4#,16#72#,1,1,0], Bits_64, Args, 0, Fuel);
      Check ((if Fuel < 4 then R.Status = Budget_Exceeded else R.Status = Returned));
   end loop;
   for N in 1 .. 4 loop
      Code (1 .. 5) := [16#A4#,16#72#,1,1,0];
      R := Run (Code (1 .. N), Bits_64, Args, 0, 20);
      Check (R.Status = Truncated);
   end loop;
   Args (0) := Integer_Value'Last;
   Args (1) := 1;
   R := Run ([16#A4#,16#72#,16#68#,16#69#,0], Bits_64, Args, 2, 10);
   Check (R.Status = Returned and then R.Value = 0);
   Ada.Text_IO.Put_Line ("AML-EXPRESSION-CHECK: PASS" & Checks'Image);
end Expression_Tests;
