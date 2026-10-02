with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
procedure Store_Tests is
   use type Byte;
   use type Integer_Value;
   Checks : Natural := 0;
   R : Execution_Result;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Expect (Code : Bytes; Number : Integer_Value; Count : Natural := 0) is
      Args : constant Arguments := [others => 17];
   begin
      for W in Integer_Width loop
         R := Run (Code, W, Args, Count, 1000);
         Check (R.Status = Returned and then R.Value = Number);
      end loop;
   end Expect;
begin
   for Target in Byte range 16#60# .. 16#6E# loop
      Expect ([16#70#,1,Target,16#A4#,Target], 1);
      Expect ([16#A4#,16#70#,1,Target], 1);
      Expect ([16#A4#,16#72#,16#70#,1,Target,Target,0], 2);
      Expect ([16#70#,16#0A#,9,Target,16#A4#,16#72#,Target,16#70#,1,Target,0], 2);
      Expect ([16#72#,1,1,Target,16#A4#,Target], 2);
      Expect ([16#70#,1,Target,16#A4#,16#8E#,Target], 1);
      -- Store's result is the source, and all unrelated registers survive.
      for Other in Byte range 16#60# .. 16#6E# loop
         if Other /= Target then
            Expect ([16#70#,16#0A#,9,Other,16#70#,1,Target,16#A4#,Other], 9);
         end if;
      end loop;
   end loop;
   for Count in 0 .. 7 loop
      for A in Byte range 16#68# .. 16#6E# loop
         Expect ([16#70#,1,A,16#A4#,A], 1, Count);
      end loop;
   end loop;
   for Rem_Target in Byte range 16#60# .. 16#6E# loop
      for Quot_Target in Byte range 16#60# .. 16#6E# loop
         Expect ([16#78#,16#0A#,17,16#0A#,5,Rem_Target,Quot_Target,16#A4#,Rem_Target],
                 (if Rem_Target = Quot_Target then 3 else 2));
         Expect ([16#78#,16#0A#,17,16#0A#,5,Rem_Target,Quot_Target,16#A4#,Quot_Target], 3);
      end loop;
   end loop;
   for D in 1 .. 65 loop
      declare
         Code : Bytes (1 .. 2 * D + 2) := [others => 16#68#];
      begin
         Code (1) := 16#A4#;
         Code (2 .. D + 1) := [others => 16#70#];
         Code (D + 2) := 1;
         R := Run (Code, Bits_64, [others => 0], 0, 1000);
         Check ((if D <= 64 then R.Status = Returned and then R.Value = 1 and then R.Charged = D + 2
                 else R.Status = Expression_Limit));
         if D <= 64 then
            for Fuel in 0 .. D + 1 loop
               R := Run (Code, Bits_64, [others => 0], 0, Fuel);
               Check (R.Status = Budget_Exceeded and R.Charged <= Fuel);
            end loop;
         end if;
      end;
   end loop;
   for Target in Byte loop
      if Target in 0 | 1 | 16#FF# then
         Expect ([16#A4#,16#70#,1,Target], 1);
         Expect ([16#A4#,16#72#,1,1,Target], 2);
      elsif Target not in 16#60# .. 16#6E# then
         R := Run ([16#A4#,16#70#,1,Target], Bits_64, [others => 0], 0, 10);
         Check (R.Status = (if Target in 16#41# .. 16#5A# | 16#5F# |
                             16#5C# | 16#5E# | 16#2E# | 16#2F#
                            then Truncated else Unsupported));
      end if;
   end loop;
   R := Run ([16#A4#,16#70#,1], Bits_64, [others => 0], 0, 10);
   Check (R.Status = Truncated);
   R := Run ([16#A4#,16#70#], Bits_64, [others => 0], 0, 10);
   Check (R.Status = Truncated);
   Ada.Text_IO.Put_Line ("AML-STORE-CHECK: PASS" & Checks'Image);
end Store_Tests;
