with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with Namespace_Instance;
procedure Inspect_Tests is
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.State;
   use type Integer_Value;
   Checks : Natural := 0;
   function Enc (S : String) return Bytes is
      R : Bytes (1 .. S'Length);
   begin
      for I in R'Range loop R (I) := Character'Pos (S (S'First + (I - 1))); end loop;
      return R;
   end Enc;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Test (Body_Data : Bytes; Expected : Execution_Status;
                   Value_32 : Integer_Value := 0; Value_64 : Integer_Value := 0) is
      Data : constant Bytes :=
        [8] & Enc ("INT0") & [1] &
        [8] & Enc ("STR0") & [16#0D#,65,66,0] &
        [8] & Enc ("BUF0") & [16#11#,4,16#0A#,9,42] &
        [8] & Enc ("PKG0") & [16#12#,2,5] &
        [16#5B#,16#82#,5] & Enc ("DEV0") &
        [16#14#,7] & Enc ("NEVR") & [7,16#FE#] &
        [16#14#,Byte (6 + Body_Data'Length)] & Enc ("READ") & [0] & Body_Data;
      Tree, Before : NS.State;
      Loaded : NS.Load_Status;
      Node : NS.Node_ID;
      R : Execution_Result;
   begin
      for W in Integer_Width loop
         Tree := NS.Empty;
         NS.Load_Names (Tree, Data, W, Loaded);
         Check (Loaded = NS.Loaded);
         Node := NS.Child (Tree, 0, "READ");
         Before := Tree;
         R := NS.Invoke (Tree, Node, [others => 0], 0, 100);
         Check (R.Status = Expected and then (if Expected = Returned then
           R.Value = (if W = Bits_32 then Value_32 else Value_64)));
         Check (Tree = Before);
         if R.Status = Returned then
            R := NS.Invoke (Tree, Node, [others => 0], 0, R.Charged - 1);
            Check (R.Status = Budget_Exceeded);
         end if;
      end loop;
   end Test;
   R : Execution_Result;
begin
   Test ([16#A4#,16#87#] & Enc ("INT0"), Returned, 4, 8);
   Test ([16#A4#,16#87#] & Enc ("STR0"), Returned, 2, 2);
   Test ([16#A4#,16#87#] & Enc ("BUF0"), Returned, 9, 9);
   Test ([16#A4#,16#87#] & Enc ("PKG0"), Returned, 5, 5);
   Test ([16#A4#,16#8E#] & Enc ("INT0"), Returned, 1, 1);
   Test ([16#A4#,16#8E#] & Enc ("STR0"), Returned, 2, 2);
   Test ([16#A4#,16#8E#] & Enc ("BUF0"), Returned, 3, 3);
   Test ([16#A4#,16#8E#] & Enc ("PKG0"), Returned, 4, 4);
   Test ([16#A4#,16#8E#] & Enc ("DEV0"), Returned, 6, 6);
   Test ([16#A4#,16#8E#] & Enc ("NEVR"), Returned, 8, 8);
   Test ([16#A4#,16#87#] & Enc ("NEVR"), Unsupported_Value);
   Test ([16#A4#,16#87#] & Enc ("DEV0"), Unsupported_Value);
   Test ([16#A4#,16#8E#] & Enc ("MISS"), Unknown_Name);
   Test ([16#A4#,16#87#] & Enc ("MISS"), Unknown_Name);
   Test ([16#A4#,16#72#,16#87#] & Enc ("STR0") & [16#8E#] & Enc ("PKG0") & [0], Returned, 6, 6);
   Test ([16#A4#,16#8E#,16#60#], Returned);
   Test ([16#70#,1,16#60#,16#A4#,16#8E#,16#60#], Returned, 1, 1);
   Test ([16#70#,1,16#60#,16#A4#,16#87#,16#60#], Returned, 4, 8);
   Test ([16#A4#,16#87#,16#60#], Uninitialized);
   Test ([16#A4#,16#8E#,16#5B#,16#31#], Returned, 16, 16);
   Test ([16#A4#,16#87#,16#5B#,16#31#], Unsupported_Value);
   Test ([16#8E#] & Enc ("NEVR") & [16#A4#,1], Returned, 1, 1);
   Test ([16#A4#,16#8E#,0], Unsupported);
   Test ([16#A4#,16#8E#] & Enc ("0AD0"), Unsupported);
   Test ([16#A4#,16#8E#] & Enc ("BaD0"), Bad_Name);
   for I in 0 .. 3 loop
      Test ([16#A4#,16#87#] & Enc ("PKG0") (1 .. I), Truncated);
   end loop;
   for W in Integer_Width loop
      for Arg in Byte range 16#68# .. 16#6E# loop
         R := Run ([16#A4#,16#8E#,Arg], W, [others => 123], 7, 10);
         Check (R.Status = Returned and then R.Value = 1);
         R := Run ([16#A4#,16#87#,Arg], W, [others => 123], 7, 10);
         Check (R.Status = Returned and then R.Value = (if W = Bits_32 then 4 else 8));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("AML-INSPECT-CHECK: PASS" & Checks'Image);
end Inspect_Tests;
