with Ada.Text_IO;
with AML_Coercions;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with Namespace_Instance;
procedure Coercion_Tests is
   use type Integer_Value;
   use type AML_Coercions.Conversion_Status;
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.State;
   Checks : Natural := 0;
   C : AML_Coercions.Result;
   Expected : Integer_Value;
   function Enc (S : String) return Bytes is
      Data : Bytes (1 .. S'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (S (S'First + (I - 1))); end loop;
      return Data;
   end Enc;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Text_Test (S : String; V32, V64 : Integer_Value) is
   begin
      C := AML_Coercions.From_String (Enc (S), Bits_32);
      Check (C.Status = AML_Coercions.Converted and C.Value = V32);
      C := AML_Coercions.From_String (Enc (S), Bits_64);
      Check (C.Status = AML_Coercions.Converted and C.Value = V64);
   end Text_Test;
   procedure Test (Body_Data : Bytes; Status : Execution_Status; V : Integer_Value := 0;
                   Boolean_Result : Boolean := False) is
      Data : constant Bytes :=
        [8] & Enc ("STR0") & [16#0D#] & Enc (" 0x1fZ") & [0] &
        [8] & Enc ("BUF0") & [16#11#,4,1,16#34#,16#12#] &
        [8] & Enc ("EMP0") & [16#11#,2,0] &
        [8] & Enc ("PKG0") & [16#12#,2,1] &
        [16#14#,8] & Enc ("CALI") & [1,16#A4#,16#68#] &
        [16#14#,Byte (6 + Body_Data'Length)] & Enc ("READ") & [0] & Body_Data;
      Tree, Before : NS.State;
      Loaded : NS.Load_Status;
      R : Execution_Result;
   begin
      for W in Integer_Width loop
         Tree := NS.Empty;
         NS.Load_Names (Tree, Data, W, Loaded);
         Check (Loaded = NS.Loaded);
         Before := Tree;
         R := NS.Invoke (Tree, NS.Child (Tree, 0, "READ"), [others => 0], 0, 100);
         Check (R.Status = Status and then (if Status = Returned then R.Value =
           (if Boolean_Result then AML_Coercions.Maximum (W) else V)));
         Check (Tree = Before);
         if R.Status = Returned then
            R := NS.Invoke (Tree, NS.Child (Tree, 0, "READ"), [others => 0], 0, R.Charged - 1);
            Check (R.Status = Budget_Exceeded);
         end if;
      end loop;
   end Test;
begin
   Text_Test ("", 0, 0);
   Text_Test ("  " & ASCII.HT & "0X000fgh", 15, 15);
   Text_Test ("-10", 0, 0);
   Text_Test ("+10", 0, 0);
   Text_Test ("0x", 0, 0);
   Text_Test ("12 34", 18, 18);
   Text_Test ("0000000000000000000000000001", 1, 1);
   Text_Test ("123456789ABCDEF01", 16#12345678#, 16#123456789ABCDEF0#);
   for W in Integer_Width loop
      for B in 0 .. 255 loop
         Expected := (case B is
           when 48 .. 57 => Integer_Value (B - 48),
           when 65 .. 70 => Integer_Value (B - 55),
           when 97 .. 102 => Integer_Value (B - 87), when others => 0);
         C := AML_Coercions.From_String ([Positive'Last => Byte (B)], W);
         Check (C.Status = AML_Coercions.Converted and C.Value = Expected);
      end loop;
      for L in 0 .. 12 loop
         declare
            Data : Bytes (7 .. 6 + L);
         begin
            Expected := 0;
            for I in 1 .. L loop
               Data (6 + I) := Byte (I);
               if I <= (if W = Bits_32 then 4 else 8) then
                  Expected := Expected + Integer_Value (I) * 2 ** ((I - 1) * 8);
               end if;
            end loop;
            C := AML_Coercions.From_Buffer (Data, W);
            Check (C.Value = Expected and C.Status =
              (if L = 0 then AML_Coercions.Empty_Buffer else AML_Coercions.Converted));
         end;
      end loop;
      -- Every possible byte at every position, including ignored tail bytes,
      -- with array bounds at Positive'Last. Expected values use place values.
      for Position in 0 .. 11 loop
         for B in 0 .. 255 loop
            declare
               Data : Bytes (Positive'Last - 11 .. Positive'Last) := [others => 0];
            begin
               Data (Data'First + Position) := Byte (B);
               Expected := (if Position < (if W = Bits_32 then 4 else 8)
                            then Integer_Value (B) * 2 ** (Position * 8) else 0);
               C := AML_Coercions.From_Buffer (Data, W);
               Check (C.Status = AML_Coercions.Converted and C.Value = Expected);
            end;
         end loop;
      end loop;
   end loop;
   Test ([16#A4#,16#72#] & Enc ("STR0") & [1,0], Returned, 32);
   Test ([16#A4#,16#72#] & Enc ("BUF0") & [1,0], Returned, 16#1235#);
   Test ([16#A4#,16#72#] & Enc ("EMP0") & [1,0], Empty_Buffer);
   Test ([16#A4#,16#72#] & Enc ("PKG0") & [1,0], Unsupported_Value);
   Test ([16#A4#] & Enc ("STR0"), Object_Returned);
   Test ([16#A4#,16#72#] & Enc ("CALI") & Enc ("STR0") & [1,0], Returned, 32);
   Test ([16#A4#,16#93#,16#0A#,31] & Enc ("STR0"), Returned, Boolean_Result => True);
   Test ([16#A4#,16#93#] & Enc ("STR0") & [16#0A#,31], Unsupported_Value);
   Test ([16#A4#,16#90#] & Enc ("STR0") & [1], Returned, Boolean_Result => True);
   Test ([16#A0#,7] & Enc ("STR0") & [16#A4#,1,16#A4#,0], Returned, 1);
   Test ([16#A4#,16#72#,16#0D#,49,48,0,1,0], Returned, 17);
   Test ([16#A4#,16#72#,16#11#,4,1,16#34#,16#12#,1,0], Returned, 16#1235#);
   Test ([16#A4#,16#72#,16#11#,2,0,1,0], Empty_Buffer);
   Ada.Text_IO.Put_Line ("AML-COERCION-CHECK: PASS" & Checks'Image);
end Coercion_Tests;
