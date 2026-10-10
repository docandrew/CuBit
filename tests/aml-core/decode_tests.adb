with Ada.Text_IO; use Ada.Text_IO;
with AML_Decode; use AML_Decode;
procedure Decode_Tests is
   use type Integer_Value;
   use type Byte;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then
         raise Program_Error with "check" & Checks'Image;
      end if;
   end Check;
   procedure Literal
     (Data : Bytes; Expected : Integer_Value; Size : Positive)
   is
   begin
      for W in Integer_Width loop
         declare
            R : constant Integer_Result := Read_Integer (Data, W);
            V : constant Integer_Value :=
              (if W = Bits_32 then Expected and 16#FFFF_FFFF# else Expected);
         begin
            Check (R.Kind = Accepted and then R.Value = V
                   and then R.Consumed = Size);
         end;
         for N in 0 .. Size - 1 loop
            Check (Read_Integer
              (Data (Data'First .. Data'First + N - 1), W).Kind = Truncated);
         end loop;
      end loop;
   end Literal;
   Data : Bytes (17 .. 4116) := [others => 0];
   R : Package_Result;
   F : Field_Length_Result;
   Expected : Natural;
   Following : Natural;
   Count : Positive;
   Prefix : Byte;
begin
   Literal ([1 => 0], 0, 1);
   Literal ([1 => 1], 1, 1);
   Literal ([1 => 16#FF#], Integer_Value'Last, 1);
   Literal ([16#0A#, 16#A5#, 16#FF#], 16#A5#, 2);
   Literal ([16#0B#, 16#34#, 16#12#], 16#1234#, 3);
   Literal ([16#0C#, 16#78#, 16#56#, 16#34#, 16#12#], 16#12345678#, 5);
   Literal ([16#0E#, 16#EF#, 16#CD#, 16#AB#, 16#89#,
             16#67#, 16#45#, 16#23#, 16#01#], 16#0123456789ABCDEF#, 9);
   Literal ([Extended_Op, Revision_Extension], Interpreter_Revision, Revision_Bytes);
   for B in Byte loop
      if B = Extended_Op then
         Check (Read_Integer ([1 => B], Bits_64).Kind = Truncated);
      elsif B not in 0 | 1 | 16#FF# | 16#0A# | 16#0B# | 16#0C# | 16#0E# then
         Check (Read_Integer ([1 => B], Bits_64).Kind = Unsupported);
      end if;
      Literal ([Positive'Last - 1 => 16#0A#, Positive'Last => B],
               Integer_Value (B), 2);
   end loop;
   Check (Read_Package ([1 .. 0 => 0]).Kind = Truncated);
   --  Exhaust every possible first two bytes. Independent division/remainder
   --  oracle and full/truncated slices cover reserved bits and length bounds.
   for A in 0 .. 255 loop
      for B in 0 .. 255 loop
         Data (17) := Byte (A);
         Data (18) := Byte (B);
         Following := A / 64;
         Count := Following + 1;
         Expected := (if Following = 0 then A mod 64 else A mod 16 + B * 16);
         for N in 0 .. 5 loop
            declare
               Size : constant Natural := (if N = 5 then Data'Length else N);
            begin
               R := Read_Package (Data (17 .. 16 + Size));
               F := Read_Field_Length (Data (17 .. 16 + Size));
               if Size < Count then Check (F.Kind = Truncated);
               elsif Following > 0 and then (A / 16) mod 4 /= 0 then
                  Check (F.Kind = Malformed);
               else
                  Check (F.Kind = Accepted and then F.Bits = Expected
                    and then F.Encoding_Bytes = Count);
               end if;
               if Size < Count then
                  Check (R.Kind = Truncated);
               elsif Following > 0 and then (A / 16) mod 4 /= 0 then
                  Check (R.Kind = Malformed);
               elsif Expected < Count then
                  Check (R.Kind = Malformed);
               elsif Expected > Size then
                  Check (R.Kind = Truncated);
               else
                  Check (R.Kind = Accepted and then R.Extent = Expected
                         and then R.Encoding_Bytes = Count);
               end if;
            end;
         end loop;
      end loop;
   end loop;
   --  Every bit of the longer length encodings: high bits must not wrap
   --  back into the admitted small buffer.
   for I in 1 .. 3 loop
      for B in 0 .. 255 loop
         Data := [others => 0];
         Prefix := Byte (I * 64 + 4);
         Data (17) := Prefix;
         Data (17 + I) := Byte (B);
         Expected := 4 + B * 2 ** (4 + 8 * (I - 1));
         F := Read_Field_Length (Data (17 .. 17 + I));
         Check (F.Kind = Accepted and then F.Bits = Expected and then F.Encoding_Bytes = I + 1);
         R := Read_Package (Data);
         Check ((if Expected > Data'Length then R.Kind = Truncated else
                 R.Kind = Accepted and then R.Extent = Expected));
      end loop;
   end loop;
   R := Read_Package ([Positive'Last => 1]);
   Check (R.Kind = Accepted and then R.Extent = 1);
   R := Read_Package ([Positive'Last - 1 => 16#42#, Positive'Last => 0]);
   Check (R.Kind = Accepted and then R.Extent = 2);
   Check (Read_Package ([16#CF#, 16#FF#, 16#FF#, 16#FF#]).Kind = Truncated);
   F := Read_Field_Length ([Positive'Last => 0]);
   Check (F.Kind = Accepted and then F.Bits = 0 and then F.Encoding_Bytes = 1);
   F := Read_Field_Length ([Positive'Last - 3 => 16#CF#,
                            Positive'Last - 2 .. Positive'Last => 16#FF#]);
   Check (F.Kind = Accepted and then F.Bits = Field_Bit_Length'Last and then F.Encoding_Bytes = 4);
   F := Read_Field_Length ([16#C0#, 0, 0, 0]);
   Check (F.Kind = Accepted and then F.Bits = 0 and then F.Encoding_Bytes = 4);
   Check (Read_Package ([16#C0#, 0, 0, 0]).Kind = Malformed);
   Put_Line ("AML-DECODE-CHECK: PASS" & Checks'Image);
end Decode_Tests;
