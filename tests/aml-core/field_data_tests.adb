with Ada.Text_IO; use Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Field_Data; use AML_Field_Data;
procedure Field_Data_Tests is
   use type Byte;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   function Oracle (Data : Bytes; Offset, Count : Natural) return Byte is
      Value : Byte := 0;
   begin
      for I in 0 .. Count - 1 loop
         if (Data (Data'First + (Offset + I) / 8) and Byte (2 ** ((Offset + I) mod 8))) /= 0 then
            Value := Value or Byte (2 ** I);
         end if;
      end loop;
      return Value;
   end Oracle;
   Pair : Bytes (17 .. 18);
   R : Read_Result;
   Data : Bytes (17 .. 49);
begin
   -- Exhaust all adjacent byte pairs and starting bit positions for eight-bit
   -- windows; shorter windows sweep every source byte and every bit count.
   for A in Byte loop
      Pair (17) := A;
      for B in Byte loop
         Pair (18) := B;
         for Offset in 0 .. 7 loop
            Check (Window (Pair, Offset, 8) = Oracle (Pair, Offset, 8));
         end loop;
      end loop;
      for Count in 1 .. 7 loop
         for Offset in 0 .. 8 loop
            Check (Window (Pair, Offset, Count) = Oracle (Pair, Offset, Count));
         end loop;
      end loop;
   end loop;
   for I in Data'Range loop Data (I) := Byte ((I * 137) mod 256); end loop;
   for Offset in 0 .. Data'Length * 8 + 1 loop
      for Count in 0 .. 65 loop
         R := Read_Bits (Data, Offset, Count);
         if Offset + Count > Data'Length * 8 then
            Check (R.Status = Truncated);
         else
            Check (R.Status = Accepted and then R.Length = (Count + 7) / 8);
            for I in 1 .. R.Length loop
               Check (R.Content (I) = Oracle (Data, Offset + (I - 1) * 8,
                 Natural'Min (8, Count - (I - 1) * 8)));
            end loop;
            Check ((for all I in R.Length + 1 .. Max_Buffer_Length => R.Content (I) = 0));
         end if;
      end loop;
   end loop;
   R := Read_Bits ([1 .. 0 => 0], 0, 0);
   Check (R.Status = Accepted and then R.Length = 0);
   Check (Read_Bits ([1 .. 0 => 0], 1, 0).Status = Truncated);
   Check (Read_Bits (Data, Natural'Last, 8).Status = Truncated);
   Check (Read_Bits (Data, 0, Natural'Last).Status = Limit_Exceeded);
   R := Read_Bits ([Positive'Last => 16#80#], 7, 1);
   Check (R.Status = Accepted and then R.Content (1) = 1);
   R := Read_Bits ([Positive'Last - 1 => 16#80#, Positive'Last => 1], 7, 2);
   Check (R.Status = Accepted and then R.Content (1) = 3);
   R := Read_Bits ([1 .. Max_Buffer_Length => 16#FF#], 0, Max_Bits);
   Check (R.Status = Accepted and then R.Length = Max_Buffer_Length
     and then (for all I in R.Content'Range => R.Content (I) = 16#FF#));
   Check (Read_Bits ([1 .. Max_Buffer_Length => 16#FF#], 1, Max_Bits).Status = Truncated);
   Put_Line ("AML-FIELD-DATA-CHECK: PASS" & Checks'Image);
end Field_Data_Tests;
