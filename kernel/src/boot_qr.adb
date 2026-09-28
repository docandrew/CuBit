pragma Ada_2022;
with Interfaces; use Interfaces;
package body Boot_QR is
   Data_Codewords : constant := 80;
   Error_Codewords : constant := 20;
   Total_Codewords : constant := Data_Codewords + Error_Codewords;
   subtype Byte_Index is Natural range 0 .. Total_Codewords - 1;
   type Bytes is array (Byte_Index) of Unsigned_8;
   type Function_Matrix is array (Module_Index, Module_Index) of Boolean;
   Function_Modules : Function_Matrix := [others => [others => False]];

   function Multiply (X, Y : Unsigned_8) return Unsigned_8 is
      Result : Unsigned_8 := 0;
      High : Boolean;
   begin
      --  Horner form of multiplication in GF(2^8 / 0x11D). Keeping the
      --  reduction in the same order as ISO/IEC 18004 reference encoders
      --  avoids relying on a hand-transformed equivalent in boot code.
      for I in reverse 0 .. 7 loop
         High := (Result and 16#80#) /= 0;
         Result := Shift_Left (Result, 1);
         if High then Result := Result xor 16#1D#; end if;
         if (Y and Shift_Left (Unsigned_8 (1), I)) /= 0 then
            Result := Result xor X;
         end if;
      end loop;
      return Result;
   end Multiply;

   procedure Set_Function (Result : in out Matrix; X, Y : Integer; Dark : Boolean) is
   begin
      if X in Integer (Module_Index'First) .. Integer (Module_Index'Last) and then
        Y in Integer (Module_Index'First) .. Integer (Module_Index'Last)
      then
         Result (Natural (Y), Natural (X)) := Dark;
         Function_Modules (Natural (Y), Natural (X)) := True;
      end if;
   end Set_Function;

   procedure Finder (Result : in out Matrix; X, Y : Integer) is
      DX, DY, Distance : Integer;
   begin
      for Row in -1 .. 7 loop
         for Column in -1 .. 7 loop
            DX := Column - 3;
            DY := Row - 3;
            Distance := Integer'Max (abs DX, abs DY);
            Set_Function (Result, X + Column, Y + Row,
                          Distance /= 2 and then Distance /= 4);
         end loop;
      end loop;
   end Finder;

   procedure Alignment (Result : in out Matrix; X, Y : Integer) is
      Distance : Integer;
   begin
      for Row in -2 .. 2 loop
         for Column in -2 .. 2 loop
            Distance := Integer'Max (abs Column, abs Row);
            Set_Function (Result, X + Column, Y + Row, Distance /= 1);
         end loop;
      end loop;
   end Alignment;

   procedure Format (Result : in out Matrix; Mask : Natural) is
      Data : constant Unsigned_16 := Shift_Left (Unsigned_16 (1), 3) or Unsigned_16 (Mask);
      Remainder : Unsigned_16 := Data;
      Bits : Unsigned_16;
      function Bit (Position : Natural) return Boolean is
        ((Shift_Right (Bits, Position) and 1) /= 0);
   begin
      for I in 1 .. 10 loop
         Remainder := Shift_Left (Remainder, 1) xor
           (if (Remainder and 16#0200#) /= 0 then 16#0537# else 0);
      end loop;
      Bits := (Shift_Left (Data, 10) or Remainder) xor 16#5412#;
      --  The format copies deliberately cross the timing row/column.  The
      --  first copy starts down the vertical timing column, then turns onto
      --  the horizontal row; the second is its opposite-side counterpart.
      for I in 0 .. 5 loop Set_Function (Result, 8, I, Bit (I)); end loop;
      Set_Function (Result, 8, 7, Bit (6));
      Set_Function (Result, 8, 8, Bit (7));
      Set_Function (Result, 7, 8, Bit (8));
      for I in 9 .. 14 loop Set_Function (Result, 14 - I, 8, Bit (I)); end loop;
      for I in 0 .. 7 loop Set_Function (Result, Dimension - 1 - I, 8, Bit (I)); end loop;
      for I in 8 .. 14 loop Set_Function (Result, 8, Dimension - 15 + I, Bit (I)); end loop;
      Set_Function (Result, 8, Dimension - 8, True);
   end Format;

   procedure Patterns (Result : in out Matrix) is
   begin
      Finder (Result, 0, 0);
      Finder (Result, Dimension - 7, 0);
      Finder (Result, 0, Dimension - 7);
      Alignment (Result, 26, 26);
      for I in 8 .. Dimension - 9 loop
         Set_Function (Result, I, 6, I mod 2 = 0);
         Set_Function (Result, 6, I, I mod 2 = 0);
      end loop;
      -- Reserve format cells before data placement; mask zero is used.
      Format (Result, 0);
   end Patterns;

   procedure Make_Codewords (Text : String; Result : out Bytes) is
      Data : Bytes := [others => 0];
      Bit_Length : Natural := 0;
      procedure Append (Value : Unsigned_16; Count : Positive) is
      begin
         for Offset in reverse 0 .. Count - 1 loop
            if (Shift_Right (Value, Offset) and 1) /= 0 then
               Data (Bit_Length / 8) := Data (Bit_Length / 8) or
                 Shift_Left (Unsigned_8 (1), 7 - Bit_Length mod 8);
            end if;
            Bit_Length := Bit_Length + 1;
         end loop;
      end Append;
      Divisor : array (Natural range 0 .. Error_Codewords - 1) of Unsigned_8 :=
        [others => 0];
      Remainder : array (Natural range 0 .. Error_Codewords - 1) of Unsigned_8 :=
        [others => 0];
      Root, Factor : Unsigned_8 := 1;
   begin
      Append (4, 4);
      Append (Unsigned_16 (Text'Length), 8);
      for C of Text loop Append (Unsigned_16 (Character'Pos (C)), 8); end loop;
      for I in 1 .. Natural'Min (4, Data_Codewords * 8 - Bit_Length) loop
         Append (0, 1);
      end loop;
      while Bit_Length mod 8 /= 0 loop Append (0, 1); end loop;
      for I in Bit_Length / 8 .. Data_Codewords - 1 loop
         Data (I) := (if I mod 2 = 0 then 16#EC# else 16#11#);
      end loop;
      --  Generator polynomial without its implicit leading coefficient.
      Divisor (Error_Codewords - 1) := 1;
      for I in 0 .. Error_Codewords - 1 loop
         for J in 0 .. Error_Codewords - 1 loop
            Divisor (J) := Multiply (Divisor (J), Root);
            if J + 1 < Error_Codewords then
               Divisor (J) := Divisor (J) xor Divisor (J + 1);
            end if;
         end loop;
         Root := Multiply (Root, 2);
      end loop;
      for I in 0 .. Data_Codewords - 1 loop
         Factor := Data (I) xor Remainder (0);
         for J in 0 .. Error_Codewords - 2 loop
            Remainder (J) := Remainder (J + 1);
         end loop;
         Remainder (Error_Codewords - 1) := 0;
         --  Keep the reduction as two passes. Besides mirroring the QR
         --  specification directly (shift, append zero, then subtract the
         --  scaled divisor), this makes the old/new remainder provenance
         --  explicit for both reviewers and future proofs.
         for J in 0 .. Error_Codewords - 1 loop
            Remainder (J) := Remainder (J) xor Multiply (Divisor (J), Factor);
         end loop;
      end loop;
      for I in 0 .. Data_Codewords - 1 loop Result (I) := Data (I); end loop;
      for I in 0 .. Error_Codewords - 1 loop Result (Data_Codewords + I) := Remainder (I); end loop;
   end Make_Codewords;

   procedure Place_And_Mask (Result : in out Matrix; Codewords : Bytes) is
      Bit_Index : Natural := 0;
      Right : Integer := Dimension - 1;
      Pair_Right : Integer;
      Upward : Boolean;
      Y : Integer;
   begin
      while Right > 0 loop
         --  The scan source remains 32, 30, ..., 2. Each of the three
         --  stripes at or left of the timing column shifts one column left;
         --  mutating the source itself would accidentally skip 4 and 2.
         Pair_Right := (if Right <= 6 then Right - 1 else Right);
         --  QR zig-zag direction alternates for each two-column stripe.
         --  This is the low-bit-2 test from ISO/IEC 18004: its true cases
         --  are residues 0 and 1, not just zero.
         Upward := ((Pair_Right + 1) mod 4) < 2;
         for Vertical in 0 .. Dimension - 1 loop
            Y := (if Upward then Dimension - 1 - Vertical else Vertical);
            for Column in 0 .. 1 loop
               declare X : constant Integer := Pair_Right - Column; begin
                  if not Function_Modules (Natural (Y), Natural (X)) then
                     --  Version 4 has seven remainder modules after the 100
                     --  codewords. They are zero before masking, but must be
                     --  masked just like ordinary data modules.
                     Result (Natural (Y), Natural (X)) := Bit_Index < Total_Codewords * 8 and then
                       (Shift_Right (Codewords (Bit_Index / 8),
                         7 - Bit_Index mod 8) and 1) /= 0;
                     --  Standard mask 0: (x + y) is even.
                     if (X + Y) mod 2 = 0 then
                        Result (Natural (Y), Natural (X)) :=
                          not Result (Natural (Y), Natural (X));
                     end if;
                     Bit_Index := Bit_Index + 1;
                  end if;
               end;
            end loop;
         end loop;
         Right := Right - 2;
      end loop;
   end Place_And_Mask;

   procedure Encode (Text : String; Result : out Matrix) is
      Codewords : Bytes;
   begin
      Result := [others => [others => False]];
      Function_Modules := [others => [others => False]];
      Patterns (Result);
      Make_Codewords (Text, Codewords);
      Place_And_Mask (Result, Codewords);
      --  The second call writes the same format reservation with final bits.
      Format (Result, 0);
   end Encode;
end Boot_QR;
