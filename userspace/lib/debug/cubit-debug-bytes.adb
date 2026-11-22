------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;

package body CuBit.Debug.Bytes with SPARK_Mode is

   Byte_Bits : constant := 8;
   LEB_Payload : constant := 16#7F#;
   LEB_More : constant := 16#80#;
   LEB_Sign : constant := 16#40#;
   Word_Bits : constant := 64;

   --  Count bytes from C.At_Byte are within both C's limit and Data. The
   --  comparisons with Data are made in Natural, so nothing here wraps.
   function In_Data (Data : Byte_Buffer; C : Cursor; Count : Offset) return Boolean is
     (C.Ok and then Data'Length > 0
      and then C.At_Byte <= C.Limit and then Count <= C.Limit - C.At_Byte
      and then C.At_Byte <= Offset (Natural'Last)
      and then Natural (C.At_Byte) >= Data'First
      and then Natural (C.At_Byte) <= Data'Last
      and then Count <= Offset (Natural'Last)
      and then (Count = 0 or else Natural (Count) - 1 <= Data'Last - Natural (C.At_Byte)));

   procedure Sized (Data : Byte_Buffer; C : in out Cursor; Size : Natural; V : out Unsigned_64) is
   begin
      V := 0;
      if Size > 8 or else not In_Data (Data, C, Offset (Size)) then
         C.Ok := False;
         return;
      end if;
      declare
         First : constant Natural := Natural (C.At_Byte);
      begin
         pragma Assert (First >= Data'First);
         pragma Assert (Size = 0 or else Size - 1 <= Data'Last - First);
         for I in reverse 0 .. Size - 1 loop
            V := Shift_Left (V, Byte_Bits) or Unsigned_64 (Data (First + I));
         end loop;
      end;
      C.At_Byte := C.At_Byte + Offset (Size);
   end Sized;

   procedure U8  (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64) is
   begin
      Sized (Data, C, 1, V);
   end U8;
   procedure U16 (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64) is
   begin
      Sized (Data, C, 2, V);
   end U16;
   procedure U32 (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64) is
   begin
      Sized (Data, C, 4, V);
   end U32;
   procedure U64 (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64) is
   begin
      Sized (Data, C, 8, V);
   end U64;

   --  At most ten bytes hold 64 bits; an eleventh is malformed.
   LEB_Byte_Ceiling : constant := 10;
   LEB_Step : constant := 7;

   procedure ULEB (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64) is
      B : Unsigned_64;
   begin
      V := 0;
      for Count in 0 .. LEB_Byte_Ceiling - 1 loop
         U8 (Data, C, B);
         if not C.Ok then
            V := 0;
            return;
         end if;
         V := V or Shift_Left (B and LEB_Payload, LEB_Step * Count);
         if (B and LEB_More) = 0 then
            return;
         end if;
      end loop;
      C.Ok := False;   --  an overlong encoding
      V := 0;
   end ULEB;

   procedure SLEB (Data : Byte_Buffer; C : in out Cursor; V : out Integer_64) is
      B : Unsigned_64;
      R : Unsigned_64 := 0;
      Shift : Natural;
   begin
      V := 0;
      for Count in 0 .. LEB_Byte_Ceiling - 1 loop
         U8 (Data, C, B);
         if not C.Ok then
            return;
         end if;
         R := R or Shift_Left (B and LEB_Payload, LEB_Step * Count);
         if (B and LEB_More) = 0 then
            Shift := LEB_Step * (Count + 1);
            if Shift < Word_Bits and then (B and LEB_Sign) /= 0 then
               R := R or Shift_Left (not 0, Shift);
            end if;
            V := To_Signed (R);
            return;
         end if;
      end loop;
      C.Ok := False;
   end SLEB;

   procedure Skip (C : in out Cursor; Count : Offset) is
   begin
      if not C.Ok or else C.At_Byte > C.Limit or else Count > C.Limit - C.At_Byte then
         C.Ok := False;
      else
         C.At_Byte := C.At_Byte + Count;
      end if;
   end Skip;

   procedure Skip_String (Data : Byte_Buffer; C : in out Cursor) is
      B : Unsigned_64;
   begin
      loop
         U8 (Data, C, B);
         exit when not C.Ok or else B = 0;
      end loop;
   end Skip_String;

   function C_String (Data : Byte_Buffer; At_Byte : Offset; Max : Positive) return String is
      Readable : constant Boolean :=
        Data'Length > 0 and then At_Byte <= Offset (Natural'Last)
        and then Natural (At_Byte) >= Data'First and then Natural (At_Byte) <= Data'Last;
      Base : constant Natural := (if Readable then Natural (At_Byte) else 0);

      --  How many bytes before the NUL, the end of Data, or Max.
      function Length_Of return Natural
      with Post => Length_Of'Result <= Max
        and then (if Length_Of'Result > 0 then
                    Readable and then Base >= Data'First
                    and then Length_Of'Result - 1 <= Data'Last - Base)
      is
         Length : Natural := 0;
      begin
         if not Readable then
            return 0;
         end if;
         pragma Assert (Base >= Data'First and then Base <= Data'Last);
         for I in 0 .. Max - 1 loop
            pragma Loop_Invariant (Length = I);
            pragma Loop_Invariant (I = 0 or else I - 1 <= Data'Last - Base);
            exit when I > Data'Last - Base or else Data (Base + I) = 0;
            Length := I + 1;
         end loop;
         return Length;
      end Length_Of;
      Length : constant Natural := Length_Of;
      Result : String (1 .. Length) := [others => ' '];
   begin
      for I in 1 .. Length loop
         Result (I) := Character'Val (Data (Base + (I - 1)));
      end loop;
      return Result;
   end C_String;

   function Conv is new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);
   function Conv is new Ada.Unchecked_Conversion (Integer_64, Unsigned_64);

   function To_Signed (V : Unsigned_64) return Integer_64 is (Conv (V));
   function To_Unsigned (V : Integer_64) return Unsigned_64 is (Conv (V));

   function Sign_Extend (V : Unsigned_64; Bits : Positive) return Integer_64 is
   begin
      if Bits >= Word_Bits then
         return Conv (V);
      end if;
      declare
         Mask : constant Unsigned_64 := Shift_Left (1, Bits) - 1;
         Low : constant Unsigned_64 := V and Mask;
      begin
         if (Low and Shift_Left (1, Bits - 1)) /= 0 then
            return Conv (Low or not Mask);
         end if;
         return Conv (Low);
      end;
   end Sign_Extend;

end CuBit.Debug.Bytes;
