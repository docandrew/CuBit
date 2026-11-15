pragma Ada_2022;

package body CuBit.Metric_Updates with SPARK_Mode is
   KIND_AT : constant := 0;
   CAUSE_AT : constant := 4;
   SERIES_AT : constant := 8;
   FIRST_WORD_AT : constant := 16;
   SECOND_WORD_AT : constant := 24;
   THIRD_WORD_AT : constant := 32;
   HALF_BYTES : constant := 4;
   WORD_BYTES : constant := 8;
   BYTE_BITS : constant := 8;

   procedure Put (Into : in out Element_Buffer; At_Byte : Element_Index; Bytes : Positive; Value : Unsigned_64)
     with Pre => Bytes <= WORD_BYTES and then At_Byte <= Element_Bytes - Bytes;
   procedure Put (Into : in out Element_Buffer; At_Byte : Element_Index; Bytes : Positive; Value : Unsigned_64) is
      Rest : Unsigned_64 := Value;
   begin
      for I in 0 .. Bytes - 1 loop
         Into (At_Byte + I) := Unsigned_8 (Rest and 16#FF#);
         Rest := Shift_Right (Rest, BYTE_BITS);
      end loop;
   end Put;

   function Get (From : Element_Buffer; At_Byte : Element_Index; Bytes : Positive) return Unsigned_64
     with Pre => Bytes <= WORD_BYTES and then At_Byte <= Element_Bytes - Bytes;
   function Get (From : Element_Buffer; At_Byte : Element_Index; Bytes : Positive) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. Bytes - 1 loop
         Value := Shift_Left (Value, BYTE_BITS) or Unsigned_64 (From (At_Byte + I));
      end loop;
      return Value;
   end Get;

   procedure Encode (Item : Element; Into : out Element_Buffer) is
   begin
      Into := [others => 0];
      Put (Into, KIND_AT, HALF_BYTES, Unsigned_64 (Element_Kind'Enum_Rep (Item.Kind)));
      Put (Into, SERIES_AT, WORD_BYTES, Item.Series);
      case Item.Kind is
         when Sample =>
            Put (Into, FIRST_WORD_AT, WORD_BYTES, Item.Time_Us);
            Put (Into, SECOND_WORD_AT, WORD_BYTES, Item.Value);
         when Gap =>
            Put (Into, CAUSE_AT, HALF_BYTES, Unsigned_64 (Gap_Cause'Enum_Rep (Item.Cause)));
            Put (Into, FIRST_WORD_AT, WORD_BYTES, Item.Missing);
            Put (Into, SECOND_WORD_AT, WORD_BYTES, Item.First_Us);
            Put (Into, THIRD_WORD_AT, WORD_BYTES, Item.Last_Us);
      end case;
   end Encode;

   function Decode (From : Element_Buffer) return Decoded is
      Kind_Word : constant Unsigned_64 := Get (From, KIND_AT, HALF_BYTES);
      Cause_Word : constant Unsigned_64 := Get (From, CAUSE_AT, HALF_BYTES);
      Series : constant Unsigned_64 := Get (From, SERIES_AT, WORD_BYTES);
      A : constant Unsigned_64 := Get (From, FIRST_WORD_AT, WORD_BYTES);
      B : constant Unsigned_64 := Get (From, SECOND_WORD_AT, WORD_BYTES);
      C : constant Unsigned_64 := Get (From, THIRD_WORD_AT, WORD_BYTES);
   begin
      if Kind_Word = Element_Kind'Enum_Rep (Sample) then
         if Cause_Word /= 0 or else C /= 0 or else Series = 0 then
            return (Success => False);
         end if;
         return (Success => True, Value => (Kind => Sample, Series => Series, Time_Us => A, Value => B));
      elsif Kind_Word = Element_Kind'Enum_Rep (Gap) then
         if B > C then
            return (Success => False);
         end if;
         for Cause in Gap_Cause loop
            if Cause_Word = Gap_Cause'Enum_Rep (Cause)
              and then (Series = 0) = (Cause = Subscriber_Shed)
            then
               return (Success => True,
                       Value => (Kind => Gap, Series => Series, Cause => Cause, Missing => A, First_Us => B,
                                 Last_Us => C));
            end if;
         end loop;
      end if;
      return (Success => False);
   end Decode;
end CuBit.Metric_Updates;
