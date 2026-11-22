pragma Ada_2022;
package body Metric_Codecs with SPARK_Mode is

   procedure Lemma_Zigzag_Round_Trip (Value : Unsigned_64) is null;

   procedure Put_Varint (Into : in out Byte_Array; At_Byte : Natural; Value : Unsigned_64) is
      Count : constant Varint_Length := Varint_Bytes (Value);
   begin
      for I in 0 .. Count - 1 loop
         Into (At_Byte + I) := Group (Value, I);
         pragma Loop_Invariant (for all J in 0 .. I => Into (At_Byte + J) = Group (Value, J));
         pragma Loop_Invariant
           (for all K in Into'Range =>
              (if K < At_Byte or else K > At_Byte + I then Into (K) = Into'Loop_Entry (K)));
      end loop;
   end Put_Varint;

   procedure Get_Varint
     (From : Byte_Array; At_Byte : Natural; Value : out Unsigned_64; Length : out Varint_Length;
      Valid : out Boolean)
   is
      Read : Natural := 0;
   begin
      Value := 0;
      Length := 1;
      Valid := False;
      while Read < Maximum_Varint_Bytes and then At_Byte + Read <= From'Last loop
         pragma Loop_Invariant (Value = Value_Of (From, At_Byte, Read));
         pragma Loop_Invariant (for all K in At_Byte .. At_Byte + Read - 1 => Continues (From (K)));
         Value := Value or Shift_Left (Unsigned_64 (From (At_Byte + Read) and Group_Mask), Group_Bits * Read);
         if not Continues (From (At_Byte + Read)) then
            Length := Read + 1;
            Valid := True;
            return;
         end if;
         Read := Read + 1;
      end loop;
   end Get_Varint;

   procedure Lemma_Varint_Round_Trip (From : Byte_Array; At_Byte : Natural; Value : Unsigned_64) is
      Count : constant Varint_Length := Varint_Bytes (Value);
   begin
      --  Each byte is its group; the groups of Value, rejoined, are Value
      --  (it has no bits above Count groups).
      for I in 1 .. Count loop
         pragma Loop_Invariant
           (Value_Of (From, At_Byte, I - 1) =
              (if I = 1 then 0 else Value and (Shift_Left (1, Group_Bits * (I - 1)) - 1)));
         pragma Loop_Invariant
           (for all K in At_Byte .. At_Byte + I - 2 => Continues (From (K)));
         pragma Assert (From (At_Byte + I - 1) = Group (Value, I - 1));
         pragma Assert ((Group (Value, I - 1) and Group_Mask) = Bits_Of (Value, I - 1));
         pragma Assert (Unsigned_64 (From (At_Byte + I - 1) and Group_Mask) =
                          (Shift_Right (Value, Group_Bits * (I - 1)) and Group_Mask));
         if I < Count then
            pragma Assert ((Group (Value, I - 1) and More_Bit) /= 0);
         else
            pragma Assert (Group (Value, I - 1) = Bits_Of (Value, I - 1));
            pragma Assert ((Bits_Of (Value, I - 1) and More_Bit) = 0);
         end if;
      end loop;
      pragma Assert (Value_Of (From, At_Byte, Count) = Value);
   end Lemma_Varint_Round_Trip;
end Metric_Codecs;
