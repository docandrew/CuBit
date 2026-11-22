pragma Ada_2022;
package body Multiboot_Memory_Map with SPARK_Mode is
   function Read_32 (Data : Bytes; Position : Natural) return Unsigned_32 with
     Pre => Position <= Data'Length and then Data'Length - Position >= 4
   is
      Value : Unsigned_32 := 0;
   begin
      for I in Natural range 0 .. 3 loop
         Value := Value or Shift_Left
           (Unsigned_32 (Data (Data'First + Position + I)), 8 * I);
      end loop;
      return Value;
   end Read_32;

   function Read_64 (Data : Bytes; Position : Natural) return Unsigned_64 with
     Pre => Position <= Data'Length and then Data'Length - Position >= 8
   is
   begin
      return Unsigned_64 (Read_32 (Data, Position)) or
        Shift_Left (Unsigned_64 (Read_32 (Data, Position + 4)), 32);
   end Read_64;

   procedure Next_Entry
     (Data : Bytes; Position : Natural; Maximum : Unsigned_64;
      Following : out Natural; Item : out Decoded_Region; Result : out Status)
   is
      type Record_Size is range 0 .. 2 ** 32 - 1;
      Size : Record_Size;
      Base, Length : Unsigned_64;
      Tag : Unsigned_32;
   begin
      Following := Position;
      Item := (others => <>);
      if Data'Length - Position < 4 then
         Result := Truncated_Header;
         return;
      end if;
      Size := Record_Size (Read_32 (Data, Position));
      if Size < 20 then
         Result := Short_Record;
         return;
      elsif Size > Record_Size (Data'Length - Position - 4) then
         Result := Truncated_Record;
         return;
      end if;
      Base := Read_64 (Data, Position + 4);
      Length := Read_64 (Data, Position + 12);
      Tag := Read_32 (Data, Position + 20);
      if Length /= 0 then
         if Base > Maximum or else Length - 1 > Maximum - Base then
            Result := Address_Out_Of_Range;
            return;
         end if;
         Item.First := Base;
         Item.Last := Base + (Length - 1);
         Item.Empty := False;
         Item.Kind := (case Tag is
            when 1 => Usable, when 3 => ACPI_Reclaim, when 4 => ACPI_NVS,
            when 5 => Defective, when others => Reserved);
      end if;
      Following := Position + 4 + Natural (Size);
      Result := Success;
   end Next_Entry;

   procedure Parse
     (Data : Bytes; Maximum : Unsigned_64; Output : out Entries;
      Count : out Natural; Result : out Status)
   is
      Position : Natural := 0;
      Following : Natural;
      Item : Decoded_Region;
   begin
      Output := [others => <>];
      Count := 0;
      Result := Empty_Map;
      while Position < Data'Length loop
         pragma Loop_Invariant (Position <= Data'Length);
         pragma Loop_Invariant (Count <= Output'Length);
         pragma Loop_Invariant (if Position = 0 then Count = 0 else Count > 0);
         pragma Loop_Invariant
           (if Count > 0 then
              (for all I in Output'First .. Output'First + (Count - 1) =>
                 Valid (Output (I), Maximum)));
         pragma Loop_Variant (Decreases => Data'Length - Position);
         if Count = Output'Length then
            Count := 0;
            Result := Capacity_Exceeded;
            return;
         end if;
         Next_Entry (Data, Position, Maximum, Following, Item, Result);
         if Result /= Success then
            Count := 0;
            return;
         end if;
         Output (Output'First + Count) := Item;
         Count := Count + 1;
         Position := Following;
      end loop;
   end Parse;
end Multiboot_Memory_Map;
