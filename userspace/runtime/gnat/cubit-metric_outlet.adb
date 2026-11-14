pragma Ada_2022;
package body CuBit.Metric_Outlet with SPARK_Mode is
   WORD_BYTES : constant := 8;
   HALF_BYTES : constant := 4;
   BYTE_BITS : constant := 8;
   BYTE_MASK : constant := 16#FF#;
   KIND_AT : constant := 1;
   KEY_AT : constant := 5;
   SERIES_KIND_AT : constant := 9;
   UNIT_AT : constant := 10;
   NAME_LENGTH_AT : constant := 11;
   RESERVED_FIRST : constant := 12;
   NAME_AT : constant := Header_Bytes + 1;
   TIME_AT : constant := 9;
   VALUE_AT : constant := 17;
   SHED_AT : constant := 9;
   FIRST_AT : constant := 17;
   LAST_AT : constant := 25;
   pragma Compile_Time_Error (LAST_AT + WORD_BYTES - 1 /= Loss_Bytes, "a loss fills Loss_Bytes");
   pragma Compile_Time_Error (VALUE_AT + WORD_BYTES - 1 /= Sample_Bytes, "a sample fills Sample_Bytes");

   procedure Put (Into : in out Element_Buffer; At_Byte : Element_Index; Count : Positive;
                  Value : Unsigned_64)
   with Pre => Count <= WORD_BYTES and then At_Byte <= Maximum_Element_Bytes - Count + 1;
   function Get (From : Element_Buffer; At_Byte : Element_Index; Count : Positive)
     return Unsigned_64
   with Pre => Count <= WORD_BYTES and then At_Byte <= Maximum_Element_Bytes - Count + 1;

   procedure Put (Into : in out Element_Buffer; At_Byte : Element_Index; Count : Positive;
                  Value : Unsigned_64) is
   begin
      for I in 0 .. Count - 1 loop
         Into (At_Byte + I) := Unsigned_8 (Shift_Right (Value, I * BYTE_BITS) and BYTE_MASK);
      end loop;
   end Put;

   function Get (From : Element_Buffer; At_Byte : Element_Index; Count : Positive)
     return Unsigned_64
   is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. Count - 1 loop
         Value := Shift_Left (Value, BYTE_BITS) or Unsigned_64 (From (At_Byte + I));
      end loop;
      return Value;
   end Get;

   procedure Encode (Item : Element; Into : out Element_Buffer; Length : out Element_Length) is
   begin
      Into := [others => 0];
      Put (Into, KIND_AT, HALF_BYTES, Unsigned_64 (Element_Kind'Enum_Rep (Item.Kind)));
      Put (Into, KEY_AT, HALF_BYTES, Unsigned_64 (Item.Key));
      case Item.Kind is
         when Describe =>
            Into (SERIES_KIND_AT) := Unsigned_8 (Series_Kind'Enum_Rep (Item.Series));
            Into (UNIT_AT) := Unsigned_8 (Unit'Enum_Rep (Item.Measure));
            Into (NAME_LENGTH_AT) := Unsigned_8 (Item.Length);
            for I in 1 .. Item.Length loop
               Into (NAME_AT + I - 1) := Character'Pos (Item.Name (I));
            end loop;
         when Sample =>
            Put (Into, TIME_AT, WORD_BYTES, Item.Time_Us);
            Put (Into, VALUE_AT, WORD_BYTES, Item.Value);
         when Loss =>
            Put (Into, SHED_AT, WORD_BYTES, Item.Shed);
            Put (Into, FIRST_AT, WORD_BYTES, Item.First_Us);
            Put (Into, LAST_AT, WORD_BYTES, Item.Last_Us);
      end case;
      Length := Length_Of (Item);
   end Encode;

   function Decode (From : Element_Buffer; Length : Natural) return Decoded is
      Kind_Word, Key_Word : Unsigned_64;
   begin
      if Length <= Key_Bytes or else Length > Maximum_Element_Bytes then
         return (Success => False, Why => Bad_Length);
      end if;
      Kind_Word := Get (From, KIND_AT, HALF_BYTES);
      Key_Word := Get (From, KEY_AT, HALF_BYTES);
      if Key_Word not in 1 .. Maximum_Series_Per_Source then
         return (Success => False, Why => Bad_Key);
      end if;
      if Kind_Word = Element_Kind'Enum_Rep (Describe) then
         declare
            Series_Code : constant Unsigned_8 := From (SERIES_KIND_AT);
            Unit_Code : constant Unsigned_8 := From (UNIT_AT);
            Name_Count : constant Natural := Natural (From (NAME_LENGTH_AT));
            Result : Element (Describe);
            Series_Found, Unit_Found : Boolean := False;
         begin
            if Name_Count not in 1 .. Maximum_Name_Bytes or else Length /= Describe_Fixed_Bytes + Name_Count then
               return (Success => False, Why => Bad_Length);
            end if;
            if (for some I in RESERVED_FIRST .. Header_Bytes => From (I) /= 0) then
               return (Success => False, Why => Nonzero_Reserved);
            end if;
            Result.Key := Series_Key (Key_Word);
            for K in Series_Kind loop
               if Series_Code = Series_Kind'Enum_Rep (K) then
                  Result.Series := K;
                  Series_Found := True;
               end if;
            end loop;
            if not Series_Found then
               return (Success => False, Why => Bad_Series_Kind);
            end if;
            for U in Unit loop
               if Unit_Code = Unit'Enum_Rep (U) then
                  Result.Measure := U;
                  Unit_Found := True;
               end if;
            end loop;
            if not Unit_Found then
               return (Success => False, Why => Bad_Unit);
            end if;
            Result.Length := Name_Count;
            for I in 1 .. Name_Count loop
               Result.Name (I) := Character'Val (From (NAME_AT + I - 1));
            end loop;
            if not Valid_Name (Result.Name (1 .. Name_Count)) then
               return (Success => False, Why => Bad_Name);
            end if;
            return (Success => True, Value => Result);
         end;
      elsif Kind_Word = Element_Kind'Enum_Rep (Sample) then
         if Length /= Sample_Bytes then
            return (Success => False, Why => Bad_Length);
         end if;
         return (Success => True,
                 Value => (Kind => Sample, Key => Series_Key (Key_Word),
                           Time_Us => Get (From, TIME_AT, WORD_BYTES),
                           Value => Get (From, VALUE_AT, WORD_BYTES)));
      elsif Kind_Word = Element_Kind'Enum_Rep (Loss) then
         if Length /= Loss_Bytes then
            return (Success => False, Why => Bad_Length);
         end if;
         declare
            Shed : constant Unsigned_64 := Get (From, SHED_AT, WORD_BYTES);
            First : constant Unsigned_64 := Get (From, FIRST_AT, WORD_BYTES);
            Last : constant Unsigned_64 := Get (From, LAST_AT, WORD_BYTES);
         begin
            if Shed = 0 or else First > Last then
               return (Success => False, Why => Bad_Loss);
            end if;
            return (Success => True,
                    Value => (Kind => Loss, Key => Series_Key (Key_Word), Shed => Shed,
                              First_Us => First, Last_Us => Last));
         end;
      end if;
      return (Success => False, Why => Unknown_Kind);
   end Decode;
end CuBit.Metric_Outlet;
