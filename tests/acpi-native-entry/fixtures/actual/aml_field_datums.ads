pragma Ada_2022;
with AML_Decode;
with ACPI_FADT.Transactions;
package AML_Field_Datums with SPARK_Mode, Pure is
   subtype Scalar is AML_Decode.Integer_Value;
   subtype Width is ACPI_FADT.Transactions.Access_Width;
   type Bit_Position is new Scalar;
   type Bit_Length is new Scalar;
   type Byte_Position is new Scalar;
   type Datum_Index is new Scalar;
   subtype Local_Bit is Natural range 0 .. 63;
   subtype Local_Length is Positive range 1 .. 64;
   type Update_Rule is (Preserve, Write_As_Ones, Write_As_Zeros);
   type Plan is private;
   function Valid (Item : Plan) return Boolean;
   function Count (Item : Plan) return Datum_Index;
   -- Region length/base are geometry, never authority. Zero length is rejected.
   -- Bit positions are region-relative. The whole field and every access datum
   -- must fit; wrapping bit endpoints or address extents are rejected.
   function Prepare (Base, Region_Length : Scalar; Start : Bit_Position;
      Length : Bit_Length; Access_Width : Width) return Plan
      with Global => null,
        Post => (if Valid (Prepare'Result) then Count (Prepare'Result) > 0);
   type Datum is record
      Offset : Byte_Position;
      Source_Bit : Bit_Position;
      First_Bit : Local_Bit;
      Bits : Local_Length;
      Mask : Scalar;
      Access_Width : Width;
   end record;
   function At_Index (Item : Plan; Index : Datum_Index) return Datum
      with Global => null, Pre => Valid (Item) and then Index < Count (Item);
   function Width_Mask (Access_Width : Width) return Scalar with Global => null;
   -- Value is positioned in datum bits. Bits outside Mask are ignored.
   -- These formulas specify values only, not permission or hardware atomicity.
   function Merge (Access_Width : Width; Mask, Old_Value, Value : Scalar;
      Rule : Update_Rule) return Scalar with Global => null;
   function Needs_Read (Access_Width : Width; Mask : Scalar;
      Rule : Update_Rule) return Boolean with Global => null;
private
   use type Scalar;
   Bits_Per_Byte : constant := 8;
   Byte_Bits : constant := Bits_Per_Byte;
   Word_Bits : constant := 2 * Bits_Per_Byte;
   Dword_Bits : constant := 4 * Bits_Per_Byte;
   Qword_Bits : constant := 8 * Bits_Per_Byte;
   subtype Datum_Bit_Width is Local_Length
      with Static_Predicate => Datum_Bit_Width in 8 | 16 | 32 | 64;
   function Datum_Bits (Access_Width : Width) return Datum_Bit_Width is
     (Bits_Per_Byte * ACPI_FADT.Transactions.Octets (Access_Width));
   function Block_At (Position : Scalar; Access_Width : Width) return Scalar is
     (case Access_Width is
        when ACPI_FADT.Transactions.Byte_Access => Position / Byte_Bits,
        when ACPI_FADT.Transactions.Word_Access => Position / Word_Bits,
        when ACPI_FADT.Transactions.Dword_Access => Position / Dword_Bits,
        when ACPI_FADT.Transactions.Qword_Access => Position / Qword_Bits);
   function Local_Position (Position : Scalar; Access_Width : Width) return Local_Bit is
     (case Access_Width is
        when ACPI_FADT.Transactions.Byte_Access => Local_Bit (Position mod Byte_Bits),
        when ACPI_FADT.Transactions.Word_Access => Local_Bit (Position mod Word_Bits),
        when ACPI_FADT.Transactions.Dword_Access => Local_Bit (Position mod Dword_Bits),
        when ACPI_FADT.Transactions.Qword_Access => Local_Bit (Position mod Qword_Bits));
   function Block_First (Block : Scalar; Access_Width : Width) return Scalar is
     (case Access_Width is
        when ACPI_FADT.Transactions.Byte_Access => Block * Byte_Bits,
        when ACPI_FADT.Transactions.Word_Access => Block * Word_Bits,
        when ACPI_FADT.Transactions.Dword_Access => Block * Dword_Bits,
        when ACPI_FADT.Transactions.Qword_Access => Block * Qword_Bits);
   function Coherent (Item : Plan) return Boolean;
   type Plan is record
      Ready : Boolean := False;
      First_Byte : Scalar := 0;
      Start_Bit, Last_Bit : Scalar := 0;
      Number : Datum_Index := 0;
      Access_Width : Width := ACPI_FADT.Transactions.Byte_Access;
   end record with Type_Invariant => not Plan.Ready or else Coherent (Plan);
   -- Every admitted plan describes the complete aligned datum interval.
   -- Keep these constructor relationships at the private type boundary.
   function Coherent (Item : Plan) return Boolean is
     (Item.Start_Bit <= Item.Last_Bit
      and then Item.First_Byte =
        Block_At (Item.Start_Bit, Item.Access_Width) *
            Scalar (ACPI_FADT.Transactions.Octets (Item.Access_Width))
      and then Item.Number > 0
      and then Item.Number = Datum_Index
        (Block_At (Item.Last_Bit, Item.Access_Width) -
         Block_At (Item.Start_Bit, Item.Access_Width) + 1));
   function Valid (Item : Plan) return Boolean is (Item.Ready and then Coherent (Item));
   function Count (Item : Plan) return Datum_Index is (Item.Number);
end AML_Field_Datums;
