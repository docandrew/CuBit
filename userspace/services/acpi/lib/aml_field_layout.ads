pragma Ada_2022;
with AML_Decode;
with ACPI_FADT.Transactions;
-- Geometry only: supplied register geometry is NOT an authorization token.
package AML_Field_Layout with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Byte;
   use type ACPI_FADT.Transactions.Access_Width;
   Bits_Per_Byte : constant := 8;
   Memory_Space : constant AML_Decode.Byte := 0;
   IO_Space : constant AML_Decode.Byte := 1;
   IO_Last_Address : constant AML_Decode.Integer_Value := 16#FFFF#;
   type Field_Form is (Ordinary_Field, Bank_Field, Index_Field);
   type Region_Geometry is record
      Space : AML_Decode.Byte := Memory_Space;
      Base, Length : AML_Decode.Integer_Value := 0;
   end record;
   type Field_Geometry is record
      Region : Region_Geometry;
      Bit_Offset, Bit_Length : AML_Decode.Integer_Value := 0;
      Flags, Effective_Access, Attribute, Access_Length : AML_Decode.Byte := 0;
      Form : Field_Form := Ordinary_Field;
      Has_Connection : Boolean := False;
   end record;
   type Register_Geometry is record
      Region : Region_Geometry;
      Byte_Offset : AML_Decode.Integer_Value := 0;
      Width : ACPI_FADT.Transactions.Access_Width := ACPI_FADT.Transactions.Byte_Access;
   end record;
   -- Total decision. Never returns an address, register ID or capability.
   function Admitted (Field : Field_Geometry; Register : Register_Geometry)
      return Boolean with Global => null,
      Post => (if Admitted'Result then
         Field.Form = Ordinary_Field and then not Field.Has_Connection
         and then Field.Region = Register.Region
         and then Field.Region.Length /= 0
         and then Field.Region.Space in Memory_Space | IO_Space
         and then (if Register.Width = ACPI_FADT.Transactions.Qword_Access
           then Field.Region.Space = Memory_Space)
         and then Field.Bit_Offset mod Bits_Per_Byte = 0
         and then Field.Bit_Offset / Bits_Per_Byte = Register.Byte_Offset
         and then Field.Bit_Length = AML_Decode.Integer_Value
           (Bits_Per_Byte * ACPI_FADT.Transactions.Octets (Register.Width))
         and then Field.Attribute = 0 and then Field.Access_Length = 0
         and then Field.Region.Base <= AML_Decode.Integer_Value'Last - (Field.Region.Length - 1)
         and then Register.Byte_Offset < Field.Region.Length
         and then AML_Decode.Integer_Value (ACPI_FADT.Transactions.Octets (Register.Width))
           <= Field.Region.Length - Register.Byte_Offset
         and then ACPI_FADT.Transactions.Aligned
           (Field.Region.Base + Register.Byte_Offset, Register.Width));
end AML_Field_Layout;
