pragma Ada_2022;
package body AML_Field_Layout with SPARK_Mode is
   Access_Mask : constant AML_Decode.Byte := 16#0F#;
   Lock_Mask : constant AML_Decode.Byte := 16#10#;
   Update_Mask : constant AML_Decode.Byte := 16#60#;
   Reserved_Mask : constant AML_Decode.Byte := 16#80#;
   Reserved_Update : constant AML_Decode.Byte := 16#60#;
   Last_Defined_Access : constant AML_Decode.Byte := 5;
   Any_Access_Code : constant AML_Decode.Byte := 0;
   Byte_Access_Code : constant AML_Decode.Byte := 1;
   Word_Access_Code : constant AML_Decode.Byte := 2;
   Dword_Access_Code : constant AML_Decode.Byte := 3;
   Qword_Access_Code : constant AML_Decode.Byte := 4;
   function Admitted (Field : Field_Geometry; Register : Register_Geometry)
      return Boolean is
      Size : constant AML_Decode.Integer_Value :=
         AML_Decode.Integer_Value (ACPI_FADT.Transactions.Octets (Register.Width));
      Address : AML_Decode.Integer_Value;
      Expected_Access : AML_Decode.Byte;
   begin
      if Field.Form /= Ordinary_Field or else Field.Has_Connection
        or else Field.Region /= Register.Region
        or else Field.Region.Space not in Memory_Space | IO_Space
        or else Field.Region.Length = 0
        or else (Register.Width = ACPI_FADT.Transactions.Qword_Access
          and then Field.Region.Space /= Memory_Space)
      then return False; end if;
      if (Field.Flags and Reserved_Mask) /= 0
        or else (Field.Flags and Lock_Mask) /= 0
        or else (Field.Flags and Update_Mask) = Reserved_Update
        or else (Field.Flags and Access_Mask) > Last_Defined_Access
        or else Field.Attribute /= 0 or else Field.Access_Length /= 0
      then return False; end if;
      Expected_Access := (case Register.Width is
         when ACPI_FADT.Transactions.Byte_Access => Byte_Access_Code,
         when ACPI_FADT.Transactions.Word_Access => Word_Access_Code,
         when ACPI_FADT.Transactions.Dword_Access => Dword_Access_Code,
         when ACPI_FADT.Transactions.Qword_Access => Qword_Access_Code);
      -- Normal ACPICA implements AnyAcc with byte access. This admission is
      -- exact byte geometry only; it never widens an AnyAcc request.
      if (Field.Effective_Access /= Expected_Access
          and then not (Field.Effective_Access = Any_Access_Code
            and then Register.Width = ACPI_FADT.Transactions.Byte_Access))
        or else Field.Bit_Offset mod Bits_Per_Byte /= 0
        or else Field.Bit_Offset / Bits_Per_Byte /= Register.Byte_Offset
        or else Field.Bit_Length /= Size * Bits_Per_Byte
        or else Register.Byte_Offset >= Field.Region.Length
        or else Size > Field.Region.Length - Register.Byte_Offset
      then return False; end if;
      -- Validate the entire declared extent before any modular addition.
      if Field.Region.Base > AML_Decode.Integer_Value'Last - (Field.Region.Length - 1)
      then return False; end if;
      Address := Field.Region.Base + Register.Byte_Offset;
      if not ACPI_FADT.Transactions.Aligned (Address, Register.Width)
        or else (Field.Region.Space = IO_Space and then
          (Address > IO_Last_Address or else Size - 1 > IO_Last_Address - Address))
      then return False; end if;
      return True;
   end Admitted;
end AML_Field_Layout;
