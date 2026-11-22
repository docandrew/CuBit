pragma Ada_2022;
with AML_Decode;
package body ACPI_EC_Boot_Table with SPARK_Mode is
   use Interfaces;
   use type Firmware_Tables.Admission;
   Control_Offset : constant := 36;
   Data_Offset : constant := 48;
   UID_Offset : constant := 60;
   GPE_Offset : constant := 64;
   GAS_Size : constant := 12;
   GAS_Width_Offset : constant := 1;
   GAS_Bit_Offset : constant := 2;
   GAS_Access_Offset : constant := 3;
   GAS_Address_Offset : constant := 4;
   UID_Size : constant := 4;
   Address_Size : constant := 8;
   Minimum_Rooted_Path : constant := 2;
   Bits_Per_Byte : constant := 8;
   Byte_Width : constant := 8;
   Byte_Access : constant := 1;
   Unspecified_Access : constant := 0;
   System_Memory : constant := 0;
   System_IO : constant := 1;
   Revision_One : constant := 1;
   ASCII_Last : constant := 127;
   function Read_LE (Data : Firmware_Tables.Bytes; Offset : Natural;
                     Width : Positive) return Unsigned_64 with
     Pre => Width <= Address_Size and then Data'Length >= Width
       and then Offset <= Data'Length - Width
   is
      Value : Unsigned_64 := 0;
   begin
      for I in 0 .. Width - 1 loop
         Value := Value or Shift_Left
           (Unsigned_64 (Data (Data'First + Offset + I)), Bits_Per_Byte * I);
      end loop;
      return Value;
   end Read_LE;
   function GAS (Data : Firmware_Tables.Bytes; Offset : Natural)
                 return ACPI_FADT.Generic_Address with
     Pre => Data'Length >= GAS_Size and then Offset <= Data'Length - GAS_Size
   is
   begin
      return (Space => Data (Data'First + Offset),
              Width => Data (Data'First + Offset + GAS_Width_Offset),
              Bit_Offset => Data (Data'First + Offset + GAS_Bit_Offset),
              Access_Size => Data (Data'First + Offset + GAS_Access_Offset),
              Address => Read_LE (Data, Offset + GAS_Address_Offset, Address_Size));
   end GAS;
   function Decode (Data : Firmware_Tables.Bytes) return Description is
      Header : constant Firmware_Tables.Table_Result :=
        Firmware_Tables.Read_Table (Data, "ECDT");
   begin
      if Header.Status /= Firmware_Tables.Accepted
        or else Header.Extent /= Data'Length
      then
         return (Status => Invalid_Table);
      elsif Data'Length <= Fixed_Size then
         return (Status => Truncated_Fixed_Fields);
      end if;
      for Offset in Fixed_Size .. Data'Length - 1 loop
         if Data (Data'First + Offset) = 0 then
            return (Status => Decoded, Revision => Header.Revision,
                    Control => GAS (Data, Control_Offset),
                    Data => GAS (Data, Data_Offset),
                    UID => Unsigned_32 (Read_LE (Data, UID_Offset, UID_Size)),
                    GPE_Bit => Data (Data'First + GPE_Offset),
                    Path_Length => Offset - Fixed_Size,
                    Suffix_Length => Data'Length - Offset - 1);
         elsif Data (Data'First + Offset) > ASCII_Last then
            return (Status => Non_ASCII_Identifier);
         end if;
      end loop;
      return (Status => Missing_Terminator);
   end Decode;
   function Read_Path (Data : Firmware_Tables.Bytes) return Path_Result is
      Item : constant Description := Decode (Data);
      Count : Segment_Count := 0;
      Length : Natural range 0 .. AML_Names.Segment'Length := 0;
      Parts : AML_Names.Segment_Array := [others => "____"];
      C : Firmware_Tables.Byte;
   begin
      if Item.Status /= Decoded then
         return (Status => Invalid_Description);
      elsif Item.Path_Length < Minimum_Rooted_Path
        or else Data (Data'First + Fixed_Size) /= Character'Pos ('\')
      then
         return (Status => Invalid_Path);
      end if;
      Count := 1;
      for Position in 1 .. Item.Path_Length - 1 loop
         C := Data (Data'First + Fixed_Size + Position);
         if C = Character'Pos ('.') then
            if Length = 0 then return (Status => Invalid_Path); end if;
            if Count = Segment_Count'Last then return (Status => Path_Too_Deep); end if;
            Count := Count + 1;
            Length := 0;
         else
            if Length = AML_Names.Segment'Length
              or else (if Length = 0 then not AML_Names.Lead (AML_Decode.Byte (C))
                       else not AML_Names.Tail (AML_Decode.Byte (C)))
            then
               return (Status => Invalid_Path);
            end if;
            Length := Length + 1;
            Parts (Count) (Length) := Character'Val (C);
         end if;
      end loop;
      if Length = 0 then return (Status => Invalid_Path); end if;
      return (Status => Parsed, Count => Count, Parts => Parts);
   end Read_Path;
   function Check_Registers (Data : Firmware_Tables.Bytes) return Register_Status is
      Item : constant Description := Decode (Data);
      function Check (Address : ACPI_FADT.Generic_Address) return Register_Status is
      begin
         if Address.Space not in System_Memory | System_IO then
            return Unsupported_Space;
         elsif Address.Width /= Byte_Width or Address.Bit_Offset /= 0
           or Address.Access_Size not in Unspecified_Access | Byte_Access
         then
            return Unsupported_Access;
         elsif Address.Space = System_IO and Address.Address > Unsigned_64 (Unsigned_16'Last) then
            return IO_Address_Too_Wide;
         end if;
         return Byte_Registers_Described;
      end Check;
      Status : Register_Status;
   begin
      if Item.Status /= Decoded then return Invalid_Description;
      elsif Item.Revision /= Revision_One then return Unsupported_Revision;
      elsif Item.Suffix_Length /= 0 then return Unrecognized_Suffix;
      end if;
      Status := Check (Item.Control);
      if Status /= Byte_Registers_Described then return Status; end if;
      Status := Check (Item.Data);
      if Status /= Byte_Registers_Described then return Status; end if;
      if Item.Control.Space = Item.Data.Space and Item.Control.Address = Item.Data.Address then
         return Aliased_Registers;
      end if;
      return Byte_Registers_Described;
   end Check_Registers;
end ACPI_EC_Boot_Table;
