pragma Ada_2022;
with Interfaces;
-- Transport bytes, not AML objects. No controller access or hardware authority.
package HID_I2C_Descriptors with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype Byte is Interfaces.Unsigned_8;
   type Bytes is array (Natural range <>) of Byte;
   type Register_Number is new Interfaces.Unsigned_16;
   type Byte_Count is range 0 .. 65_535;
   Descriptor_Bytes : constant Positive := 30;
   Version_1_00 : constant Interfaces.Unsigned_16 := 16#0100#;
   Length_Prefix_Bytes : constant Byte_Count := 2;
   type Descriptor is record
      Report_Length : Byte_Count;
      Report_Register, Input_Register : Register_Number;
      Maximum_Input : Byte_Count;
      Output_Register : Register_Number;
      Maximum_Output : Byte_Count;
      Command_Register, Data_Register : Register_Number;
      Vendor_ID, Product_ID, Firmware_Version : Interfaces.Unsigned_16;
      Reserved : Interfaces.Unsigned_32;
   end record;
   type Decode_Status is
     (Decoded, Short_Transfer, Trailing_Data, Invalid_Length, Unsupported_Version);
   type Decode_Result (Status : Decode_Status := Short_Transfer) is record
      case Status is
         when Decoded => Value : Descriptor;
         when others => null;
      end case;
   end record;

   -- Ghost wire model: fifteen consecutive little-endian words. Arithmetic
   -- byte weights are independent of the implementation's shift/or operations.
   Octet_Bits : constant Positive := 8;
   Bytes_Per_Word : constant Positive := 2;
   Byte_Scale : constant Positive := 2 ** Octet_Bits;
   Word_Scale : constant Interfaces.Unsigned_64 := 2 ** (Octet_Bits * Bytes_Per_Word);
   type Wire_Word is (Length_Field, Version_Field, Report_Length_Field,
      Report_Register_Field, Input_Register_Field, Input_Length_Field,
      Output_Register_Field, Output_Length_Field, Command_Register_Field,
      Data_Register_Field, Vendor_Field, Product_Field, Firmware_Field,
      Reserved_Low_Field, Reserved_High_Field) with Ghost;
   function Model_Word (Data : Bytes; Field : Wire_Word)
      return Interfaces.Unsigned_16 is
     (Interfaces."+"
        (Interfaces.Unsigned_16 (Data (Data'First + Bytes_Per_Word * Wire_Word'Pos (Field))),
         Interfaces."*" (Interfaces.Unsigned_16 (Byte_Scale), Interfaces.Unsigned_16
           (Data (Data'First + (Bytes_Per_Word * Wire_Word'Pos (Field) + 1))))))
     with Ghost, Global => null,
       Pre => Interfaces.Unsigned_64 (Data'Length) = Interfaces.Unsigned_64 (Descriptor_Bytes);
   function Model_Descriptor (Data : Bytes) return Descriptor is
     (Report_Length => Byte_Count (Model_Word (Data, Report_Length_Field)),
      Report_Register => Register_Number (Model_Word (Data, Report_Register_Field)),
      Input_Register => Register_Number (Model_Word (Data, Input_Register_Field)),
      Maximum_Input => Byte_Count (Model_Word (Data, Input_Length_Field)),
      Output_Register => Register_Number (Model_Word (Data, Output_Register_Field)),
      Maximum_Output => Byte_Count (Model_Word (Data, Output_Length_Field)),
      Command_Register => Register_Number (Model_Word (Data, Command_Register_Field)),
      Data_Register => Register_Number (Model_Word (Data, Data_Register_Field)),
      Vendor_ID => Model_Word (Data, Vendor_Field),
      Product_ID => Model_Word (Data, Product_Field),
      Firmware_Version => Model_Word (Data, Firmware_Field),
      Reserved => Interfaces.Unsigned_32
        (Interfaces.Unsigned_64 (Model_Word (Data, Reserved_Low_Field))
         + Word_Scale * Interfaces.Unsigned_64 (Model_Word (Data, Reserved_High_Field))))
     with Ghost, Global => null,
       Pre => Interfaces.Unsigned_64 (Data'Length) = Interfaces.Unsigned_64 (Descriptor_Bytes);
   function Model_Status (Data : Bytes) return Decode_Status is
     (if Interfaces.Unsigned_64 (Data'Length) < Interfaces.Unsigned_64 (Descriptor_Bytes) then Short_Transfer
      elsif Interfaces.Unsigned_64 (Data'Length) > Interfaces.Unsigned_64 (Descriptor_Bytes) then Trailing_Data
      elsif Interfaces."/=" (Model_Word (Data, Length_Field), Interfaces.Unsigned_16 (Descriptor_Bytes))
      then Invalid_Length
      elsif Interfaces."/=" (Model_Word (Data, Version_Field), Version_1_00) then Unsupported_Version
      else Decoded)
     with Ghost, Global => null;
   -- One exact v1.00 descriptor; all fields and reserved bytes correspond to
   -- the wire model. Zero/repeated registers remain representable. Decode
   -- establishes no suitability, report semantics, readiness or authority.
   function Decode (Data : Bytes) return Decode_Result with Global => null,
     Post => Decode'Result.Status = Model_Status (Data)
       and then (if Decode'Result.Status = Decoded then
          Decode'Result.Value = Model_Descriptor (Data));

   type Capacity_Status is
     (Fits, Empty_Report, Invalid_Input_Length, Invalid_Output_Length,
      Report_Too_Large, Input_Too_Large, Output_Too_Large);
   function Valid_Lengths (Value : Descriptor) return Boolean is
     (Value.Report_Length > 0 and then Value.Maximum_Input >= Length_Prefix_Bytes
      and then (Value.Maximum_Output = 0 or else Value.Maximum_Output >= Length_Prefix_Bytes))
     with Ghost, Global => null;
   -- Separate caller-capacity policy, including exact refusal precedence.
   -- Input/output include the length prefix. Output zero is optional, no IO.
   function Check_Capacities
     (Value : Descriptor;
      Report_Capacity, Input_Capacity, Output_Capacity : Byte_Count)
      return Capacity_Status with Global => null,
     Contract_Cases =>
       (Value.Report_Length = 0 => Check_Capacities'Result = Empty_Report,
        Value.Report_Length > 0 and then Value.Maximum_Input < Length_Prefix_Bytes =>
          Check_Capacities'Result = Invalid_Input_Length,
        Value.Report_Length > 0 and then Value.Maximum_Input >= Length_Prefix_Bytes
          and then Value.Maximum_Output > 0 and then Value.Maximum_Output < Length_Prefix_Bytes =>
          Check_Capacities'Result = Invalid_Output_Length,
        Valid_Lengths (Value) and then Value.Report_Length > Report_Capacity =>
          Check_Capacities'Result = Report_Too_Large,
        Valid_Lengths (Value) and then Value.Report_Length <= Report_Capacity
          and then Value.Maximum_Input > Input_Capacity => Check_Capacities'Result = Input_Too_Large,
        Valid_Lengths (Value) and then Value.Report_Length <= Report_Capacity
          and then Value.Maximum_Input <= Input_Capacity and then Value.Maximum_Output > Output_Capacity =>
          Check_Capacities'Result = Output_Too_Large,
        others => Check_Capacities'Result = Fits);
end HID_I2C_Descriptors;
