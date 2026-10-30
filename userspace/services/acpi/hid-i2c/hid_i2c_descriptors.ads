pragma Ada_2022;
with Interfaces;
-- Transport bytes, not AML objects. No controller access or hardware authority.
package HID_I2C_Descriptors with SPARK_Mode, Pure is
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
   -- One complete v1.00 descriptor, little endian. Preserve reserved bytes for
   -- diagnostics; they neither select a behavior nor invalidate known fields.
   -- Register zero and repeated register numbers are representable. Decoding
   -- does not establish suitability, report semantics, readiness or identity.
   function Decode (Data : Bytes) return Decode_Result with Global => null;
   type Capacity_Status is
     (Fits, Empty_Report, Invalid_Input_Length, Invalid_Output_Length,
      Report_Too_Large, Input_Too_Large, Output_Too_Large);
   -- Separate host policy, with caller-selected capacities rather than silent
   -- truncation. Input/output lengths already include their two-byte prefix.
   -- Zero output length is supported; output is optional. No allocation or IO.
   function Check_Capacities
     (Value : Descriptor;
      Report_Capacity, Input_Capacity, Output_Capacity : Byte_Count)
      return Capacity_Status with Global => null;
end HID_I2C_Descriptors;
