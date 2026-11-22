pragma Ada_2022;
with Interfaces;
-- Copied connection descriptions. No controller lookup or bus authority.
package ACPI_Resource_Data.Serial_Connections with SPARK_Mode, Pure is
   type Bus_Kind is (I2C_Bus, SPI_Bus, UART_Bus, CSI2_Bus, Vendor_Bus);
   type Resource_Role is (Produces_And_Consumes, Consumes_Resource);
   type Initiator is (Controller_Initiated, Device_Initiated);
   type Address_Mode is (Seven_Bit, Ten_Bit);
   type Wire_Mode is (Four_Wire, Three_Wire);
   type Signal_Polarity is (Active_Low, Active_High);
   type Clock_Phase is (First_Phase, Second_Phase);
   type Clock_Polarity is (Starts_Low, Starts_High);
   type Flow_Control is (No_Flow_Control, Hardware_Flow_Control, XON_XOFF);
   type Stop_Bits is (No_Stop_Bits, One_Stop_Bit, One_And_Half_Stop_Bits, Two_Stop_Bits);
   type Data_Bits is (Five_Bits, Six_Bits, Seven_Bits, Eight_Bits, Nine_Bits);
   type Byte_Order is (Little_Endian, Big_Endian);
   type Parity is (No_Parity, Even_Parity, Odd_Parity, Mark_Parity, Space_Parity);
   type PHY_Kind is (C_PHY, D_PHY);
   type Rate is new Interfaces.Unsigned_32;
   type Slave_Address is range 0 .. 1_023;
   type Device_Selection is new Interfaces.Unsigned_16;
   type FIFO_Bytes is new Interfaces.Unsigned_16;
   type Transfer_Bits is new Firmware_Tables.Byte;
   type Serial_Lines is new Firmware_Tables.Byte;
   type Local_Port is range 0 .. 63;
   type Descriptor_Revision is range 1 .. 2;
   type Byte_Span is record
      First : Byte_Offset := 0;
      Length : Byte_Count := 0;
   end record;
   type Connection_Information (Kind : Bus_Kind := I2C_Bus) is record
      case Kind is
         when I2C_Bus =>
            Addressing : Address_Mode;
            Address : Slave_Address;
            Connection_Speed : Rate;
            -- ACPI 6.6 LVR for I2C devices on I3C hosts. Host-specific validity
            -- (including required seven-bit mode) belongs to controller admission.
            Legacy_Virtual_Register : Firmware_Tables.Byte;
         when SPI_Bus =>
            Wires : Wire_Mode;
            Selection_Polarity : Signal_Polarity;
            Selection : Device_Selection;
            Transfer_Unit : Transfer_Bits;
            Phase : Clock_Phase;
            Clock : Clock_Polarity;
            Maximum_Speed : Rate;
         when UART_Bus =>
            Flow : Flow_Control;
            Stops : Stop_Bits;
            Bits : Data_Bits;
            Order : Byte_Order;
            Parity_Mode : Parity;
            Lines : Serial_Lines;
            Baud_Rate : Rate;
            Receive_FIFO, Transmit_FIFO : FIFO_Bytes;
         when CSI2_Bus =>
            PHY : PHY_Kind;
            Port : Local_Port;
         when Vendor_Bus => null;
      end case;
   end record;
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Unsupported_Revision, Unsupported_Bus_Type,
      Unsupported_Type_Revision, Reserved_Bits_Set, Bad_Source_Index,
      Bad_Type_Length, Missing_Source, Bad_Source_Termination,
      Bad_I2C_Address, Bad_SPI_Clock, Bad_UART_Format, Bad_CSI2_PHY);
   type Decode_Result (Status : Decode_Status := Unsupported_Descriptor) is record
      case Status is
         when Decoded =>
            Revision : Descriptor_Revision;
            Bus_Type, Type_Revision, Source_Index : Firmware_Tables.Byte;
            Specific_Flags : Interfaces.Unsigned_16;
            Role : Resource_Role;
            Initiated_By : Initiator;
            Shared : Boolean;
            Information : Connection_Information;
            -- Absolute spans in this Data copy; Source includes its final NUL.
            Vendor, Source : Byte_Span;
            Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- V1 and V2 I2C/SPI/UART, V1 CSI2 and common vendor types192..255.
   -- V2 sharing follows the ASL V2 macros; V1 bit2 is reserved. Vendor-specific
   -- data/revision semantics are opaque. Source syntax/identity is not resolved.
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result with Global => null,
      Post => (if Decode'Result.Status = Decoded then
         Decode'Result.Next_Offset > Offset
         and then Decode'Result.Next_Offset <= Byte_Count (Data'Length)
         and then Decode'Result.Vendor.First > Offset
         and then Decode'Result.Vendor.First + Decode'Result.Vendor.Length = Decode'Result.Source.First
         and then Decode'Result.Source.Length > 0
         and then Decode'Result.Source.First + Decode'Result.Source.Length = Decode'Result.Next_Offset);
end ACPI_Resource_Data.Serial_Connections;
