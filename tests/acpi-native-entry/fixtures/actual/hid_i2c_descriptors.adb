pragma Ada_2022;
package body HID_I2C_Descriptors with SPARK_Mode is
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   subtype Word_Offset is Natural range 0 .. Descriptor_Bytes - 2;
   function Word (Data : Bytes; Offset : Word_Offset)
      return Interfaces.Unsigned_16
   is (Interfaces.Unsigned_16 (Data (Data'First + Offset))
       or Interfaces.Shift_Left
         (Interfaces.Unsigned_16 (Data (Data'First + (Offset + 1))), 8))
      with Pre => Data'Length = Descriptor_Bytes;
   function Decode (Data : Bytes) return Decode_Result is
      Length_Offset : constant Word_Offset := 0;
      Version_Offset : constant Word_Offset := 2;
      Report_Length_Offset : constant Word_Offset := 4;
      Report_Register_Offset : constant Word_Offset := 6;
      Input_Register_Offset : constant Word_Offset := 8;
      Input_Length_Offset : constant Word_Offset := 10;
      Output_Register_Offset : constant Word_Offset := 12;
      Output_Length_Offset : constant Word_Offset := 14;
      Command_Register_Offset : constant Word_Offset := 16;
      Data_Register_Offset : constant Word_Offset := 18;
      Vendor_Offset : constant Word_Offset := 20;
      Product_Offset : constant Word_Offset := 22;
      Firmware_Offset : constant Word_Offset := 24;
      Reserved_Low_Offset : constant Word_Offset := 26;
      Reserved_High_Offset : constant Word_Offset := 28;
   begin
      if Data'Length < Descriptor_Bytes then
         return (Status => Short_Transfer);
      elsif Data'Length > Descriptor_Bytes then
         return (Status => Trailing_Data);
      elsif Word (Data, Length_Offset) /=
        Interfaces.Unsigned_16 (Descriptor_Bytes) then
         return (Status => Invalid_Length);
      elsif Word (Data, Version_Offset) /= Version_1_00 then
         return (Status => Unsupported_Version);
      end if;
      return (Status => Decoded, Value =>
        (Report_Length => Byte_Count (Word (Data, Report_Length_Offset)),
         Report_Register => Register_Number (Word (Data, Report_Register_Offset)),
         Input_Register => Register_Number (Word (Data, Input_Register_Offset)),
         Maximum_Input => Byte_Count (Word (Data, Input_Length_Offset)),
         Output_Register => Register_Number (Word (Data, Output_Register_Offset)),
         Maximum_Output => Byte_Count (Word (Data, Output_Length_Offset)),
         Command_Register => Register_Number (Word (Data, Command_Register_Offset)),
         Data_Register => Register_Number (Word (Data, Data_Register_Offset)),
         Vendor_ID => Word (Data, Vendor_Offset),
         Product_ID => Word (Data, Product_Offset),
         Firmware_Version => Word (Data, Firmware_Offset),
         Reserved => Interfaces.Unsigned_32 (Word (Data, Reserved_Low_Offset))
           or Interfaces.Shift_Left
             (Interfaces.Unsigned_32 (Word (Data, Reserved_High_Offset)), 16)));
   end Decode;
   function Check_Capacities
     (Value : Descriptor;
      Report_Capacity, Input_Capacity, Output_Capacity : Byte_Count)
      return Capacity_Status is
   begin
      if Value.Report_Length = 0 then return Empty_Report;
      elsif Value.Maximum_Input < Length_Prefix_Bytes then
         return Invalid_Input_Length;
      elsif Value.Maximum_Output /= 0
        and then Value.Maximum_Output < Length_Prefix_Bytes then
         return Invalid_Output_Length;
      elsif Value.Report_Length > Report_Capacity then return Report_Too_Large;
      elsif Value.Maximum_Input > Input_Capacity then return Input_Too_Large;
      elsif Value.Maximum_Output > Output_Capacity then return Output_Too_Large;
      else return Fits;
      end if;
   end Check_Capacities;
end HID_I2C_Descriptors;
