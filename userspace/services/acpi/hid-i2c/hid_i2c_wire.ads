pragma Ada_2022;
with HID_I2C_Descriptors;
--  Bus wire values only, never CPU/MMIO addresses or hardware authority.
--  A future I2C/HID driver consumes ACPI discovery facts and uses these codecs;
--  the ACPI service does not become the input or bus controller driver.
package HID_I2C_Wire with SPARK_Mode, Pure is
   package D renames HID_I2C_Descriptors;
   use type D.Byte_Count;
   subtype Register_Selector is D.Bytes (0 .. 1);
   subtype Control_Message is D.Bytes (0 .. 3);
   type Control is (Reset, Power_On, Sleep);
   function Select_Register (Number : D.Register_Number) return Register_Selector
     with Global => null;
   --  Whole write: little-endian command-register selector, reserved/type/id
   --  byte (power state for SET_POWER), opcode. No arbitrary opcode interface.
   function Encode_Control (Number : D.Register_Number; Kind : Control)
     return Control_Message with Global => null;
   type Frame_Status is (Input_Report, Empty_Frame, Reset_Completed,
      Unexpected_Reset, Invalid_Maximum, Short_Prefix, Invalid_Length,
      Exceeds_Maximum, Truncated);
   type Frame_Result is record
      Status : Frame_Status := Short_Prefix;
      Declared_Length : D.Byte_Count := 0;
      Payload_Length : D.Byte_Count := 0;
   end record;
   Payload_Offset : constant Natural := Natural (D.Length_Prefix_Bytes);
   -- Clamp buffer capacity to the protocol word range without converting an
   -- unbounded array's element count to Natural. This is NOT a transfer size.
   function Buffer_Capacity (Data : D.Bytes) return D.Byte_Count
     with Global => null,
       Post => (if Data'Last < Data'First then Buffer_Capacity'Result = 0
         elsif Data'Last - Data'First >= Natural (D.Byte_Count'Last) - 1 then
           Buffer_Capacity'Result = D.Byte_Count'Last
         else Natural (Buffer_Capacity'Result) = Data'Last - Data'First + 1);
   --  Data is the actual completed transfer, not an unfilled buffer capacity.
   --  IO owner must first validate the controller's completion/byte count.
   --  Length includes its two-byte prefix; trailing bus padding is ignored.
   --  Reset completion is admitted only while the owner awaits reset. It is
   --  never forwarded as an input report; unsolicited reset requires recovery.
   --  Report ID/fields and touchpad usages require the separate HID descriptor.
   function Inspect_Input (Data : D.Bytes; Maximum_Input : D.Byte_Count;
      Reset_Pending : Boolean) return Frame_Result
     with Global => null,
       Post => (if Inspect_Input'Result.Status = Input_Report then
         Data'First < Data'Last
         and then Inspect_Input'Result.Payload_Length > 0
         and then Natural (Inspect_Input'Result.Payload_Length) <=
           Data'Last - Data'First - (Payload_Offset - 1)
         else Inspect_Input'Result.Payload_Length = 0);
end HID_I2C_Wire;
