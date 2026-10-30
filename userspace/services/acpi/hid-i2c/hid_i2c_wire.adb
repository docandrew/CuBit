pragma Ada_2022;
package body HID_I2C_Wire with SPARK_Mode is
   use type D.Register_Number;
   Reset_Opcode : constant D.Byte := 16#01#;
   Power_Opcode : constant D.Byte := 16#08#;
   Awake_Value : constant D.Byte := 0;
   Sleep_Value : constant D.Byte := 1;
   Byte_Radix : constant D.Register_Number := 256;
   function Select_Register (Number : D.Register_Number) return Register_Selector is
     ([D.Byte (Number mod Byte_Radix), D.Byte (Number / Byte_Radix)]);
   function Encode_Control (Number : D.Register_Number; Kind : Control)
     return Control_Message is
      Selector : constant Register_Selector := Select_Register (Number);
   begin
      return [Selector (0), Selector (1),
        (if Kind = Sleep then Sleep_Value else Awake_Value),
        (if Kind = Reset then Reset_Opcode else Power_Opcode)];
   end Encode_Control;
   function Buffer_Capacity (Data : D.Bytes) return D.Byte_Count is
   begin
      if Data'Last < Data'First then return 0;
      elsif Data'Last - Data'First >= Natural (D.Byte_Count'Last) - 1 then
         return D.Byte_Count'Last;
      else return D.Byte_Count (Data'Last - Data'First + 1);
      end if;
   end Buffer_Capacity;
   function Inspect_Input (Data : D.Bytes; Maximum_Input : D.Byte_Count;
      Reset_Pending : Boolean) return Frame_Result is
      Result : Frame_Result;
      Length : D.Byte_Count;
      Wire_Radix : constant Positive := 256;
   begin
      if Maximum_Input < D.Length_Prefix_Bytes then
         Result.Status := Invalid_Maximum; return Result;
      end if;
      --  Compare bounds rather than converting 'Length to Natural: a legal
      --  array indexed0..Natural'Last has Natural'Last+1 elements.
      if Data'Last <= Data'First then return Result; end if;
      Length := D.Byte_Count (Data (Data'First)) +
        D.Byte_Count (Data (Data'First + 1)) * D.Byte_Count (Wire_Radix);
      Result.Declared_Length := Length;
      if Length = 0 then
         Result.Status := (if Reset_Pending then Reset_Completed else Unexpected_Reset);
      elsif Length < D.Length_Prefix_Bytes then Result.Status := Invalid_Length;
      elsif Length > Maximum_Input then Result.Status := Exceeds_Maximum;
      elsif Natural (Length) - 1 > Data'Last - Data'First then Result.Status := Truncated;
      elsif Length = D.Length_Prefix_Bytes then Result.Status := Empty_Frame;
      else
         Result.Payload_Length := Length - D.Length_Prefix_Bytes;
         Result.Status := Input_Report;
      end if;
      return Result;
   end Inspect_Input;
end HID_I2C_Wire;
