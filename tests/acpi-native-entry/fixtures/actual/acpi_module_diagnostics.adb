with Interfaces; use Interfaces;
package body ACPI_Module_Diagnostics with SPARK_Mode is
   use type Count;
   function Hex (Value : Unsigned_64) return String is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Text : String (1 .. 16);
      Rest : Unsigned_64 := Value;
   begin
      for I in reverse Text'Range loop
         Text (I) := Hex_Digits (Natural (Rest and 16#F#) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return Text;
   end Hex;
   function Format (Item : Observation) return String is
   begin
      if Item.Count = 0 then return "acpi: stage aml-recovery ok NONE"; end if;
      return "acpi: stage aml-recovery ok " & Item.Kind'Image &
        " count=" & Item.Count'Image & " definition=" & Item.Definition'Image &
        " resume=0x" & Hex (Unsigned_64 (Item.Resume_Offset)) &
        " charged=0x" & Hex (Unsigned_64 (Item.Charged));
   end Format;
end ACPI_Module_Diagnostics;
