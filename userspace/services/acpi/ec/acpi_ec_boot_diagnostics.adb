with Interfaces;
with ACPI_EC_Boot_Table;
package body ACPI_EC_Boot_Diagnostics with SPARK_Mode is
   package O renames ACPI_EC_Boot_Observations;
   package E renames ACPI_EC_Boot_Table;
   use type O.Outcome, E.Decode_Status, E.Path_Status, E.Register_Status;
   use type Interfaces.Unsigned_64;
   Maximum_Hex_Digits : constant Positive := 16;
   UID_Hex_Digits : constant Positive := 8;
   Byte_Hex_Digits : constant Positive := 2;
   Bits_Per_Hex_Digit : constant := 4;
   Hex_Mask : constant Interfaces.Unsigned_64 := 15;
   function Hex (Value : Interfaces.Unsigned_64; Number_Digits : Positive) return String
     with Pre => Number_Digits <= Maximum_Hex_Digits
   is
      Alphabet : constant String := "0123456789ABCDEF";
      Text : String (1 .. Number_Digits);
      Remaining : Interfaces.Unsigned_64 := Value;
   begin
      for Index in reverse Text'Range loop
         Text (Index) := Alphabet (Natural (Remaining and Hex_Mask) + 1);
         Remaining := Interfaces.Shift_Right (Remaining, Bits_Per_Hex_Digit);
      end loop;
      return Text;
   end Hex;
   function Described (Item : O.Report) return Boolean is
     (Item.Status = O.Inspected and then Item.Description.Status = E.Decoded
      and then Item.Path.Status = E.Parsed
      and then Item.Registers = E.Byte_Registers_Described);
   function Format (Item : O.Report) return String is
      Prefix : constant String := "acpi: stage ec-boot " &
        (if Described (Item) or else Item.Status = O.Missing_ECDT then "ok " else "fail ") &
        Item.Status'Image & " table=" & Item.Table'Image &
        " bytes=" & Item.Required_Bytes'Image;
   begin
      if Item.Status /= O.Inspected then return Prefix; end if;
      if Item.Description.Status /= E.Decoded then
         return Prefix & " decode=" & Item.Description.Status'Image;
      end if;
      return Prefix & " path=" & Item.Path.Status'Image &
        " registers=" & Item.Registers'Image &
        " control-space=" & Item.Description.Control.Space'Image &
        " control=0x" & Hex (Item.Description.Control.Address, Maximum_Hex_Digits) &
        " data-space=" & Item.Description.Data.Space'Image &
        " data=0x" & Hex (Item.Description.Data.Address, Maximum_Hex_Digits) &
        " uid=0x" & Hex (Interfaces.Unsigned_64 (Item.Description.UID), UID_Hex_Digits) &
        " gpe-bit=0x" & Hex (Interfaces.Unsigned_64 (Item.Description.GPE_Bit), Byte_Hex_Digits) &
        " authority=NOT_ESTABLISHED";
   end Format;
end ACPI_EC_Boot_Diagnostics;
