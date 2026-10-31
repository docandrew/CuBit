with Native_HID_Test_Log;
package body ACPI_Native_Log is
   procedure Info (Text : String) is
   begin Native_HID_Test_Log.Emit (Text, False); end Info;
   procedure Warning (Text : String) is
   begin Native_HID_Test_Log.Emit (Text, True); end Warning;
   procedure Critical (Text : String) is
   begin Native_HID_Test_Log.Emit (Text, True); end Critical;
   procedure Grant_Notice (Item : CuBit.Control_Events.Event) is
      pragma Unreferenced (Item);
   begin null; end Grant_Notice;
end ACPI_Native_Log;
