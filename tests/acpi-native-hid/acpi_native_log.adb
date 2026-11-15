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
   procedure Drain_Diagnostics(IO_Pending,IO_Uncertain:Boolean;Drained:out Boolean) is
   begin Drained:=not IO_Pending and then not IO_Uncertain;end Drain_Diagnostics;
end ACPI_Native_Log;
