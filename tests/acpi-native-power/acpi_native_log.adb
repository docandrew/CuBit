with Power_Test_Log;
package body ACPI_Native_Log is
   procedure Info (Text : String) is
   begin Power_Test_Log.Emit (Text); end Info;
   procedure Warning (Text : String) is
   begin Power_Test_Log.Emit (Text); end Warning;
   procedure Critical (Text : String) is
   begin Power_Test_Log.Emit (Text); end Critical;
   procedure Grant_Notice (Item : CuBit.Control_Events.Event) is
      pragma Unreferenced (Item);
   begin null; end Grant_Notice;
end ACPI_Native_Log;
