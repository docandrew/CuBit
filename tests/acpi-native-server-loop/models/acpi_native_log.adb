with ACPI_Log_Lifetime;
package body ACPI_Native_Log is
   procedure Grant_Notice (Item : CuBit.Control_Events.Event) is
   begin
      Notices := Notices + 1;
      if ACPI_Log_Lifetime.Related (Peer, Item) then Disconnects := Disconnects + 1; end if;
   end Grant_Notice;
   procedure Info (Text : String) is
      pragma Unreferenced (Text);
   begin null; end Info;
   procedure Warning (Text : String) is
      pragma Unreferenced (Text);
   begin null; end Warning;
   procedure Critical (Text : String) is
      pragma Unreferenced (Text);
   begin null; end Critical;
end ACPI_Native_Log;
