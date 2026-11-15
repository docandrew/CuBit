with Owner_Test_Control;
package body ACPI_Native_Log is
   procedure Info (Text : String) is
   begin Owner_Test_Control.Write (Owner_Test_Control.Information, Text); end Info;
   procedure Warning (Text : String) is
   begin Owner_Test_Control.Write (Owner_Test_Control.Warning, Text); end Warning;
   procedure Critical (Text : String) is
   begin Owner_Test_Control.Write (Owner_Test_Control.Critical, Text); end Critical;
   procedure Grant_Notice (Item : CuBit.Control_Events.Event) is
      pragma Unreferenced (Item);
   begin null; end Grant_Notice;
   procedure Copied(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False) is
      pragma Unreferenced(Peer_Quiet);
   begin Owner_Test_Control.Write((if Success then Owner_Test_Control.Information else Owner_Test_Control.Warning),Text);end Copied;
   function Copied_Dropped return Interfaces.Unsigned_64 is (0);
   procedure Drain_Diagnostics(IO_Pending,IO_Uncertain:Boolean;Drained:out Boolean) is
   begin Drained:=not IO_Pending and not IO_Uncertain;end Drain_Diagnostics;
   function Publisher_Dropped return Interfaces.Unsigned_64 is (0);
end ACPI_Native_Log;
