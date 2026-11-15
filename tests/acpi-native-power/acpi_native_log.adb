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
   procedure Drain_Diagnostics(IO_Pending,IO_Uncertain:Boolean;Drained:out Boolean) is
   begin Drained:=not IO_Pending and then not IO_Uncertain;end Drain_Diagnostics;
   procedure Copied (Text : String; Peer_Quiet : Boolean; Success : Boolean := False) is
      pragma Unreferenced (Peer_Quiet, Success);
   begin Power_Test_Log.Emit (Text); end Copied;
   function Copied_Dropped return Interfaces.Unsigned_64 is (0);
   function Publisher_Dropped return Interfaces.Unsigned_64 is (0);
end ACPI_Native_Log;
