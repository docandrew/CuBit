with ACPI_Native_Log;
package body ACPI_Native_Diagnostic_Output is
   procedure Write (Text : String; Peer_Quiet : Boolean; Success : Boolean := False) is
   begin
      ACPI_Native_Log.Copied(Text,Peer_Quiet,Success);
   end Write;
end ACPI_Native_Diagnostic_Output;
