with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Messages;
with Startup;
package body ACPI_Native_Log is
   Logger : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   procedure Write (Text : String; Level : CuBit.Log_Records.Severity) is
      Value : constant CuBit.Log_Records.Decoded :=
        CuBit.Log_Records.Make (Text, Level);
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint (Text & ASCII.LF);
      if Value.Success then
         CuBit.Logging.Emit (Logger, Value.Value, Submitted);
      end if;
   end Write;
   procedure Info (Text : String) is
   begin
      Write (Text, CuBit.Log_Records.Information);
   end Info;
   procedure Warning (Text : String) is
   begin
      Write (Text, CuBit.Log_Records.Warning);
   end Warning;
   procedure Critical (Text : String) is
   begin
      Write (Text, CuBit.Log_Records.Critical);
   end Critical;
end ACPI_Native_Log;
