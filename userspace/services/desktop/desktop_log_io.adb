with CuBit.Messages;
with CuBit.Logging;
with Startup;
package body Desktop_Log_IO with SPARK_Mode => Off is
   Writer : CuBit.Logging.Publisher;
   procedure Echo (Text : String) is
   begin CuBit.Messages.debugPrint (Text); end Echo;
   procedure Emit (Value : CuBit.Log_Records.Log_Record; Accepted : out Boolean) is
   begin CuBit.Logging.Emit (Writer, Value, Accepted); end Emit;
   procedure Flush is
      Drained : Boolean;
      pragma Unreferenced (Drained);
   begin CuBit.Logging.Flush (Writer, Drained); end Flush;
begin
   --  An unbound publisher drops every record, so bind before the first
   --  Emit (the startup table is loaded before the program's units
   --  elaborate).
   CuBit.Logging.Bind (Writer, Startup.Logstore);
end Desktop_Log_IO;
