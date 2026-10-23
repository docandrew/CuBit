with CuBit.Messages;
with CuBit.Logging;
with Startup;
package body Desktop_Log_IO with SPARK_Mode => Off is
   --  Bound when declared (CAP-003): there is no unbound publisher.
   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   procedure Echo (Text : String) is
   begin CuBit.Messages.debugPrint (Text); end Echo;
   procedure Emit (Value : CuBit.Log_Records.Log_Record; Accepted : out Boolean) is
   begin CuBit.Logging.Emit (Writer, Value, Accepted); end Emit;
   procedure Flush is
      Drained : Boolean;
      pragma Unreferenced (Drained);
   begin CuBit.Logging.Flush (Writer, Drained); end Flush;
end Desktop_Log_IO;
