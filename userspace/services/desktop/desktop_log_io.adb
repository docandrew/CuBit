with CuBit.Messages;
with CuBit.Logging;
package body Desktop_Log_IO with SPARK_Mode => Off is
   Writer : CuBit.Logging.Publisher;
   procedure Echo (Text : String) is
   begin CuBit.Messages.debugPrint (Text); end Echo;
   procedure Emit (Value : CuBit.Log_Records.Log_Record; Accepted : out Boolean) is
   begin CuBit.Logging.Emit (Writer, Value, Accepted); end Emit;
end Desktop_Log_IO;
