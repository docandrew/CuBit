with CuBit.Messages;
with CuBit.Logging;
with CuBit.Console_Route;
with Startup;
package body Desktop_Log_IO with SPARK_Mode => Off is
   --  Bound when declared (CAP-003): there is no unbound publisher. Used
   --  only before main routes Desktop's console text (CuBit.Console_Route,
   --  ABI-002); from then on records go into the route's ring, the one
   --  Desktop's debugPrint text shares, so its lines keep their order.
   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   --  Routed, the text is not echoed: each line it ends is published
   --  (Desktop_Logs) as a record marked as console text, which test
   --  builds' logstore mirrors onto the serial console once.
   procedure Echo (Text : String) is
   begin
      if not CuBit.Console_Route.Routed then
         CuBit.Messages.debugPrint (Text);
      end if;
   end Echo;
   procedure Emit (Value : CuBit.Log_Records.Log_Record; Accepted : out Boolean) is
   begin
      if CuBit.Console_Route.Routed then
         CuBit.Console_Route.Emit (Value, Console => True, Accepted => Accepted);
      else
         CuBit.Logging.Emit (Writer, Value, Accepted);
      end if;
   end Emit;
   procedure Flush is
      Drained : Boolean;
      pragma Unreferenced (Drained);
   begin
      if CuBit.Console_Route.Routed then
         CuBit.Console_Route.Flush;
      end if;
      CuBit.Logging.Flush (Writer, Drained);
   end Flush;
end Desktop_Log_IO;
