with Interfaces;
with CuBit.Messages;
with Observatory_Trace_View;
generic
   Capability : CuBit.Messages.CapabilitySlot;
package Observatory_Archive_Reader with SPARK_Mode is
   -- Archive reads have their own budget; live metrics retain 250 ms.
   Timeout_Us : constant Interfaces.Unsigned_64 := 2_000_000;
   type Result_Kind is (Idle, Loading, Complete, Incomplete, Unavailable);
   function Status return Result_Kind;
   function Cleanup_Pending return Boolean;
   function Matches (Token : Interfaces.Unsigned_64) return Boolean;
   procedure Start (Page : Observatory_Trace_View.Page_Number; Accepted : out Boolean);
   procedure Tick (Sequence : in out Interfaces.Unsigned_64; Now_Us : Interfaces.Unsigned_64);
   procedure Collect (Value : CuBit.Messages.CompletionEntry);
   procedure Take (View : out Observatory_Trace_View.State; Success : out Boolean);
   procedure Close;
end Observatory_Archive_Reader;
