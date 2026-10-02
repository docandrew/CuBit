with Interfaces;
with CuBit.Messages;
with CuBit.Metric_Protocol;
with Observatory_Metric_Queries;
generic
   Capability : CuBit.Messages.CapabilitySlot;
package Observatory_Metric_Observer with SPARK_Mode => Off is
   -- Single event-loop owner, static page lifetime. No call waits for a reply.
   -- Sequence is shared with every other async route in the application.
   procedure Begin_Query (Cursor : Observatory_Metric_Queries.Cursor;
      Sequence : in out Interfaces.Unsigned_64; Now : Interfaces.Unsigned_64;
      Submitted : out Boolean);
   function Matches (Token : Interfaces.Unsigned_64) return Boolean;
   procedure Collect (Value : CuBit.Messages.CompletionEntry);
   -- One bounded revoke/retirement check per invocation; never a wait loop.
   procedure Tick (Now : Interfaces.Unsigned_64);
   function Disabled return Boolean;
   function Ready return Boolean;
   -- A disabled observer needs retry wakes only while a grant remains.
   function Cleanup_Pending return Boolean;
   procedure Take (Rows : out CuBit.Metric_Protocol.Summary_Page;
      Written : out CuBit.Metric_Protocol.Row_Count;
      Next : out Observatory_Metric_Queries.Cursor; Success : out Boolean);
   procedure Close;
end Observatory_Metric_Observer;
