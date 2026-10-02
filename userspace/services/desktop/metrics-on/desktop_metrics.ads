with Interfaces;
with CuBit.Messages;
with Compositor_Frame_Trace;
with Compositor_Stage_Metrics;
with Desktop_Metric_Publisher;
with CCL_Manifest_Bindings;
package Desktop_Metrics with SPARK_Mode => Off is
   Enabled : constant Boolean := True;
   package Publisher is new Desktop_Metric_Publisher (CCL_Manifest_Bindings.Slot_metrics);
   procedure Record_Stage
     (Stage : Compositor_Stage_Metrics.Stage; First, Last : Interfaces.Unsigned_64) renames Publisher.Record_Stage;
   procedure Record_Completion (Frame : Compositor_Frame_Trace.Record_Value) renames Publisher.Record_Completion;
   procedure Pump (Sequence : in out Interfaces.Unsigned_64; Now : Interfaces.Unsigned_64) renames Publisher.Pump;
   procedure Collect (Completion : CuBit.Messages.CompletionEntry) renames Publisher.Collect;
   function Matches (Token : Interfaces.Unsigned_64) return Boolean renames Publisher.Matches;
   function Pending return Boolean renames Publisher.Pending;
   function Delay_Us (Now : Interfaces.Unsigned_64) return Interfaces.Unsigned_64 renames Publisher.Delay_Us;
   function Disabled return Boolean renames Publisher.Disabled;
   function Dropped return Interfaces.Unsigned_64 renames Publisher.Dropped;
   function Invalid return Interfaces.Unsigned_64 renames Publisher.Invalid;
   function Rejected return Interfaces.Unsigned_64 renames Publisher.Rejected;
end Desktop_Metrics;
