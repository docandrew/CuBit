with Interfaces;
with CuBit.Messages;
with Compositor_Frame_Trace;
with Compositor_Stage_Metrics;
with Compositor_Work_Metrics;
with Compositor_Trace_Wire;
package Desktop_Metrics with SPARK_Mode => Off is
   Enabled : constant Boolean := False;
   procedure Record_Work
     (Kind : Compositor_Work_Metrics.Work_Kind; Pixels, Now : Interfaces.Unsigned_64) is null;
   procedure Record_Stage
     (Stage : Compositor_Stage_Metrics.Stage; First, Last : Interfaces.Unsigned_64) is null;
   procedure Record_Completion (Frame : Compositor_Frame_Trace.Record_Value) is null;
   procedure Record_Trace (Value : Compositor_Trace_Wire.Event) is null;
   procedure Record_Unsupported_Trace is null;
   procedure Record_Trace_Status (Now : Interfaces.Unsigned_64) is null;
   procedure Pump (Sequence : in out Interfaces.Unsigned_64; Now : Interfaces.Unsigned_64) is null;
   procedure Collect (Completion : CuBit.Messages.CompletionEntry) is null;
   function Matches (Token : Interfaces.Unsigned_64) return Boolean is (False);
   function Pending return Boolean is (False);
   function Delay_Us (Now : Interfaces.Unsigned_64) return Interfaces.Unsigned_64 is (Interfaces.Unsigned_64'Last);
   function Disabled return Boolean is (True);
   function Dropped return Interfaces.Unsigned_64 is (0);
   function Invalid return Interfaces.Unsigned_64 is (0);
   function Rejected return Interfaces.Unsigned_64 is (0);
end Desktop_Metrics;
