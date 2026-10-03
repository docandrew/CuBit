with Interfaces;
with CuBit.Messages;
with Compositor_Frame_Trace;
with Compositor_Stage_Metrics;
with Compositor_Work_Metrics;
generic
   Capability : CuBit.Messages.CapabilitySlot;
package Desktop_Metric_Publisher with SPARK_Mode => Off is
   -- Serialized by Desktop's event loop. This adapter owns the SDK's two
   -- pages for its entire lifetime, including after telemetry quarantine.
   procedure Record_Work
     (Kind : Compositor_Work_Metrics.Work_Kind; Pixels, Now : Interfaces.Unsigned_64);
   procedure Record_Stage
     (Stage : Compositor_Stage_Metrics.Stage; First, Last : Interfaces.Unsigned_64);
   procedure Record_Completion (Frame : Compositor_Frame_Trace.Record_Value);
   -- At most one asynchronous submission; no waits, retries or extra clock.
   procedure Pump (Sequence : in out Interfaces.Unsigned_64; Now : Interfaces.Unsigned_64);
   function Matches (Token : Interfaces.Unsigned_64) return Boolean;
   function Pending return Boolean;
   function Delay_Us (Now : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
   procedure Collect (Completion : CuBit.Messages.CompletionEntry);
   procedure Quarantine;
   function Disabled return Boolean;
   function Dropped return Interfaces.Unsigned_64;
   function Invalid return Interfaces.Unsigned_64;
   function Rejected return Interfaces.Unsigned_64;
end Desktop_Metric_Publisher;
