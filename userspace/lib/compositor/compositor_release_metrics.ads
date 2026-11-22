with CuBit.Metric_Records;
with Compositor_Frame_Trace;
package Compositor_Release_Metrics with SPARK_Mode, Pure is
   package Records renames CuBit.Metric_Records;
   package Frames renames Compositor_Frame_Trace;
   use type Records.Record_Kind, Records.Unit, Records.Metric_Key, Frames.Tick;

   -- Per-output software submit-to-release spans. These are not input,
   -- display-latch or photon latency. Frame tokens are allocated from the
   -- Desktop-wide non-reused request sequence, including across output reopen.
   function Key (Output : Frames.Output) return Records.Metric_Key is
     (Records.Metric_Key (Output + 1));
   function Declaration (Output : Frames.Output) return Records.Metric_Record
     with Post => Records.Valid (Declaration'Result) and then
       Declaration'Result.Kind = Records.Describe and then
       Declaration'Result.Key = Key (Output) and then
       Declaration'Result.Declared = Records.Span and then
       Declaration'Result.Measure = Records.Microseconds;

   type Sample (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Records.Metric_Record (Records.Span);
         when False => null;
      end case;
   end record;
   function Prepare (Frame : Frames.Record_Value) return Sample
     with Post => Prepare'Result.Valid = Frames.Valid (Frame) and then
       (if Prepare'Result.Valid then
          Records.Valid (Prepare'Result.Value) and then
          Prepare'Result.Value.Key = Key (Frame.Output_ID) and then
          Prepare'Result.Value.Start_Us = Frame.Submitted and then
          Prepare'Result.Value.End_Us = Frame.Completed and then
          Prepare'Result.Value.Span_Correlation = Frame.Frame);
end Compositor_Release_Metrics;
