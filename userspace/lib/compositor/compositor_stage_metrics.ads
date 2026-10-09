with CuBit.Metric_Records;
with Compositor_Elapsed;
package Compositor_Stage_Metrics with SPARK_Mode, Pure is
   package Records renames CuBit.Metric_Records;
   subtype Tick is Compositor_Elapsed.Tick;
   use type Tick, Records.Record_Kind, Records.Unit, Records.Metric_Key;
   -- Inclusive execution durations, not queue waiting or physical input latency.
   -- Stages may nest (drawing inside dispatch); do not sum them as disjoint work.
   type Stage is (Input_Dispatch, Request_Dispatch, Scene_Draw, Submit_Call,
                  Completion_Dispatch, Diagnostic_Output);
   function Key (Item : Stage) return Records.Metric_Key is
     (case Item is
       when Input_Dispatch .. Submit_Call => 3 + Stage'Pos (Item),
       when Completion_Dispatch => 11,
       when Diagnostic_Output => 12);
   function Declaration (Item : Stage) return Records.Metric_Record
     with Post => Records.Valid (Declaration'Result) and then
       Declaration'Result.Kind = Records.Describe and then
       Declaration'Result.Key = Key (Item) and then
       Declaration'Result.Declared = Records.Latency and then
       Declaration'Result.Measure = Records.Microseconds;
   type Sample (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Records.Metric_Record (Records.Latency);
         when False => null;
      end case;
   end record;
   function Prepare (Item : Stage; First, Last : Tick) return Sample
     with Post => Prepare'Result.Valid = Compositor_Elapsed.Measure (First, Last).Valid and then
       (if Prepare'Result.Valid then
          Records.Valid (Prepare'Result.Value) and then
          Prepare'Result.Value.Key = Key (Item) and then
          Prepare'Result.Value.Time_Us = Last and then
          Prepare'Result.Value.Value = Last - First and then
          Prepare'Result.Value.Correlation = 0);
end Compositor_Stage_Metrics;
