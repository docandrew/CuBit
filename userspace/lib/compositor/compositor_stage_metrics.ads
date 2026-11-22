with CuBit.Metric_Records;
with Compositor_Elapsed;
package Compositor_Stage_Metrics with SPARK_Mode, Pure is
   package Records renames CuBit.Metric_Records;
   subtype Tick is Compositor_Elapsed.Tick;
   use type Tick, Records.Record_Kind, Records.Unit, Records.Metric_Key;
   -- Inclusive execution durations. Stages may nest (drawing inside
   -- dispatch); do not sum them as disjoint work. Loop_Turn is one event
   -- loop pass that did work, housekeeping included. Two are latencies, not
   -- execution: Input_To_Present, the oldest unpresented input's intake until
   -- the frame showing it is submitted (drawing included; not scanout), and
   -- Input_Source_Age, a pointer report's driver capture until desktop's
   -- intake (driver retention and queueing behind a busy loop; millisecond
   -- clock, so a multiple of 1000 us).
   type Stage is (Input_Dispatch, Request_Dispatch, Scene_Draw, Submit_Call,
                  Completion_Dispatch, Diagnostic_Output, Loop_Turn,
                  Input_To_Present, Input_Source_Age);
   function Key (Item : Stage) return Records.Metric_Key is
     (case Item is
       when Input_Dispatch .. Submit_Call => 3 + Stage'Pos (Item),
       when Completion_Dispatch => 11,
       when Diagnostic_Output => 12,
       when Loop_Turn => 13,
       when Input_To_Present => 14,
       when Input_Source_Age => 15);
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
