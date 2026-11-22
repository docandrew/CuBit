with CuBit.Metric_Records;
with Compositor_Elapsed;
package Compositor_Work_Metrics with SPARK_Mode, Pure is
   package R renames CuBit.Metric_Records;
   subtype Tick is Compositor_Elapsed.Tick;
   use type Tick, R.Record_Kind, R.Unit, R.Metric_Key;
   -- Delta work per observation, never cumulative totals. Units distinguish pixels from transferred bytes.
   type Work_Kind is (Scene_Pixels, Repair_Pixels, GPU_Readback_Bytes, CPU_Copy_Bytes);
   function Key (Item : Work_Kind) return R.Metric_Key is (7 + Work_Kind'Pos (Item));
   function Declaration (Item : Work_Kind) return R.Metric_Record
     with Post => R.Valid (Declaration'Result) and then
       Declaration'Result.Kind = R.Describe and then
       Declaration'Result.Key = Key (Item) and then
       Declaration'Result.Declared = R.Counter and then Declaration'Result.Measure = (if Item in GPU_Readback_Bytes | CPU_Copy_Bytes then R.Bytes else R.Count);
   type Sample (Valid : Boolean := False) is record
      case Valid is
         when True => Value : R.Metric_Record (R.Counter);
         when False => null;
      end case;
   end record;
   function Prepare (Item : Work_Kind; Amount, Now : Tick) return Sample
     with Post => Prepare'Result.Valid = (Now /= Compositor_Elapsed.Unavailable) and then
       (if Prepare'Result.Valid then R.Valid (Prepare'Result.Value) and then
         Prepare'Result.Value.Key = Key (Item) and then
         Prepare'Result.Value.Value = Amount and then Prepare'Result.Value.Time_Us = Now and then
         Prepare'Result.Value.Correlation = 0);
end Compositor_Work_Metrics;
