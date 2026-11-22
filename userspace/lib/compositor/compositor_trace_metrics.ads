pragma Ada_2022;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
with Compositor_Trace_Wire;
package Compositor_Trace_Metrics with Pure, SPARK_Mode is
   package R renames CuBit.Metric_Records;
   package P renames CuBit.Metric_Protocol;
   package W renames Compositor_Trace_Wire;
   use type W.Word, R.Metric_Record;
   Schema : constant R.Metric_Key := 1;
   --  Outer endpoint incarnation must remain stable for all four rows.
   --  The raw observer enforces that; a collector resets on reconnection.
   type Envelope is record
      Sequence, Pid, Publisher, Batch : W.Word := 0;
      Value : R.Trace_Record;
   end record;
   type Group is array (R.Trace_Part) of Envelope;
   function Coherent (Rows : Group) return Boolean is
     (Rows (0).Sequence in 1 .. W.Word'Last - 3 and then
      Rows (0).Pid /= 0 and then P.Is_Publisher (Rows (0).Publisher) and then
      Rows (0).Batch /= 0 and then
      (for all I in R.Trace_Part =>
         Rows (I).Sequence = Rows (0).Sequence + W.Word (I) and then
         Rows (I).Pid = Rows (0).Pid and then
         Rows (I).Publisher = Rows (0).Publisher and then
         Rows (I).Batch = Rows (0).Batch and then
         Rows (I).Value.Key = Schema and then
         Rows (I).Value.Trace_ID /= 0 and then
         Rows (I).Value.Trace_ID = Rows (0).Value.Trace_ID and then
         Rows (I).Value.Part = I));
   function Part_Data (Data : W.Packet; I : R.Trace_Part) return R.Trace_Data is
     ([Data (I * 4), Data (I * 4 + 1), Data (I * 4 + 2), Data (I * 4 + 3)]);
   function Fragment (Value : W.Event) return R.Trace_Group
     with Pre => W.Valid (Value),
       Post => R.Valid_Group (Fragment'Result) and then
         (for all I in R.Trace_Part =>
            Fragment'Result (I) =
              (R.Trace, Schema, Value.Event_ID, I,
               Part_Data (W.Encode (Value), I)));
   function Assemble (Rows : Group) return W.Decoded
     with Post => (if Assemble'Result.Success then Coherent (Rows) and then
                    W.Valid (Assemble'Result.Value) and then
                    Assemble'Result.Value.Event_ID = Rows (0).Value.Trace_ID);
end Compositor_Trace_Metrics;
