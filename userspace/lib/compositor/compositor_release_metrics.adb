with Interfaces;
package body Compositor_Release_Metrics with SPARK_Mode is
   use type Interfaces.Unsigned_8;
   function Declaration (Output : Frames.Output) return Records.Metric_Record is
      -- "desktop.out0.submit_release", with output digit at byte 12.
      Name : Records.Metric_Name :=
        (Bytes =>
           [100, 101, 115, 107, 116, 111, 112, 46, 111, 117, 116, 48,
            46, 115, 117, 98, 109, 105, 116, 95, 114, 101, 108, 101, 97, 115, 101,
            others => 0], Length => 27);
   begin
      Name.Bytes (12) := Character'Pos ('0') + Interfaces.Unsigned_8 (Output);
      return (Records.Describe, Key (Output), Records.Span, Records.Microseconds, Name);
   end Declaration;

   function Prepare (Frame : Frames.Record_Value) return Sample is
   begin
      if not Frames.Valid (Frame) then
         return (Valid => False);
      end if;
      return (True, (Records.Span, Key (Frame.Output_ID), Frame.Submitted,
                     Frame.Completed, Frame.Frame));
   end Prepare;
end Compositor_Release_Metrics;
