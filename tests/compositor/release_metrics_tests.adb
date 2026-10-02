with Ada.Text_IO;
with Compositor_Release_Metrics;
with Compositor_Elapsed;
procedure Release_Metrics_Tests is
   package M renames Compositor_Release_Metrics;
   package F renames M.Frames;
   package R renames M.Records;
   use type R.Record_Kind, R.Unit, R.Metric_Key, F.Tick;
   Frame : F.Record_Value;
   Prepared : M.Sample;
begin
   for Output in F.Output loop
      declare
         D : constant R.Metric_Record := M.Declaration (Output);
         Expected : constant String :=
           (if Output = 0 then "desktop.out0.submit_release" else "desktop.out1.submit_release");
      begin
         pragma Assert (R.Valid (D) and D.Kind = R.Describe and
                        D.Declared = R.Span and D.Measure = R.Microseconds);
         pragma Assert (D.Name.Length = Expected'Length);
         for I in Expected'Range loop
            pragma Assert (Character'Val (D.Name.Bytes (I)) = Expected (I));
         end loop;
      end;
      for I in 1 .. 10_000 loop
         Frame := (Output, 7, F.Tick (I), F.Tick (I), F.Tick (I + 10));
         Prepared := M.Prepare (Frame);
         pragma Assert (Prepared.Valid and then Prepared.Value.Key = M.Key (Output));
         pragma Assert (Prepared.Value.Start_Us = Frame.Submitted and
                        Prepared.Value.End_Us = Frame.Completed and
                        Prepared.Value.Span_Correlation = Frame.Frame);
         declare Decoded : constant R.Decoded_Record := R.Decode (R.Encode (Prepared.Value));
         begin
            pragma Assert (Decoded.Success and then Decoded.Value.Kind = R.Span and then
              Decoded.Value.Start_Us = Frame.Submitted and then
              Decoded.Value.End_Us = Frame.Completed and then
              Decoded.Value.Span_Correlation = Frame.Frame);
         end;
      end loop;
   end loop;
   Frame := (0, 1, 1, 0, 0); pragma Assert (M.Prepare (Frame).Valid);
   Frame.Submitted := F.Tick'Last - 1; Frame.Completed := F.Tick'Last - 1;
   pragma Assert (M.Prepare (Frame).Valid);
   Frame.Completed := Compositor_Elapsed.Unavailable;
   pragma Assert (not M.Prepare (Frame).Valid);
   Frame.Completed := 0; pragma Assert (not M.Prepare (Frame).Valid);
   Frame.Submitted := 0; Frame.Frame := 0; pragma Assert (not M.Prepare (Frame).Valid);
   Frame.Frame := 1; Frame.Session := 0; pragma Assert (not M.Prepare (Frame).Valid);
   Frame.Session := 1; Frame.Submitted := Compositor_Elapsed.Unavailable;
   pragma Assert (not M.Prepare (Frame).Valid);
   Ada.Text_IO.Put_Line ("RELEASE-METRICS: PASS 20000 wire round trips, output keys, declarations and invalid clocks/identities");
end Release_Metrics_Tests;
