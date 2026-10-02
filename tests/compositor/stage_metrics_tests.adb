with Ada.Text_IO;
with Compositor_Stage_Metrics;
with Compositor_Elapsed;
procedure Stage_Metrics_Tests is
   package M renames Compositor_Stage_Metrics;
   package R renames M.Records;
   use type M.Tick, R.Record_Kind, R.Metric_Key, R.Unit;
   Checks : Natural := 0;
   procedure Check (Stage : M.Stage; First, Last : M.Tick) is
      S : constant M.Sample := M.Prepare (Stage, First, Last);
      Expected : constant Boolean := First < M.Tick'Last and Last < M.Tick'Last and Last >= First;
   begin
      pragma Assert (S.Valid = Expected);
      if S.Valid then
         declare D : constant R.Decoded_Record := R.Decode (R.Encode (S.Value));
         begin
            pragma Assert (D.Success and then D.Value.Kind = R.Latency and then
              D.Value.Key = 3 + M.Stage'Pos (Stage) and then
              D.Value.Time_Us = Last and then D.Value.Value = Last - First and then
              D.Value.Correlation = 0);
         end;
      end if;
      Checks := Checks + 1;
   end Check;
begin
   for Stage in M.Stage loop
      declare D : constant R.Decoded_Record := R.Decode (R.Encode (M.Declaration (Stage)));
         Name : constant String := (case Stage is
           when M.Input_Dispatch => "desktop.input_dispatch",
           when M.Request_Dispatch => "desktop.request_dispatch",
           when M.Scene_Draw => "desktop.scene_draw",
           when M.Submit_Call => "desktop.submit_call");
      begin
         pragma Assert (D.Success and then D.Value.Kind = R.Describe and then
           D.Value.Key = 3 + M.Stage'Pos (Stage) and then D.Value.Declared = R.Latency and then
           D.Value.Measure = R.Microseconds and then R.Same_Name (D.Value.Name, R.To_Name (Name)));
      end;
      for First in M.Tick range 0 .. 100 loop
         for Last in M.Tick range 0 .. 100 loop Check (Stage, First, Last); end loop;
      end loop;
      for Edge in M.Tick range M.Tick'Last - 2 .. M.Tick'Last loop
         Check (Stage, 0, Edge); Check (Stage, Edge, 0); Check (Stage, Edge, Edge);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("STAGE-METRICS: PASS" & Checks'Image & " clock/codec cases and four distinct declarations");
end Stage_Metrics_Tests;
