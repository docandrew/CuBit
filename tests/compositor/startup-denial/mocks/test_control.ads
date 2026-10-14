package Test_Control is
 type Stage is (Device, Health, Targets, Pipeline, Upload, Readback);
 type Counts is array(Stage) of Natural;
 Calls : Counts := (others => 0);
 Deny : Boolean := False; Failure : Stage := Device;
 --  CAP-002: no render entry in the startup table (an unadmitted start).
 Render_Absent : Boolean := False;
 Stops : Natural := 0;
 Stage_Logs, Stage_Length : Natural := 0;
 Last_Stage : String(1..32) := (others => ' ');
 procedure Reset;
 procedure Attempt(S : Stage; OK : out Boolean);
end Test_Control;
