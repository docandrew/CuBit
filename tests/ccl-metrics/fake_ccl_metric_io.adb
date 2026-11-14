--  A scripted metrics.svc for the hosted metrics.* tests: canned answers,
--  and what each call asked for (Fake_Calls) for the tests to check.
with Fake_Calls;
package body CCL_Metric_IO is
   Series_Id : constant := 77;
   Raw_Points : constant := 300;
   function Available return Boolean is (True);
   function Now_Us return Unsigned_64 is (Fake_Calls.Now);

   procedure List
     (Match : Name_Match; Name, Source : String; Skip : Unsigned_64; Answer : out Pages.Page;
      Rows, Total : out Unsigned_64; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      pragma Unreferenced (Skip);
   begin
      Fake_Calls.Last_Match := Name_Match'Pos (Match);
      Fake_Calls.Last_Name := [others => ' '];
      Fake_Calls.Last_Name (1 .. Name'Length) := Name;
      Fake_Calls.Last_Name_Length := Name'Length;
      Fake_Calls.Last_Source_Length := Source'Length;
      Answer := [others => 0];
      Answer (Pages.Row_Id) := Series_Id;
      Answer (Pages.Row_Kind) := 3;
      Answer (Pages.Row_Unit) := 2;
      Answer (Pages.Row_Published) := 96;
      Answer (Pages.Row_Retained) := 96;
      Answer (Pages.Row_Last) := 5_000;
      Pages.Put_Text (Answer, Pages.Row_Name, "desktop.latency.input_to_ready", Pages.Text_Bytes);
      Pages.Put_Text (Answer, Pages.Row_Source, "com.cubit.desktop", Pages.Text_Bytes);
      Pages.Put_Text (Answer, Pages.Row_Program, "desktop", Pages.Short_Text_Bytes);
      Pages.Put_Text (Answer, Pages.Row_Instance, "12", Pages.Short_Text_Bytes);
      Rows := 1;
      Total := 1;
      Success := True;
      Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "");
   end List;

   procedure Read_Range
     (Id, From_Us, To_Us, Skip, Step_Us : Unsigned_64; Answer : out Pages.Page;
      Rows, Total, Gaps, Resolution_Us : out Unsigned_64; Success : out Boolean;
      Why : out CuBit.Failures.Failure)
   is
   begin
      Fake_Calls.Last_Id := Id;
      Fake_Calls.Last_From := From_Us;
      Fake_Calls.Last_To := To_Us;
      Fake_Calls.Last_Step := Step_Us;
      Answer := [others => 0];
      Gaps := (if Skip = 0 then 1 else 0);
      Answer (Pages.Range_Gaps_At) := 100;
      Answer (Pages.Range_Gaps_At + 1) := 200;
      Answer (Pages.Range_Gaps_At + 2) := 4;
      Answer (Pages.Range_Gaps_At + 3) := 1;
      Total := Raw_Points;
      Rows := Unsigned_64'Min (Pages.Raw_Rows, Raw_Points - Unsigned_64'Min (Skip, Raw_Points));
      for R in 0 .. Natural (Rows) - 1 loop
         Answer (Pages.Range_Rows_At + R * Pages.Raw_Row_Words) := 1_000 + Skip + Unsigned_64 (R);
         Answer (Pages.Range_Rows_At + R * Pages.Raw_Row_Words + 1) := Skip + Unsigned_64 (R);
      end loop;
      Resolution_Us := 0;
      Success := True;
      Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "");
   end Read_Range;

   procedure Read_Window
     (Match : Name_Match; Name, Source : String; From_Us, To_Us : Unsigned_64; Answer : out Pages.Page;
      Merged : out Unsigned_64; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      pragma Unreferenced (Name, Source);
   begin
      Fake_Calls.Last_Match := Name_Match'Pos (Match);
      Fake_Calls.Last_From := From_Us;
      Fake_Calls.Last_To := To_Us;
      Answer := [others => 0];
      Answer (Pages.Stat_Count) := 96;
      Answer (Pages.Stat_P99) := 880;
      Answer (Pages.Stat_Error_Permille) := 32;
      Merged := 2;
      Success := True;
      Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "");
   end Read_Window;
end CCL_Metric_IO;
