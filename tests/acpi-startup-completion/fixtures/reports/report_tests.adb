pragma Ada_2022;
with ACPI_Startup_Completion;
with Ada.Text_IO;
with ACPI_Bootstrap;
with ACPI_Native_HID_Detection;
with ACPI_Native_Power_Detection;
with ACPI_Native_Startup_Completion;
with CuBit.Consumer_Initialization_Exchange;
procedure Report_Tests is
   package D renames ACPI_Bootstrap;
   package H renames ACPI_Native_HID_Detection;
   package W renames ACPI_Native_Power_Detection;
   package P renames CuBit.Consumer_Initialization_Exchange;
   use type P.Phase_Status;
   type Severity is (Done, Errors, Pending, Broken);
   type Device_Table is array (D.Device_Phase) of Severity;
   type HID_Table is array (H.Outcome) of Severity;
   type Power_Table is array (W.Outcome) of Severity;
   Device_Expected : constant Device_Table := [D.Devices_Completed => Done,
      D.Devices_Completed_With_Errors => Errors,
      D.Devices_Not_Started | D.Devices_Failed => Broken, others => Pending];
   HID_Expected : constant HID_Table := [H.No_Devices | H.Finished => Done,
      H.Cleanup_Failed | H.Owner_Error | H.Recovery_Required => Broken, others => Pending];
   Power_Expected : constant Power_Table := [W.No_Devices | W.Finished => Done,
      W.Lid_Failed | W.Source_Failed => Errors,
      W.Recovery_Required | W.Owner_Error => Broken, others => Pending];
   Expected_Status : constant array (Severity) of P.Phase_Status :=
      [Done => P.Completed, Errors => P.Completed_With_Errors,
       Pending => P.Incomplete, Broken => P.Failed];
   Devices : D.Device_Report;
   HID : H.Report;
   Power : W.Report;
   Count : Natural := 0;
   procedure Check (Expected : P.Phase_Status; Closed : Boolean := True;
      Events : ACPI_Startup_Completion.Phase_Result := ACPI_Startup_Completion.Finished;
      Modules : Boolean := False) is
   begin
      if ACPI_Native_Startup_Completion.Classify (Devices, HID, Power, Closed, Events, Modules) /= Expected then
         raise Program_Error with "report classification check" & Count'Image;
      end if;
      Count := Count + 1;
   end Check;
   procedure Good is
   begin
      Devices := (Stage => D.Devices_Completed, others => <>);
      HID := (Status => H.No_Devices, others => <>);
      Power := (Status => W.No_Devices, others => <>);
   end Good;
begin
   for Device_Status in D.Device_Phase loop
      for HID_Status in H.Outcome loop
         for Power_Status in W.Outcome loop
            for Closed in Boolean loop
               Good;
               Devices.Stage := Device_Status; HID.Status := HID_Status; Power.Status := Power_Status;
               Check (Expected_Status (Severity'Max (Device_Expected (Device_Status),
                 Severity'Max (HID_Expected (HID_Status), Severity'Max (Power_Expected (Power_Status),
                   (if Closed then Done else Pending))))), Closed);
            end loop;
         end loop;
      end loop;
   end loop;
   Good; Devices.Fatal := (Present => True, others => <>); Check (P.Failed);
   Good; Devices.Errors := 1; Check (P.Completed_With_Errors);
   Good; HID.Cleanup_Required := True; Check (P.Failed);
   Good; HID.Recovery_Needed := True; Check (P.Failed);
   Good; HID.Candidates := 1; Check (P.Incomplete);
   Good; HID.Status := H.Finished; HID.Candidates := 2; HID.Observed := 1; Check (P.Incomplete);
   HID.Observed := 2; Check (P.Completed);
   Good; Power.Recovery_Needed := True; Check (P.Failed);
   Good; Power.Lid_Observed := True; Check (P.Incomplete);
   Good; Power.Source_Query_Failed := True; Check (P.Incomplete);
   Good; Power.Status := W.Finished; Power.Lid_Query_Failed := True; Check (P.Completed_With_Errors);
   Good; Power.Status := W.Finished; Power.Source_Query_Failed := True; Check (P.Completed_With_Errors);
   Good; Power.Status := W.Finished; Power.Batteries := 2;
   Power.Sampled_Batteries := 1; Check (P.Incomplete);
   Power.Absent_Batteries := 1; Check (P.Completed);
   Power.Batteries := W.Maximum_Batteries; Power.Sampled_Batteries := W.Maximum_Batteries;
   Power.Absent_Batteries := W.Maximum_Batteries; Check (P.Incomplete);
   Good; Power.Status := W.Battery_Failed; Check (P.Incomplete);
   Power.Batteries := 2; Power.Failed_Batteries := 1; Power.Sampled_Batteries := 1;
   Check (P.Completed_With_Errors);
   Power.Status := W.Battery_Search_Failed; Check (P.Incomplete);
   Power.Status := W.Budget_Exhausted; Check (P.Incomplete);
   Power.Status := W.Battery_Limit; Check (P.Incomplete);
   Power.Status := W.Battery_Failed; Power.Recovery_Needed := True; Check (P.Failed);
   Power.Recovery_Needed := False; Power.Failed_Batteries := 2; Check (P.Incomplete);
   Power.Failed_Batteries := 0; Power.Sampled_Batteries := 2; Check (P.Incomplete);
   Power.Failed_Batteries := 1; Power.Sampled_Batteries := 1;
   Power.Status := W.Finished; Check (P.Incomplete);
   Power.Status := W.No_Devices; Check (P.Incomplete);
   Power.Status := W.Lid_Failed; Power.Lid_Query_Failed := True; Check (P.Completed_With_Errors);
   Power.Status := W.Source_Failed; Power.Source_Query_Failed := True; Check (P.Completed_With_Errors);
   Good; Check (P.Incomplete, False);
   HID.Recovery_Needed := True; Check (P.Failed, False);
   for Event_Result in ACPI_Startup_Completion.Phase_Result loop
      for Modules in Boolean loop
         Good;
         Check ((case Event_Result is
            when ACPI_Startup_Completion.Failed => P.Failed,
            when ACPI_Startup_Completion.Incomplete => P.Incomplete,
            when ACPI_Startup_Completion.Finished_With_Errors => P.Completed_With_Errors,
            when ACPI_Startup_Completion.Finished =>
               (if Modules then P.Completed_With_Errors else P.Completed)),
            Events => Event_Result, Modules => Modules);
      end loop;
   end loop;
   Good; HID.Recovery_Needed := True;
   Check (P.Failed, Events => ACPI_Startup_Completion.Incomplete, Modules => True);
   Ada.Text_IO.Put_Line ("Startup report checks" & Count'Image);
end Report_Tests;
