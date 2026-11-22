with Owner_Test_Control;
package body ACPI_Native_HID_Detection is
   function Name (Status : Outcome) return String is
     (case Status is
        when Not_Attempted => "NOT_ATTEMPTED", when Not_Ready => "NOT_READY",
        when No_Devices => "NO_DEVICES", when Finished => "FINISHED",
        when Budget_Exhausted => "BUDGET_EXHAUSTED", when Policy_Unresolved => "POLICY_UNRESOLVED",
        when Search_Failed => "SEARCH_FAILED", when Discovery_Failed => "DISCOVERY_FAILED",
        when Sources_Failed => "SOURCES_FAILED", when Capacity_Refused => "CAPACITY_REFUSED",
        when Candidate_Limit => "CANDIDATE_LIMIT", when Cleanup_Failed => "CLEANUP_FAILED",
        when Owner_Error => "OWNER_ERROR", when Recovery_Required => "RECOVERY_REQUIRED",
        when Channel_Refused => "CHANNEL_REFUSED");
   function Step_Name (Step : Progress_Step) return String is
     (case Step is
        when Not_Started => "NOT_STARTED", when Search_Candidates => "SEARCH_CANDIDATES",
        when Begin_Device_Probe => "BEGIN_DEVICE_PROBE", when Release_Inactive_Probe => "RELEASE_INACTIVE_PROBE",
        when Resolve_Resources => "RESOLVE_RESOURCES", when Cancel_Probe => "CANCEL_PROBE",
        when Validate_Candidate => "VALIDATE_CANDIDATE", when Prepare_Channels => "PREPARE_CHANNELS",
        when Record_Observation => "RECORD_OBSERVATION");
   function Error_Name (Kind : Error_Kind) return String is
     (case Kind is
        when Policy_Error => "POLICY_ERROR",
        when Identity_Error => "IDENTITY_ERROR",
        when Firmware_Error => "FIRMWARE_ERROR",
        when Source_Error => "SOURCE_ERROR",
        when Channel_Error => "CHANNEL_ERROR");

   procedure Run (Server : in out ACPI_Requests.State; Result : out Report;
      Limits : Policy := (others => <>)) is
      pragma Unreferenced (Limits);
      package T renames Owner_Test_Control;
      use type ACPI_Requests.Phase, T.Scenario;
   begin
      if ACPI_Requests.Current (Server) /= ACPI_Requests.Complete then
         raise Program_Error with "detection before completed snapshot";
      end if;
      T.Detections := T.Detections + 1;
      T.Sequence := T.Sequence + 1; T.Detected_At := T.Sequence;
      if T.Selected = T.Detection_Exception then
         raise Program_Error with "modeled unexpected detection exception";
      end if;
      Result := (Status => (if T.Selected = T.Detection_Cleanup then Cleanup_Failed
        elsif T.Selected = T.Detection_Recovery then Recovery_Required else No_Devices),
        Recovery_Needed => T.Selected = T.Detection_Recovery,
        Cleanup_Required => T.Selected = T.Detection_Cleanup, others => <>);
   end Run;
   procedure Run_With_Policy (Server : in out ACPI_Requests.State;
      Result : out Report; Limits : Policy := (others => <>)) is
      procedure Candidate_Check
        (Service : aliased in out ACPI_Service.State;
         Item : ACPI_Service.Node_Descriptor; Budget : Natural;
         Result : out Owner.Controller.Policy_Report) renames Check_Candidate;
      pragma Unreferenced(Candidate_Check);
   begin
      Run (Server, Result, Limits);
   end Run_With_Policy;
end ACPI_Native_HID_Detection;
