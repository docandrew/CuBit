with Owner_Test_Control;
package body ACPI_Native_HID_Detection is
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
end ACPI_Native_HID_Detection;
