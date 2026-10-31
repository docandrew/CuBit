with Owner_Test_Control;
package body ACPI_Native_Power_Detection is
   procedure Run (Server : in out ACPI_Requests.State; Result : out Report;
      Limits : Policy := (others => <>)) is
      pragma Unreferenced (Limits);
      package T renames Owner_Test_Control;
      use type ACPI_Requests.Phase, T.Scenario;
   begin
      if ACPI_Requests.Current (Server) /= ACPI_Requests.Complete or else T.Detections /= 1 then
         raise Program_Error with "power before completed snapshot/HID pass";
      end if;
      T.Power_Detections := T.Power_Detections + 1;
      T.Sequence := T.Sequence + 1; T.Power_Detected_At := T.Sequence;
      if T.Selected = T.Power_Exception then raise Program_Error with "modeled power exception"; end if;
      Result := (Status => (if T.Selected = T.Power_Cleanup then Recovery_Required else No_Devices),
        Recovery_Needed => T.Selected = T.Power_Cleanup, others => <>);
   end Run;
end ACPI_Native_Power_Detection;
