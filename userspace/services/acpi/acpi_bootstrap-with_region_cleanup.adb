pragma Ada_2022;
package body ACPI_Bootstrap.With_Region_Cleanup with SPARK_Mode is
   procedure Advance (Session : in out State; Allowed : out Admission; Completed : out Boolean) is
   begin
      Completed:=False; Allowed:=Not_Ready;
      if Session.Stage/=Complete or else not Session.Device_Data.Regions_Attempted
        or else Session.Device_Data.Stage not in Devices_Completed | Devices_Completed_With_Errors |
          Devices_Incomplete | Devices_Failed
      then return; end if;
      if Session.Notify_Enabled and then Natural(ACPI_Service.Pending_Notifications(Session.Core))>0 then
         Allowed:=Notifications_Pending; return;
      end if;
      if Retained_Results_Pending(Session) then Allowed:=Results_Pending; return; end if;
      Allowed:=Admitted; Step(Session.Core,Completed);
   end Advance;
end ACPI_Bootstrap.With_Region_Cleanup;
