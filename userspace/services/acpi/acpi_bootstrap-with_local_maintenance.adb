pragma Ada_2022;
package body ACPI_Bootstrap.With_Local_Maintenance with SPARK_Mode is
   procedure Advance (Session : in out State; Allowed : out Admission; Completed : out Boolean) is
   begin
      Completed:=False;Allowed:=Not_Ready;
      if not Device_Walk_Finished (Session) then return;end if;
      if Session.Notify_Enabled and then Natural(ACPI_Service.Pending_Notifications(Session.Core))>0 then
         Allowed:=Notifications_Pending;return;
      end if;
      if Retained_Results_Pending(Session) then Allowed:=Results_Pending;return;end if;
      Allowed:=Admitted;Step(Session.Core,Completed);
   end Advance;
end ACPI_Bootstrap.With_Local_Maintenance;
