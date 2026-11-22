pragma Ada_2022;
package body ACPI_Requests.With_Local_Maintenance with SPARK_Mode is
   procedure Advance (Server : in out State; Allowed : out Controller.Admission; Completed : out Boolean) is
   begin
      Completed:=False;
      if Notifications_Pending(Server) then Allowed:=Controller.Notifications_Pending;return;end if;
      Controller.Advance(Server.Boot,Allowed,Completed);
   end Advance;
end ACPI_Requests.With_Local_Maintenance;
