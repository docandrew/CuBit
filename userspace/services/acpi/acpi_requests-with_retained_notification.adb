package body ACPI_Requests.With_Retained_Notification with SPARK_Mode is
   procedure Route (S : in out Consumer_State; Server : ACPI_Requests.State;
      Routed : out Boolean) is
      procedure Apply (Service : ACPI_Service.State; Completed : out Boolean) is
      begin
         Observe_Retained(S,Service,Receipts.Current(Server.Receipt),
           Server.Staged_Notification,Completed);
      end Apply;
      package Metadata is new ACPI_Requests.With_Metadata_Continuation(Apply);
   begin
      Routed:=False;
      if Receipts.Current(Server.Receipt)=0 then Routed:=True;return;end if;
      Metadata.Advance(Server,Routed);
   end Route;
end ACPI_Requests.With_Retained_Notification;
