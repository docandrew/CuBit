pragma Ada_2022;
with ACPI_Requests.With_Notification_Routing;
package body ACPI_Requests.Notification_Pump with SPARK_Mode is
   package Routing is new ACPI_Requests.With_Notification_Routing (Route_Local);
   procedure Run (Server : in out State; Limit : Batch_Limit; Result : out Report) is
      Reply : Response;
      ID : Receipt_Number;
      use type ACPI_Bootstrap.Device_Phase;
      procedure Stop (Status : Outcome) is
      begin
         Result.Status := Status;
         Result.Retained := Receipts.Current (Server.Receipt);
      end Stop;
   begin
      Result := (others => <>);
      if Current (Server) /= Complete
        or else ACPI_Bootstrap.Device_Completion (Server.Boot).Stage /= ACPI_Bootstrap.Devices_Completed
        or else not ACPI_Bootstrap.Notifications_Enabled (Server.Boot) then
         Stop (Not_Ready); return;
      end if;
      for Turn in 1 .. Limit loop
         pragma Loop_Invariant (Result.Claims = Turn - 1);
         pragma Loop_Invariant (Result.ACK_Attempts = Result.Acknowledged);
         pragma Loop_Invariant (Result.Acknowledged <= Result.Claims);
         Result.Last_Operation := Claiming;
         Result.Claims := Result.Claims + 1;
         Routing.Handle (Server, Snapshot_Provider,
           (Label => Claim_Notification, Data => [Revision (Server), 0, 0, 0], others => <>), Reply);
         Result.Broker_Status := Reply.Status;
         if Reply.Status = Incomplete then Stop (Fanout_Blocked); return; end if;
         if Reply.Status /= OK then Stop (Broker_Refused); return; end if;
         ID := Reply.Data (1);
         if ID = 0 then Stop (Queue_Empty); return; end if;
         Result.Last_Operation := Acknowledging;
         Result.ACK_Attempts := Result.ACK_Attempts + 1;
         Routing.Handle (Server, Snapshot_Provider,
           (Label => Acknowledge_Notification, Data => [Revision (Server), ID, 0, 0], others => <>), Reply);
         Result.Broker_Status := Reply.Status;
         if Reply.Status = Incomplete then Stop (Fanout_Blocked); return; end if;
         if Reply.Status /= OK then Stop (Broker_Refused); return; end if;
         Result.Acknowledged := Result.Acknowledged + 1;
      end loop;
      Stop (Limit_Reached);
   end Run;
end ACPI_Requests.Notification_Pump;
