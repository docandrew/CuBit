package body ACPI_Native_Server.With_Notifications is
   procedure Advance (Server : in out ACPI_Requests.State; Result : out Owner_Outcome) is
   begin
      Result := Owner_Refused;
      Completed := False;
      Last := (others => <>);
      Pump.Run (Server, Claims_Per_Turn, Last);
      Completed := True;
      case Last.Status is
         when Pump.Queue_Empty => Result := Owner_Idle;
         when Pump.Limit_Reached => Result := Owner_Pending;
         when Pump.Not_Ready | Pump.Fanout_Blocked | Pump.Broker_Refused => null;
      end case;
   end Advance;
   function Observation_Complete return Boolean is (Completed);
   function Observation return Pump.Report is (Last);
end ACPI_Native_Server.With_Notifications;
