pragma Ada_2022;
package body ACPI_Requests.With_Driver with SPARK_Mode is
   package Controlled is new ACPI_Bootstrap.With_Driver (Device_Driver, Execute_Module_Startup);
   procedure Handle (Server : in out State; Run : in out Device_Driver.Session;
      Origin : Authority; Request : Packet; Reply : out Response) is
      Initial_Version : constant Revision_Number := Server.Version;
   begin
      if Request.Label /= Finish_Snapshot then
         ACPI_Requests.Handle (Server, Origin, Request, Reply);
         return;
      end if;
      Reply := (Status => Malformed, Data => [0 => Server.Version, others => 0]);
      if Origin = No_Authority then
         Reply := (Status => Denied, Data => [others => 0]); return;
      end if;
      if Request.Length /= 4 or Request.Flags /= 0 or Request.Reserved /= 0 then return; end if;
      if Origin /= Snapshot_Provider then Reply.Status := Denied; return; end if;
      if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
      if Initial_Version = Max_Revision then Reply.Status := Resource_Limit; return; end if;
      if Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
      if Current (Server) /= Receiving or else Server.Open then Reply.Status := Wrong_Order; return; end if;
      Controlled.Finish (Server.Boot, Run, ACPI_Bootstrap.Default_Startup_Budget);
      Reply.Status := (if Current (Server) = Complete then OK else Incomplete);
      Server.Version := Initial_Version + 1;
      Reply.Data (0) := Server.Version;
   end Handle;
end ACPI_Requests.With_Driver;
