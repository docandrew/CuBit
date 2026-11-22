pragma Ada_2022;
package body ACPI_Requests.With_Notification_Routing with SPARK_Mode is
   Canonical_Word_Count : constant Unsigned_8 := 4;
   function Admissible_ACK (Server : State; Origin : Authority; Request : Packet) return Boolean is
     (Origin = Snapshot_Provider and then Request.Label = Acknowledge_Notification
      and then Request.Length = Canonical_Word_Count and then Request.Flags = 0 and then Request.Reserved = 0
      and then Request.Data (2 .. 3) = [0, 0]
      and then Request.Data (0) = Server.Version and then Server.Version < Max_Revision
      and then Receipts.Current (Server.Receipt) /= 0
      and then Request.Data (1) = Receipts.Current (Server.Receipt));
   procedure Handle (Server : in out State; Origin : Authority;
      Request : Packet; Reply : out Response) is
      Routed : Boolean;
   begin
      if Admissible_ACK (Server, Origin, Request) then
         Route_Local (Server, Routed);
         if not Routed then
            Reply := (Status => Incomplete,
              Data => [Server.Version, Receipts.Current (Server.Receipt), 0, 0]);
            return;
         end if;
      end if;
      Base_Handle (Server, Origin, Request, Reply);
      if Origin = Snapshot_Provider and then Request.Label = Claim_Notification
        and then Reply.Status = OK and then Receipts.Current (Server.Receipt) /= 0
      then
         Route_Local (Server, Routed);
         if not Routed then Reply.Status := Incomplete; end if;
      end if;
   end Handle;
end ACPI_Requests.With_Notification_Routing;
