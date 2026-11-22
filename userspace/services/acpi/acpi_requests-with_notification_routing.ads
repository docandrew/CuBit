pragma Ada_2022;
generic
   -- Trusted owner fanout of this Server's retained event to ALL local
   -- consumers. Bounded, no AML/IPC/reset/reentry or Server mutation; returns
   -- normally. Repeated calls for the same receipt must be idempotent, including
   -- after partial fanout failure. Return True only once every consumer has
   -- observed the retained event or been explicitly invalidated.
   with procedure Route_Local (Server : State; Routed : out Boolean);
   -- May select the owner's provisioned startup handler. It must preserve the
   -- existing Requests framing/authority/receipt/revision contracts and must
   -- neither reenter this adapter nor bypass routing by acknowledging itself.
   with procedure Base_Handle (Server : in out State; Origin : Authority;
      Request : Packet; Reply : out Response) is ACPI_Requests.Handle;
package ACPI_Requests.With_Notification_Routing with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   -- Successful Claim attempts local routing immediately. A canonically valid
   -- ACK of the current receipt must route successfully BEFORE Base_Handle can
   -- discard it. Failure returns existing Incomplete and preserves the receipt
   -- and revision. Invalid/unauthorized/stale/exhausted requests retain normal
   -- protocol handling without invoking Route_Local. Repeated accepted ACKs
   -- do not route an already retired receipt. This allocates no wire selector.
   procedure Handle (Server : in out State; Origin : Authority;
      Request : Packet; Reply : out Response)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) >= Revision (Server)'Old
       and then (if Origin = No_Authority then Reply.Status = Denied
         and then Reply.Data = [0, 0, 0, 0]
         else Reply.Data (0) = Revision (Server))
       and then (for all Word of Reply.Data => Word <= Max_Revision);
end ACPI_Requests.With_Notification_Routing;
