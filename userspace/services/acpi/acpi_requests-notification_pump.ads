pragma Ada_2022;
generic
   -- Bounded local fanout to ALL consumers,or explicitly invalidate them.
   -- No AML/IPC/reset/reentry or Server mutation. Return normally; repeated
   -- routing of a retained receipt must be idempotent,including partial failure.
   with procedure Route_Local (Server : State; Routed : out Boolean);
package ACPI_Requests.Notification_Pump with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   Maximum_Batch : constant := 32;
   subtype Batch_Limit is Natural range 0 .. Maximum_Batch;
   type Outcome is (Not_Ready, Queue_Empty, Limit_Reached, Fanout_Blocked, Broker_Refused);
   type Operation is (Claiming, Acknowledging);
   type Report is record
      Status : Outcome := Not_Ready;
      Last_Operation : Operation := Claiming;
      Claims, ACK_Attempts, Acknowledged : Batch_Limit := 0;
      Retained : Receipt_Number := 0;
      Broker_Status : ACPI_Requests.Outcome := Wrong_Order;
   end record;
   -- Trusted serialized owner work,no wire operation or event-source authority.
   -- Claim and ACK both use the existing guarded fanout adapter. On fanout
   -- failure leave the exact receipt retained; retry routes it idempotently.
   -- Limit_Reached never claims the queue is empty,even after the last ACK.
   -- Queue_Empty means only no local queued/staged work at this return. It is
   -- not source health,overflow recovery,hardware ACK or permission to publish
   -- a cache. Those remain independent owner obligations. Executes no AML.
   procedure Run (Server : in out State; Limit : Batch_Limit; Result : out Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) >= Revision (Server)'Old
       and then Result.Claims <= Limit and then Result.ACK_Attempts <= Limit
       and then Result.Acknowledged <= Result.ACK_Attempts;
end ACPI_Requests.Notification_Pump;
