generic
   type Consumer_State is limited private;
   -- Trusted local consumer: only this owner supplies the private broker's
   -- SAME-Core immutable receipt/event. Bounded, normally returning, no AML,
   -- IPC, reset, reentry or Server mutation. Accepted means observed or
   -- explicitly invalidated; repeated delivery must be idempotent.
   with procedure Observe_Retained
     (S : in out Consumer_State; Service : ACPI_Service.State;
      ID : Receipt_Number; Event : ACPI_Service.Notification_Observation;
      Accepted : out Boolean);
package ACPI_Requests.With_Retained_Notification with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   -- Bind S in Route_Local; combine with ALL other consumers and keep every
   -- ACK on With_Notification_Routing/Notification_Pump's guarded path.
   -- No wire selector, authority, AML, IPC, queue dequeue/ACK or event copy.
   procedure Route (S : in out Consumer_State; Server : ACPI_Requests.State;
      Routed : out Boolean)
     with Pre => ACPI_Requests.Valid(Server), Post => ACPI_Requests.Valid(Server)
       and then ACPI_Requests.Model(Server)=ACPI_Requests.Model(Server)'Old;
end ACPI_Requests.With_Retained_Notification;
