pragma Ada_2022;
with ACPI_Requests;
package ACPI_Native_Button_Discovery is
   type Outcome is (Not_Attempted, Already_Attempted, Stopping, Not_Ready,
      Fixed_Selected, Clock_Unavailable, Allocation_Refused, Discovery_Failed,
      Source_Unavailable, Control_Method_Selected);
   procedure Start (Server : in out ACPI_Requests.State; Fixed_Available : Boolean; Result : out Outcome)
     with Pre => ACPI_Requests.Valid (Server);
   -- Same serialized fanout as the other consumers, before their common ACK.
   procedure Route (Server : ACPI_Requests.State; Accepted : out Boolean)
     with Pre => ACPI_Requests.Valid (Server);
   procedure Begin_Stop;
   function Quiet return Boolean;
   -- Only after source admission has stopped and retained receipts are routed.
   procedure Close (Released : out Boolean);
end ACPI_Native_Button_Discovery;
