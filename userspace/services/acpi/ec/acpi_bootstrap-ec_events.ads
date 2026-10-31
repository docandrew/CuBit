pragma Ada_2022;
with ACPI_EC_Event_Receiver;
with ACPI_EC_Event_Types;
with CuBit.Process_IDs;
generic
   with package Receiver is new ACPI_EC_Event_Receiver
     (Core => ACPI_Service, others => <>);
package ACPI_Bootstrap.EC_Events with SPARK_Mode is
   type Admission is (Not_Ready, Admitted);
   -- Trusted serialized owner API. Keep Receiver with this Session for its
   -- lifetime. Scope is selected by the owner from this completed snapshot;
   -- it is never a wire node selector or proof of hardware/source authority.
   -- Before enrollment the owner independently establishes EC identity,
   -- resources, provider readiness and authenticated event-source provenance.
   -- Device initialization must complete successfully before any enrollment
   -- or event can execute AML. Enrollment itself invokes no AML.
   procedure Install
     (Session : State; Controller : ACPI_EC_Event_Types.Controller_ID;
      Epoch : ACPI_EC_Event_Types.Epoch_ID; Sender : CuBit.Process_IDs.Process_ID;
      Scope : ACPI_Service.Namespace.Node_ID; Allowed : out Admission;
      Result : out Receiver.Enrollment)
     with Pre => Valid (Session);
   -- Sender is authenticated receive metadata. Preserve source FIFO order.
   -- Not_Ready neither consumes Run nor advances Receiver's token history.
   procedure Dispatch
     (Session : in out State; Run : in out Receiver.Delivery;
      Sender : CuBit.Process_IDs.Process_ID; Item : ACPI_EC_Event_Types.Notice;
      Budget : Natural; Allowed : out Admission; Result : out Receiver.Outcome)
     with Pre => Valid (Session), Post => Valid (Session);
   -- Cleanup remains possible even when startup is no longer ready. Close
   -- delegates the retained owner check; it cannot clear Field uncertainty.
   procedure Close
     (Session : in out State; Run : in out Receiver.Delivery;
      Result : out Receiver.Close_Result)
     with Pre => Valid (Session), Post => Valid (Session);
end ACPI_Bootstrap.EC_Events;
