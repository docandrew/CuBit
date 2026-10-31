pragma Ada_2022;
with ACPI_EC_Event_Receiver;
with ACPI_EC_Event_Types;
with ACPI_Bootstrap.EC_Events;
with CuBit.Process_IDs;
generic
   with package Receiver is new ACPI_EC_Event_Receiver
     (Core => ACPI_Service, others => <>);
package ACPI_Requests.EC_Events with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Controller is new ACPI_Bootstrap.EC_Events (Receiver);
   -- Trusted owner bridge to the SAME Core used by lid/power caches and the
   -- notification receipt broker. No observer/wire selector is added. Keep
   -- Receiver and each Delivery with one Server; serialize all owner access.
   -- See Controller's independent enrollment/provenance obligations.
   procedure Install
     (Server : State; Source : ACPI_EC_Event_Types.Controller_ID;
      Epoch : ACPI_EC_Event_Types.Epoch_ID; Sender : CuBit.Process_IDs.Process_ID;
      Scope : ACPI_Service.Namespace.Node_ID; Allowed : out Controller.Admission;
      Result : out Receiver.Enrollment)
     with Pre => Valid (Server);
   procedure Dispatch
     (Server : in out State; Run : in out Receiver.Delivery;
      Sender : CuBit.Process_IDs.Process_ID; Item : ACPI_EC_Event_Types.Notice;
      Budget : Natural; Allowed : out Controller.Admission;
      Result : out Receiver.Outcome)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
   procedure Close
     (Server : in out State; Run : in out Receiver.Delivery;
      Result : out Receiver.Close_Result)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
end ACPI_Requests.EC_Events;
