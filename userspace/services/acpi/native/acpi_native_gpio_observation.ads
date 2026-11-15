pragma Ada_2022;
with ACPI_Service;with ACPI_One_Service;
with ACPI_GPIO_Bootstrap_Discovery;with Firmware_Tables;
with ACPI_GPIO_Admission_Payload;with Interfaces;
-- Concrete pre-device-_INI observer on the ONE startup evaluator. This owns
-- no pool admission/IPC hook and does not change native startup sequencing.
package ACPI_Native_GPIO_Observation is
   function Hardware_Quiet(Service:ACPI_Service.State) return Boolean;
   package Driver is new ACPI_GPIO_Bootstrap_Discovery
     (ACPI_Service,ACPI_One_Service.Root.Evaluation.Evaluate_Named,
      ACPI_Service.Release_Result,Hardware_Quiet);
   package Payload is new ACPI_GPIO_Admission_Payload
     (ACPI_Service,Driver.Observation,Driver.Session,Driver.Description,Driver.Is_Current);
   -- Copied facts only. The owner supplies the full process-incarnation epoch
   -- for its one startup catalog. This does not close IO, send IPC or run INI.
   procedure Copy_Admission_Payload(Service:ACPI_Service.State;
      Epoch:Interfaces.Unsigned_64;History:Payload.Initializer_Provenance;Data:out Payload.P.Request;Result:out Payload.Outcome);
   procedure Discover(Service:aliased in out ACPI_Service.State;
      Budget:Natural;Data:out Firmware_Tables.Bytes;
      Result:out Driver.Report;Action:out Driver.Action_Result);
   procedure Copy_Resources(Service:in out ACPI_Service.State;Data:out Firmware_Tables.Bytes;
      Result:out Driver.Report;Action:out Driver.Action_Result);
   procedure Close(Service:in out ACPI_Service.State;Result:out Driver.Report;Action:out Driver.Action_Result);
   function Description return Driver.Observation.Metadata;
   function Pending return Boolean;
   function Is_Current(Service:ACPI_Service.State) return Boolean;
   -- Outer owner must establish its full quiet IPC boundary before forwarding
   -- the copied facts; acceptance of a normal platform report is not admission.
end ACPI_Native_GPIO_Observation;
