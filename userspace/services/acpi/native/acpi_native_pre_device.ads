pragma Ada_2022;
with ACPI_Service;
with ACPI_Native_GPIO_Observation;
with CuBit.Handles;
with CuBit.GPIO_Bootstrap_Exchange;
with Interfaces;
with GPIO_Bootstrap_Payload;
-- Actual one-shot owner after provider-prefix INI/REG and before consumers.
-- Shares the existing Core/evaluator; no replacement interpreter or replay.
package ACPI_Native_Pre_Device is
   type Phase is (Fresh, Configured, Observing, Copying, Pausing, Submitting,
      Confirming, Resuming, Prepared, Prepared_Without_Admission, Failed, Retained);
   type Failure is (None, Already_Consumed, Not_Configured, No_Observed_Controller,
      Observation_Unavailable, Resource_Allocation_Refused, Payload_Refused,
      No_Endpoint, Physical_IO_Busy, Delay_Pause_Refused, Admission_Unknown,
      Admission_Refused, Grant_Cleanup_Pending, Resume_Refused, Fatal_AML, Field_Uncertain,Incomplete_Initializer_History,Initializer_History_Changed);
   type Report is record
      Stage : Phase := Fresh;
      Why : Failure := None;
      Used, Working, Paused, Admission_Attempted, Admission_Known : Boolean := False;
      Grant_Pending, Memory_Owned : Boolean := False;
      Charged : Natural := 0;
      Initializer_Invocations : Interfaces.Unsigned_32 := 0;
      Initializer_Ordered_Digest : Interfaces.Unsigned_64 := 0;
      Initializer_History_Complete : Boolean := False;
      Discovery : ACPI_Native_GPIO_Observation.Driver.Report;
      Answer : CuBit.GPIO_Bootstrap_Exchange.Disposition := CuBit.GPIO_Bootstrap_Exchange.Malformed;
      Detail, Members : Interfaces.Unsigned_64 := 0;
   end record;
   procedure Configure (Target : CuBit.Handles.Endpoint;
      Original_Deadline_Microseconds : Interfaces.Unsigned_64; Accepted : out Boolean);
   procedure Prepare (Service : aliased in out ACPI_Service.State; Budget : Natural;
      Charged : out Natural; Accepted : out Boolean);
   -- Bounded attribution/cleanup only; never scans/evaluates AML again.
   procedure Close (Service : in out ACPI_Service.State);
   function Observe return Report;
   function Quiet return Boolean;
   type Submitted_Request (Present : Boolean := False) is record
      case Present is
         when True =>
            Data : GPIO_Bootstrap_Payload.Request;
            Device : ACPI_Service.Node_Descriptor;
            Request_ID : Interfaces.Unsigned_64;
            Receipt_Known : Boolean;
            Answer : CuBit.GPIO_Bootstrap_Exchange.Disposition;
         when False => null;
      end case;
   end record;
   -- Exact immutable facts used by the one dispatched admission request.
   -- No AML/IPC/refresh, and no inference that an unknown/refused receipt
   -- admitted hardware. The caller checks the attributable receipt, current
   -- same-Core Device binding and its own handoff/cleanup boundary separately.
   -- A captured payload that never dispatched is not a submitted request.
   function Submitted return Submitted_Request;
end ACPI_Native_Pre_Device;
