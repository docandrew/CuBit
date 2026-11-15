pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_Requests;
with ACPI_Native_HID_Detection;
with ACPI_Native_HID_Collection;
with ACPI_Service;
with GPIO_Bootstrap_Payload;
generic
   -- Same serialized native instance: runtime Idle, physical/provider/EC
   -- quiet, no pending notifications/results/region control or pre-device work.
   -- Do not bind this to a predicate containing this producer's own Quiet.
   with function Owner_Quiet return Boolean;
   with procedure Collect_Offers
     (Item : in out ACPI_Native_HID_Collection.Controller.Run; Server : in out ACPI_Requests.State;
      Devices : ACPI_Native_HID_Collection.Controller.Device_Array;
      Count : ACPI_Native_HID_Collection.Controller.Candidate_Count;
      Admission_Device : ACPI_Service.Node_Descriptor;
      Admission : GPIO_Bootstrap_Payload.Request; History : ACPI_Native_HID_Collection.Provenance.Stamp;
      Budget : Natural; Resource_Bytes : Positive;
      Result : out ACPI_Native_HID_Collection.Controller.Report)
     is ACPI_Native_HID_Collection.Controller.Collect;
package ACPI_Native_HID_Producer is
   subtype Candidate_Count is Natural range 0 .. ACPI_Native_HID_Detection.Maximum_Candidates;
   type Outcome is (Not_Attempted, Collecting, Submitting, No_Devices,
      Discovery_Incomplete, Admission_Unavailable, History_Incomplete,
      Boundary_Refused, Window_Refused, Collection_Failed, Device_Unavailable, Recovery_Required,
      Request_ID_Exhausted, Offers_Refused, Offers_Queued, Handoff_Unsettled);
   type Report is record
      Status : Outcome := Not_Attempted;
      Attempted, Working, Window_Open, Recovery_Needed : Boolean := False;
      Candidates, Dispatched, Known, Queued : Candidate_Count := 0;
      Charged : Natural := 0;
      Node_Errors : Natural := 0;
      Last_Request, Last_Detail : Interfaces.Unsigned_64 := 0;
      Status_Known : Boolean := False;
      Status_Flags : Interfaces.Unsigned_64 := 0;
   end record;
   function Name (Status : Outcome) return String;
   -- Explicit names remain readable when native Discard_Names is enabled.
   function Last_Transport return String;
   function Last_Receipt return String;
   function Observe return Report;
   -- Detached fresh-collection diagnostics and historical descriptions only.
   -- Does not expose the owning Run, Core, retained values or transport state.
   function Collection_Observation return ACPI_Native_HID_Collection.Controller.Report;
   -- Process-lifetime singleton; no replay, reset, fresh deadline or new Core.
   -- Invoke after ALL other startup AML (including event Configure) and before
   -- the consumer initialization declaration. No later startup AML may follow.
   -- Microsecond and millisecond deadlines retain their original clock epochs.
   procedure Start (Server : in out ACPI_Requests.State;
      Discovery : ACPI_Native_HID_Detection.Report;
      Target : CuBit.Handles.Endpoint;
      Original_Deadline_Microseconds, Original_Deadline_Milliseconds : Interfaces.Unsigned_64;
      Result : out Report);
   -- One bounded cleanup observation per session; no further confirmation or
   -- AML. Unresolved peer outcomes persist even after grant retirement.
   procedure Advance_Cleanup;
   function Quiet return Boolean;
   -- A queued or unknown dispatched offer may leave a future/live child.
   -- Until child-quiescence acknowledgement exists, retain Core/mappings even
   -- when the page retired or completion declaration was never dispatched.
   function Retention_Required return Boolean;
end ACPI_Native_HID_Producer;
