pragma Ada_2022;
with ACPI_Requests;
with ACPI_Native_Server;
with ACPI_SCI_Events;with ACPI_SCI_Registers;with ACPI_SCI_Quiesce;
with ACPI_Native_EC_Discovery;
-- Concrete serialized native integration, not an observer/wire execution API.
package ACPI_Native_Event_Domain is
   type Phase is (Fresh,Starting,Live,Fixed_Only,Stopping,Closed,Failed);
   type Failure is (None,Not_Ready,Consumer_Start_Refused,Routing_Refused,
      EC_Startup_Refused,Consumer_Work_Refused,EC_Work_Refused);
   type Start_Outcome is (Ready,EC_Unavailable,SCI_Unavailable,Refused);
   type Report is record
      Stage:Phase:=Fresh;
      Why:Failure:=None;
      EC:ACPI_Native_EC_Discovery.Outcome:=ACPI_Native_EC_Discovery.Not_Attempted;
      Consumers_Started,EC_Armed:Boolean:=False;
   end record;
   function Observe return Report;
   function Attempted return Boolean;
   procedure Start(Server:in out ACPI_Requests.State;Result:out Start_Outcome)
     with Pre=>ACPI_Requests.Valid(Server);
   procedure Fixed_Event(Event:ACPI_SCI_Registers.Fixed_Event;Result:out ACPI_SCI_Events.Disposition);
   procedure GPE_Event(GPE:ACPI_SCI_Registers.GPE_Number;Result:out ACPI_SCI_Events.Disposition);
   function Diagnostics_Quiet(Server:ACPI_Requests.State) return Boolean
     with Pre=>ACPI_Requests.Valid(Server);
   procedure Advance(Server:in out ACPI_Requests.State;Result:out ACPI_Native_Server.Owner_Outcome)
     with Pre=>ACPI_Requests.Valid(Server);
   procedure Begin_Stop;
   procedure Confirm_Disarm(Result:ACPI_SCI_Quiesce.Result);
   type Drain_Outcome is (Drained,Pending,Retained);
   -- One bounded receipt batch and one copied fact per call, no new AML.
   -- Pending means actual work progressed; Retained stops unchanged retries.
   -- Keep consumer routes alive while the caller disconnects regions once.
   procedure Drain(Server:in out ACPI_Requests.State;Result:out Drain_Outcome)
     with Pre=>ACPI_Requests.Valid(Server);
   procedure Close(Server:in out ACPI_Requests.State;Result:out Drain_Outcome)
     with Pre=>ACPI_Requests.Valid(Server);
   function Quiet(Server:ACPI_Requests.State) return Boolean
     with Pre=>ACPI_Requests.Valid(Server);
   -- Derive finite close allowance from the actual FIFO and fixed fact slots.
   function Shutdown_Turns return Positive;
end ACPI_Native_Event_Domain;
