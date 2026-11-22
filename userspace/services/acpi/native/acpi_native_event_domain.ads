pragma Ada_2022;
with ACPI_Requests;with ACPI_Cached_Power_Values;
with ACPI_Cached_Battery_Values;
with ACPI_Native_Server;
with ACPI_SCI_Events;with ACPI_SCI_Registers;with ACPI_SCI_Quiesce;
with ACPI_Native_EC_Discovery;
-- Concrete serialized native integration, not an observer/wire execution API.
package ACPI_Native_Event_Domain is
   type Phase is (Fresh,Starting,Live,Fixed_Only,Stopping,Closed,Failed);
   type Failure is (None,Not_Ready,Consumer_Start_Refused,Routing_Refused,
      EC_Startup_Refused,Consumer_Work_Refused,EC_Work_Refused,
      Control_Not_Exclusive,GPE_Startup_Refused,GPE_Work_Refused,Button_Discovery_Refused);
   type Start_Outcome is (Ready,EC_Unavailable,SCI_Unavailable,Refused);
   type Report is record
      Stage:Phase:=Fresh;
      Why:Failure:=None;
      EC:ACPI_Native_EC_Discovery.Outcome:=ACPI_Native_EC_Discovery.Not_Attempted;
      Consumers_Started,EC_Armed:Boolean:=False;
      General_GPEs:Natural range 0..ACPI_SCI_Registers.GPE_Count:=0;
   end record;
   function Observe return Report;
   -- Copy existing lid/AC caches only, using one trusted native clock read.
   -- No AML, refresh, receipt claim/ACK, logging, IPC or hardware operation.
   -- Unavailable views have empty values; ages use the native clock epoch.
   procedure Read_Power (Server : ACPI_Requests.State;
      Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Power_Values.Snapshot)
     with Pre => ACPI_Requests.Valid (Server);
   function Attempted return Boolean;
   -- Same settled-domain admission as Read_Power; copied descriptive data only.
   procedure Read_Batteries (Server : ACPI_Requests.State;
      Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Battery_Values.View)
     with Pre => ACPI_Requests.Valid (Server);
   -- Startup-only metadata/receipt preparation. No EC/GPE enrollment or AML.
   procedure Prepare_Consumers(Server:in out ACPI_Requests.State;Accepted:out Boolean)
     with Pre=>ACPI_Requests.Valid(Server);
   -- One bounded captured-notification batch; no fresh family sampling.
   procedure Route_Prepared(Server:in out ACPI_Requests.State;Result:out ACPI_Native_Server.Owner_Outcome)
     with Pre=>ACPI_Requests.Valid(Server);
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
