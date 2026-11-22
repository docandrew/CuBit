pragma Ada_2022;
with System;
with ACPI_Native_Initializer_History;
with ACPI_One_Service;
with ACPI_Native_Delays;
with ACPI_Native_GPIO_Admission;
with ACPI_Fallible_Heap;
with CuBit.Messages;
with Firmware_Tables;
package body ACPI_Native_Pre_Device is
   package C renames ACPI_Service;
   package One renames ACPI_One_Service;
   package Fields renames One.Root.Fields;
   package GPIO renames ACPI_Native_GPIO_Observation;
   package GP renames GPIO.Payload;
   use type Interfaces.Unsigned_32,Interfaces.Unsigned_64, System.Address, CuBit.Handles.Endpoint;
   use type GPIO.Driver.Outcome, GP.Outcome, One.Runtime.Status.Phase;
   View : Report;
   Endpoint : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   Original_Deadline : Interfaces.Unsigned_64 := 0;
   Roots_Released : Boolean := False;
   Admission_Request_ID : constant Interfaces.Unsigned_64 := 1;
   Captured_Payload : GP.P.Request := GP.Empty_Payload;
   Captured_Device : C.Node_Descriptor := C.Values.No_Node;
   Payload_Captured : Boolean := False;
   function Physical_Quiet return Boolean is
     (Fields.Control_Idle and then not Fields.Observe.IO_Pending
      and then not Fields.Uncertain.Present and then Fields.EC_Commands.Quiescent);
   function Send_Boundary return Boolean is
      Deadline : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      if not View.Paused or else not Roots_Released or else GPIO.Pending
        or else One.Runtime.Current/=One.Runtime.Status.Fresh or else not Physical_Quiet
      then return False; end if;
      ACPI_Native_Delays.Inspect_Paused_Startup(Deadline,Available);
      return Available and then Deadline=Original_Deadline;
   end Send_Boundary;
   package Transport is new ACPI_Native_GPIO_Admission(Send_Boundary);
   function Observe return Report is (View);
   function Quiet return Boolean is
     (not View.Working and then not GPIO.Pending and then Transport.Quiet);
   function Submitted return Submitted_Request is
      Info : constant Transport.Report := Transport.Observe;
   begin
      if View.Working or else not Payload_Captured or else not Info.Dispatched then
         return (Present => False);
      end if;
      return (Present => True, Data => Captured_Payload, Device => Captured_Device,
         Request_ID => Admission_Request_ID, Receipt_Known => Info.Reply_Known,
         Answer => Info.Answer);
   end Submitted;
   procedure Capture_Transport is
      Info : constant Transport.Report := Transport.Observe;
   begin
      View.Admission_Attempted:=Info.Attempted; View.Admission_Known:=Info.Reply_Known;
      View.Grant_Pending:=Info.Grant_Pending; View.Memory_Owned:=Info.Memory_Owned;
      View.Answer:=Info.Answer; View.Detail:=Info.Detail; View.Members:=Info.Members;
   end Capture_Transport;
   procedure Configure (Target : CuBit.Handles.Endpoint;
      Original_Deadline_Microseconds : Interfaces.Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted:=False;
      if View.Stage/=Fresh or else Original_Deadline_Microseconds=0
        or else Original_Deadline_Microseconds=Interfaces.Unsigned_64'Last then return; end if;
      Endpoint:=Target; Original_Deadline:=Original_Deadline_Microseconds;
      View.Stage:=Configured; Accepted:=True;
   end Configure;
   procedure Prepare (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Accepted : out Boolean) is
      Empty : Firmware_Tables.Bytes(1 .. 0);
      Action : GPIO.Driver.Action_Result;
      Data : GP.P.Request;
      Payload : GP.Outcome;
      Response : Transport.Report;
      Paused,Resumed : Boolean;
      Prefix : constant ACPI_Native_Initializer_History.Report := ACPI_Native_Initializer_History.Observe;
      procedure Fail (Why : Failure) is
      begin
         View.Why:=Why;
         View.Stage:=(if GPIO.Pending or else not Transport.Quiet then Retained else Failed);
      end Fail;
      procedure Attempt is
      begin
         if One.Runtime.Current/=One.Runtime.Status.Fresh or else not Physical_Quiet then
            Fail(Physical_IO_Busy); return;
         end if;
         View.Initializer_Invocations:=Prefix.Invocations;
         View.Initializer_Ordered_Digest:=Prefix.Ordered_Digest;
         View.Initializer_History_Complete:=Prefix.Complete;
         if not Prefix.Complete then Fail(Incomplete_Initializer_History);return;end if;
         View.Stage:=Observing;
         GPIO.Discover(Service,Budget,Empty,View.Discovery,Action);
         View.Charged:=View.Discovery.Charged;
         if View.Discovery.Status=GPIO.Driver.Capacity_Required then
            declare
               Required : constant Natural:=Natural(View.Discovery.Observation_Result.Required);
               Address : System.Address := System.Null_Address;
            begin
               if Required=0 then Fail(Resource_Allocation_Refused); return; end if;
               Address:=ACPI_Fallible_Heap.Reserve
                 (Interfaces.Unsigned_64(Required),Interfaces.Unsigned_64(Firmware_Tables.Byte'Alignment));
               if Address=System.Null_Address then
                  Fail(Resource_Allocation_Refused); return;
               end if;
               View.Stage:=Copying;
               declare
                  Base : constant System.Address := Address;
                  Bytes : Firmware_Tables.Bytes(1 .. Required) with Import,Address=>Base;
               begin GPIO.Copy_Resources(Service,Bytes,View.Discovery,Action); end;
               ACPI_Fallible_Heap.Release(Address);
               View.Charged:=View.Discovery.Charged;
            end;
         end if;
         if View.Discovery.Fatal.Present then Fail(Fatal_AML); return; end if;
         if View.Discovery.Field_Uncertainty or else Fields.Uncertain.Present then Fail(Field_Uncertain); return; end if;
         Roots_Released:=not GPIO.Pending and then C.Retained_Results(Service)=0;
         if not Roots_Released then Fail(Observation_Unavailable); return; end if;
         declare Current : constant ACPI_Native_Initializer_History.Report := ACPI_Native_Initializer_History.Observe;begin
            if not Current.Complete or else Current.Invocations/=Prefix.Invocations
              or else Current.Ordered_Digest/=Prefix.Ordered_Digest then
               Fail(Initializer_History_Changed);return;
            end if;
         end;
         if View.Discovery.Status=GPIO.Driver.No_Supported_Identity_Observed then
            View.Stage:=Prepared_Without_Admission; View.Why:=No_Observed_Controller; Accepted:=True; return;
         elsif View.Discovery.Status/=GPIO.Driver.Described then Fail(Observation_Unavailable); return; end if;
         GPIO.Copy_Admission_Payload(Service,CuBit.Messages.To_Word(CuBit.Messages.Own_Process),
            (Complete=>Prefix.Complete,Invocations=>Prefix.Invocations,Ordered_Digest=>Prefix.Ordered_Digest),Data,Payload);
         if Payload/=GP.Payload_Ready then Fail(Payload_Refused); return; end if;
         -- Copy before lending the page; no later diagnostics/probe can
         -- replace the provenance of an already dispatched request.
         Captured_Payload := Data;
         Captured_Device := GPIO.Description.Device;
         Payload_Captured := True;
         if Endpoint=CuBit.Handles.No_Endpoint then
            View.Stage:=Prepared_Without_Admission; View.Why:=No_Endpoint; Accepted:=True; return;
         end if;
         if not Physical_Quiet then Fail(Physical_IO_Busy); return; end if;
         View.Stage:=Pausing;
         ACPI_Native_Delays.Pause_Startup(Paused);
         View.Paused:=Paused;
         if not Paused then Fail(Delay_Pause_Refused); return; end if;
         View.Stage:=Submitting;
         Transport.Submit(Endpoint,Data,Admission_Request_ID,Response);
         if Response.State in Transport.Completion_Unknown | Transport.Invalid_Reply then
            View.Stage:=Confirming; Transport.Confirm(Response);
         end if;
         Capture_Transport;
         if not Response.Reply_Known and then Response.Dispatched then Fail(Admission_Unknown); return; end if;
         if not Transport.Quiet then Fail(Grant_Cleanup_Pending); return; end if;
         if not Physical_Quiet or else GPIO.Pending or else C.Retained_Results(Service)/=0 then
            Fail(Physical_IO_Busy); return;
         end if;
         View.Stage:=Resuming;
         ACPI_Native_Delays.Resume_Startup(Resumed);
         if not Resumed then Fail(Resume_Refused); return; end if;
         View.Paused:=False;
         if not Response.Reply_Known then View.Stage:=Prepared_Without_Admission; View.Why:=Admission_Refused;
         else View.Stage:=Prepared; end if;
         Accepted:=True;
      end Attempt;
   begin
      Charged:=0; Accepted:=False;
      if View.Used or else View.Working then return; end if;
      if View.Stage/=Configured then View.Why:=Not_Configured; return; end if;
      View.Used:=True; View.Working:=True;
      Attempt;
      Charged:=View.Charged; View.Working:=False;
   end Prepare;
   procedure Close (Service : in out C.State) is
      Action : GPIO.Driver.Action_Result;
   begin
      if View.Working then return; end if;
      View.Working:=True;
      if GPIO.Pending then GPIO.Close(Service,View.Discovery,Action); end if;
      -- Cleanup never makes a new peer request, even while startup still has
      -- time left. Unknown completion is retained independently of the page.
      Transport.Abandon_Confirmation; Capture_Transport;
      View.Working:=False;
   end Close;
end ACPI_Native_Pre_Device;
