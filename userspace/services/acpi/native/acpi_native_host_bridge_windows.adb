pragma Ada_2022;
with ACPI_One_Service;
with ACPI_Native_Event_Domain;
with ACPI_Native_Passive_Queries;
with ACPI_Native_Power_Entry;
with ACPI_Native_Pre_Device;
with ACPI_Native_Regions;
with ACPI_Native_Delays;
with CuBit.Monotonic;
with CuBit.Messages;
with System;
package body ACPI_Native_Host_Bridge_Windows is
   package Q renames ACPI_Requests; package One renames ACPI_One_Service;
   use type O.W.Word, System.Address, One.Runtime.Status.Phase, One.Runtime.Status.Outcome;
   Has_Window : Boolean := False;
   Original_Deadline : O.W.Word := 0;
   Original_Server : System.Address := System.Null_Address;
   function Metadata_Ready (Server : Q.State) return Boolean is
     (Q.Valid (Server) and then Q.Device_Walk_Finished (Server)
      and then not Has_Window and then One.Runtime.Current = One.Runtime.Status.Idle
      and then ACPI_Native_Pre_Device.Quiet
      and then ACPI_Native_Passive_Queries.Quiet
      and then not ACPI_Native_Power_Entry.Blocks_Runtime
      and then ACPI_Native_Regions.May_Open_Diagnostic_Channel
      and then not ACPI_Native_Regions.Observe.Working
      and then not ACPI_Native_Regions.Observe.Pending
      and then ACPI_Native_Event_Domain.Diagnostics_Quiet (Server));
   function Query_Ready (Server : Q.State) return Boolean is (Metadata_Ready (Server));
   function Current return Boolean is
      Deadline : O.W.Word; Accepted : Boolean;
   begin
      if not Has_Window or else One.Runtime.Current /= One.Runtime.Status.Active
        or else ACPI_Native_Power_Entry.Blocks_Runtime then return False; end if;
      ACPI_Native_Delays.Inspect_Window (Deadline, Accepted);
      return Accepted and then Deadline = Original_Deadline;
   end Current;
   procedure Clock (Microseconds, Milliseconds : out O.W.Word; Available : out Boolean) is
      Sample : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      Microseconds := 0; Milliseconds := CuBit.Monotonic.Milliseconds;
      Available := Sample.Available and then Milliseconds /= CuBit.Messages.Wait_Forever;
      if Sample.Available then
         Microseconds := Sample.Microseconds;
         Available := Available and then Microseconds /= CuBit.Messages.Wait_Forever;
      end if;
   end Clock;
   procedure Begin_Window (Server : in out Q.State; Deadline_Microseconds : O.W.Word; Result : out O.Window_Result) is
      Outcome : One.Runtime.Status.Outcome;
   begin
      Result := O.Uncertain;
      if Has_Window then return; end if;
      if not Query_Ready (Server) then Result := O.Quiet_Refusal; return; end if;
      One.Runtime_Access.Begin_Event (Server, Deadline_Microseconds, Outcome);
      if Outcome = One.Runtime.Status.Event_Opened and then One.Runtime.Current = One.Runtime.Status.Active then
         Has_Window := True; Original_Deadline := Deadline_Microseconds; Original_Server := Server'Address;
         Result := O.Opened;
      elsif Outcome = One.Runtime.Status.Rejected and then Query_Ready (Server) then
         Result := O.Quiet_Refusal;
      end if;
   end Begin_Window;
   procedure End_Window (Server : in out Q.State; Healthy : Boolean;
      Closed : out Boolean; Now_Milliseconds : out O.W.Word) is
      Outcome : One.Runtime.Status.Outcome;
   begin
      Closed := False; Now_Milliseconds := CuBit.Messages.Wait_Forever;
      if not Has_Window or else Original_Server /= Server'Address then return; end if;
      -- Cleanup stays reachable after query admission/deadline closes. The
      -- actual runtime authenticates its original private Core independently.
      One.Runtime_Access.End_Event (Server, Healthy, Outcome);
      Closed := Healthy and then Outcome = One.Runtime.Status.Event_Completed
        and then One.Runtime.Current = One.Runtime.Status.Idle
        and then not Q.Retained_Results_Pending (Server);
      if Closed then
         Has_Window := False; Original_Deadline := 0; Original_Server := System.Null_Address;
         Now_Milliseconds := CuBit.Monotonic.Milliseconds;
      end if;
   end End_Window;
end ACPI_Native_Host_Bridge_Windows;
