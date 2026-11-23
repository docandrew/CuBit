pragma Ada_2022;
with ACPI_Requests;
with ACPI_Service;
with ACPI_Host_Bridge_Publications;
with ACPI_Host_Bridge_Channel;
with CuBit.Handles;
with CuBit.Protocols;
with CuBit.Messages;
with CuBit.Control_Events;
with Interfaces;
package ACPI_Host_Bridge_Owners is
   package G renames ACPI_Host_Bridge_Publications;
   package B renames ACPI_Host_Bridge_Channel;
   package W renames B.W;
   type Window_Result is (Opened, Quiet_Refusal, Uncertain);
   type Phase is (Fresh, Capturing, Starting_Query, Copying_Query,
      Waiting_Open, Waiting_Receipt, Retiring, Holding, Stopping, Finished, Retained,
      Suspended);
   type Failure is (None, Invalid_Setup, Metadata_Refused, Enumeration_Incomplete,
      Allocation_Refused, Clock_Failed, Deadline_Expired, Correlation_Refused,
      Window_Refused, Wrong_Owner, Query_Uncertain, Transport_Uncertain,
      Peer_Refused, Transport_Refused, Transport_Ended, Reentered, Unexpected_Fault);
   type Step_Result is (Idle, Runnable, Failed);
   type Report is record
      Stage : Phase := Fresh;
      Why : Failure := None;
      Attempted, Working, Stop_Requested, Metadata_Claim, Window_Open : Boolean := False;
      Had_Errors : Boolean := False;
      Discovery_Finished : Boolean := False;
      Held_Sessions : Natural := 0;
      Candidates, Visited, Skipped, Reports_Opened, Acknowledged : Natural := 0;
      Failed_Reports, Incomplete_Reports, Complete_Reports : Natural := 0;
      Deadline_Microseconds, Deadline_Milliseconds : W.Word := 0;
   end record;
   generic
      -- The existing snapshot owner's access type, not a new Core or wire API.
      type Server_Access is access all ACPI_Requests.State;
      with function Metadata_Boundary_Ready return Boolean;
      with function Query_Boundary_Ready return Boolean;
      with function Query_Window_Current return Boolean;
      with procedure Clock (Microseconds, Milliseconds : out W.Word; Available : out Boolean);
      with procedure Begin_Window (Server : in out ACPI_Requests.State;
         Deadline_Microseconds : W.Word; Result : out Window_Result);
      with procedure End_Window (Server : in out ACPI_Requests.State;
         Healthy : Boolean; Closed : out Boolean; Now_Milliseconds : out W.Word);
      with procedure Evaluate_Named
        (Service : aliased in out ACPI_Service.State; Item : ACPI_Service.Node_Descriptor;
         Budget : Natural; Result : out ACPI_Service.Values.Result;
         Status : out ACPI_Service.Values.Access_Status);
      -- One trusted process-wide allocation, no IPC/reentry. IDs/tokens must
      -- be nonzero, globally unique in their agreed scopes and never wrapped.
      with procedure Correlation (Device : ACPI_Service.Node_Descriptor;
         Root_ID, Sequence, Open_Token, Data_Token : out W.Word; Available : out Boolean);
      Wire_Reasons : G.Reason_Codes;
   package Managers is
      function Observe return Report;
      function Blocks_AML return Boolean;
      function Current_Transport return B.Report;
      -- Owner points to the EXISTING process-lived Requests state. Keep it
      -- alive until Retired. No reset/restart or deadline renewal is provided.
      procedure Start (Owner : Server_Access; Endpoint : CuBit.Handles.Endpoint;
         Peer : B.P.Process_ID; Schema : CuBit.Protocols.Schema_Contract;
         Connector : Interfaces.Unsigned_16;
         Deadline_Microseconds, Deadline_Milliseconds : W.Word;
         Metadata_Budget : Natural; Metadata_Batch : Positive; AML_Budget : Natural;
         Maximum_Items : G.H.R.Item_Count);
      procedure Advance (Result : out Step_Result);
      procedure Complete_Open (Item : CuBit.Messages.CompletionEntry; Handled : out Boolean);
      procedure Note_Event (Item : CuBit.Control_Events.Event; Ends : out Boolean);
      procedure Prepare_Wait (Armed : out Boolean);
      procedure Disarm;
      -- End this discovery attempt without revoking acknowledged sessions.
      -- At a metadata-only boundary, ordinary AML may continue. An active
      -- query or transport is retained with its original ownership instead.
      -- No retry, new deadline, I/O or grant release occurs here. A real stop
      -- still retires the known suspended work and its acknowledged sessions.
      procedure Suspend_Discovery (Why : Failure);
      procedure Request_Stop;
      function Wake_Deadline return W.Word;
      -- This owner's obligations only, not all ACPI/hardware quiescence.
      function Retired return Boolean;
   end Managers;
end ACPI_Host_Bridge_Owners;
