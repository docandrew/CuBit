pragma Ada_2022;
with Interfaces;
with ACPI_EC_Types;
with ACPI_EC_Query_Scans;
with ACPI_EC_Event_Types;
with ACPI_EC_Clock_Policy;
with ACPI_EC_Transactions;
with ACPI_EC_Registers;
with ACPI_EC_Endpoint;
with ACPI_EC_Native_Calls;
with ACPI_EC_Event_Publisher;
with ACPI_EC_Backend_Progress;
with ACPI_EC_Backend_Waits;
with ACPI_EC_Backend_Controller;
with CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Handles.IPC;
-- One process-lifetime privileged backend per admitted EC interface. Serialize
-- all entries; callbacks are bounded, normally returning and nonreentrant.
-- Owned means command-wide exclusive ownership or an actually held Global Lock.
-- This composition supplies neither that ownership nor register/source authority.
-- Clock returns fresh monotonic microseconds; Pause uses the same epoch.
generic
   with procedure Read (Register : ACPI_EC_Types.Read_Register;
      Value : out ACPI_EC_Types.Byte; Result : out ACPI_EC_Types.Access_Result);
   with procedure Write (Register : ACPI_EC_Types.Write_Register;
      Value : ACPI_EC_Types.Byte; Result : out ACPI_EC_Types.Access_Result);
   with function Owned return Boolean;
   with procedure Publish (Item : ACPI_EC_Event_Types.Notice;
      Result : out ACPI_EC_Event_Types.Publication);
   Request_Window : ACPI_EC_Types.Microseconds;
   Request_Budget, Pause_Budget : ACPI_EC_Types.Work_Budget;
   Maximum_Cells : Positive := 32;
   Maximum_Query_Requests : Positive := 16;
   Expected_Consumer : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
   with procedure Clock (Value : out ACPI_EC_Types.Microseconds;
      Available : out Boolean);
   with procedure Pause (Target : ACPI_EC_Types.Microseconds;
      Remaining : in out ACPI_EC_Types.Work_Budget;
      Result : out ACPI_EC_Clock_Policy.Pause_Report);
   with function Save return CuBit.Handles.IPC.Saved_Reply is CuBit.Handles.IPC.Save_Reply;
   with function Answer_Saved (Item : CuBit.Handles.Reply;
      Msg : CuBit.Messages.Message) return Boolean is CuBit.Handles.IPC.Answer;
   with function Answer_Current (Sender : CuBit.Messages.Process_ID;
      Msg : CuBit.Messages.Message) return Interfaces.Unsigned_64 is CuBit.Messages.reply;
package ACPI_EC_Backend_Owner is
   type Register_ID is new Interfaces.Unsigned_64;
   type Epoch_ID is new Interfaces.Unsigned_64;
   type Admitted_Cell is record
      Register : Register_ID := 0;
      Epoch : Epoch_ID := 0;
      Owner : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
      Cell : ACPI_EC_Types.Cell_Offset := 0;
      Readable, Writable, Exact_Write_Safe : Boolean := False;
      Write_Mask : ACPI_EC_Types.Byte := 0;
   end record;
   type Cell_List is array (Positive range <>) of Admitted_Cell;
   type Setup_Status is (Not_Configured, Ready, Setup_Refused);
   type Setup_Result is (Configured, Already_Attempted, Invalid_Policy,
                         Invalid_Route, Invalid_Cell, Too_Many_Cells);
   -- Trusted independently admitted records only, never caller wire words or
   -- firmware declarations. One attempt, including a failed/partial install.
   -- Failure keeps every submission/receive path closed, without hardware IO.
   procedure Configure (Cells : Cell_List;
      Controller : ACPI_EC_Event_Types.Controller_ID;
      Epoch : ACPI_EC_Event_Types.Epoch_ID; Result : out Setup_Result);
   function Configuration return Setup_Status;
   -- Subsequent independently admitted changes remain serialized trusted work.
   -- The existing catalog protects active records and retained epochs.
   procedure Install (Item : Admitted_Cell; Accepted : out Boolean);
   procedure Revoke (Register : Register_ID; Epoch : Epoch_ID; Accepted : out Boolean);
   type Call_Status is (Not_Ready, Receive_Owned, Serving_Stopped, Admitted, Refused, Clock_Unavailable,
                        Deadline_Overflow, Invalid_Request_Policy);
   type Reply_State is (No_Field, Awaiting_Transaction, Immediate_Reply, Completed_Reply,
      Save_Refused, Reply_Owner_Fault, Correlation_Fault);
   type Call_Report is record
      Status : Call_Status := Not_Ready;
      Phase : Reply_State := No_Field;
      Replied : Boolean := False;
   end record;
   -- Before receive ownership is claimed, this is the standalone call route.
   -- Not_Ready/Receive_Owned/Serving_Stopped preserve current reply authority.
   procedure Begin_Call (Sender : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Result : out Call_Report);
   type Query_Status is (Query_Not_Ready, Query_Receive_Owned, Query_Serving_Stopped,
      Query_Started, Query_Busy, Query_Denied,
      Query_Engine_Unavailable, Query_Exhausted, Query_Clock_Unavailable,
      Query_Deadline_Overflow, Query_Invalid_Policy);
   -- Trusted independently admitted source only; not a public wire operation.
   procedure Start_Query (Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Result : out Query_Status);
   type Query_Request_Count is new Natural range 0 .. Maximum_Query_Requests;
   type Query_Request_Result is (Request_Not_Ready, Request_Outside_Event,
      Request_Stopped, Request_Queued, Request_Overflow);
   -- Admitted event hook only. Each trigger keeps its own bounded publication
   -- budget until scheduled; there is no implicit coalescing or replay.
   -- Overflow is retained and causes the receive owner to stop new work.
   procedure Request_Query (Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Result : out Query_Request_Result);
   subtype Query_Scan_Budget is ACPI_EC_Query_Scans.Allowance;
   -- Admitted source hook only. One FIFO entry retains a bounded scan until
   -- an accepted query observes SCI clear or returns zero. Each iteration is
   -- a new transaction, with its own original request/publication allowance.
   -- No new query starts while its predecessor's publication is retained.
   procedure Request_Query_Scan
     (Attempts : ACPI_EC_Event_Types.Delivery_Budget; Queries : Query_Scan_Budget;
      Result : out Query_Request_Result);
   -- Trusted serialized owner only. Extend only a successfully exhausted scan,
   -- never an accepted transaction, failed query or unknown publication. No IO,
   -- clock call, retry or source re-enable occurs here. The supervisor must
   -- arrange a wake/turn after explicit extension; exhaustion may idle for
   -- control work and does not suppress ordinary Field requests.
   procedure Resume_Query_Scan (Queries : Query_Scan_Budget; Accepted : out Boolean);
   type Event_State is (Event_Unconfigured, Event_Configuration_Refused, Event_Ready,
      Event_Querying, Event_Publication_Pending, Event_Delivery_Stalled,
      Event_Delivery_Unknown, Event_Query_Failed, Event_Correlation_Failed);
   type Operation_ID is new Interfaces.Unsigned_64;
   type Query_Response is (No_Query_Response, No_Pending_Event,
      Zero_Query_Response, Event_Response);
   type Query_Scan_Phase is (No_Head_Scan, Scan_Queued, Scan_Waiting,
      Scan_Exhausted, Scan_Failed);
   type Query_Scan_Report is record
      Phase : Query_Scan_Phase := No_Head_Scan;
      Remaining : ACPI_EC_Types.Work_Budget := 0;
      Last_Empty_Response : Query_Response := No_Query_Response;
   end record;
   type Field_Completion is record
      Present : Boolean := False;
      Token : Operation_ID := 0;
      Hardware : ACPI_EC_Types.Outcome := ACPI_EC_Types.Idle;
      Delivered, Effects_Possible, Recovery_Retained : Boolean := False;
   end record;
   type Observation is record
      Setup : Setup_Status := Not_Configured;
      Hardware : ACPI_EC_Types.Outcome := ACPI_EC_Types.Idle;
      Catalog_Active, Field_Reply_Pending, Field_Faulted, Hardware_Recovery : Boolean := False;
      Receive_Owned, Serving_Stopped : Boolean := False;
      Last_Field_Completion : Field_Completion;
      Last_Field_Admission : Call_Report;
      Queued_Queries : Query_Request_Count := 0;
      Query_Request_Overflow : Boolean := False;
      Last_Queued_Query_Status : Query_Status := Query_Not_Ready;
      Last_Query_Outcome : ACPI_EC_Types.Outcome := ACPI_EC_Types.Idle;
      Last_Query_Response : Query_Response := No_Query_Response;
      Last_Query_Event : ACPI_EC_Types.Byte := 0;
      Scan : Query_Scan_Report;
      Event_Phase : Event_State := Event_Unconfigured;
      Notice : ACPI_EC_Event_Types.Notice_Option;
      Remaining_Pauses : ACPI_EC_Types.Work_Budget := 0;
   end record;
   -- Histories are the last actual Field admission/attributable completion,
   -- not the current request or evidence of global quiescence. They survive
   -- engine acknowledgement, failed answer delivery and receiver stop.
   -- Query response/event survive publication/claim independently of Notice.
   -- A new accepted query clears them; in-flight/failed work has no response.
   -- An empty response is a past observation, not proof of source quiescence.
   function Observe return Observation;
   type Progress_Action is (No_Work, Advance_Field, Advance_Query, Publish_Notice,
      Wait_Until, Owner_Fault, Inconsistent_Owners);
   type Progress_Report is record
      Action : Progress_Action := No_Work;
      Advanced, Clock_Available, Wait_Checked, Stop_Requested : Boolean := False;
      State : Observation;
   end record;
   -- Direct progress closes once a receive child claims this owner. The
   -- child then owns all advancement/draining; there is no release/reset.
   procedure Advance (Result : out Progress_Report);
   procedure Confirm_Claim (Sender : CuBit.Process_IDs.Process_ID;
      Item : ACPI_EC_Event_Types.Notice; Accepted : out Boolean);
   procedure Resume_Publication (Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Accepted : out Boolean);
   procedure Resume_Queries (Accepted : out Boolean);
   -- No reset, release, ownership transfer or engine escape is provided.
   -- Retain the instance and all backing even after reply/notice delivery.
private
   package Scans renames ACPI_EC_Query_Scans;
   package Engine is new ACPI_EC_Transactions (Read, Write, Clock, Owned);
   package Registers is new ACPI_EC_Registers (Engine, Maximum_Cells);
   package Endpoint is new ACPI_EC_Endpoint (Registers);
   package Calls is new ACPI_EC_Native_Calls
     (Registers, Endpoint, Save, Answer_Saved, Answer_Current);
   package Events is new ACPI_EC_Event_Publisher (Registers, Publish, Expected_Consumer);
   package Progress is new ACPI_EC_Backend_Progress (Registers, Calls, Events);
   package Waits is new ACPI_EC_Backend_Waits (Registers, Progress, Pause);
   package Controller is new ACPI_EC_Backend_Controller
     (Registers, Calls, Events, Progress, Waits, Clock,
      Request_Window, Request_Budget, Pause_Budget, Answer_Current);
   function Call_Of (Item : Controller.Call_Report) return Call_Report;
   procedure Note_Admission (Item : Controller.Call_Report);
   Last_Admission : Call_Report;
   procedure Note_Completion (Item : Calls.Report);
   Last_Completion : Field_Completion;
   Current_Setup : Setup_Status := Not_Configured;
   Receiver_Claimed, Receiver_Closed, Query_Event_Window : Boolean := False;
   subtype Query_Slot is Positive range 1 .. Maximum_Query_Requests;
   type Query_Request_Kind is (Single_Query, Query_Scan);
   type Queued_Query is record
      Kind : Query_Request_Kind := Single_Query;
      Attempts : ACPI_EC_Event_Types.Delivery_Budget := 1;
      Scan : Scans.State;
   end record;
   type Query_Queue is array (Query_Slot) of Queued_Query;
   Query_Requests : Query_Queue := [others => <>];
   Query_Head, Query_Tail : Query_Slot := Query_Slot'First;
   Query_Count : Query_Request_Count := 0;
   Query_Overflow, Prefer_Field : Boolean := False;
   Queued_Query_Status : Query_Status := Query_Not_Ready;
   Last_Scan_Empty : Query_Response := No_Query_Response;
end ACPI_EC_Backend_Owner;
