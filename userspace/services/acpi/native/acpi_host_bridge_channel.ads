pragma Ada_2022;
with Interfaces;
with CuBit.Host_Bridge_Windows;
with ACPI_Host_Bridge_Snapshot;
with ACPI_Host_Bridge_Wire;
with CuBit.Channels;
with CuBit.Channel_Contracts;
with CuBit.Control_Events;
with CuBit.Grant_References;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Protocols;
with CuBit.Request_Channels;
package ACPI_Host_Bridge_Channel is
   package W renames ACPI_Host_Bridge_Wire;
   package S renames ACPI_Host_Bridge_Snapshot;
   package M renames CuBit.Host_Bridge_Windows;
   package P renames CuBit.Process_IDs;
   package RC is new CuBit.Request_Channels (M.Request, M.Result, M.Queue_Slot_Bits, M.Queue_Slot_Bits);
   type Phase is (Not_Started, Opening, Ready, Awaiting_Receipt, Receipt_Known, Closed, Setup_Refused);
   type Failure is (None, Invalid_Setup, Payload_Too_Large, Invalid_Packet,
      Open_Not_Submitted, Open_Refused, Open_Failed, Open_Target_Died,
      Open_Cancelled, Mapping_Refused,
      Receipt_Refused, Wake_Failed, Peer_Ended, Owner_Fault);
   type Report is record
      Stage : Phase := Not_Started;
      Why : Failure := None;
      Expired, Published, Acknowledged, Peer_Accepted, Poisoned : Boolean := False;
      Root_ID, Sequence, Deadline, Open_Token, Data_Token : W.Word := 0;
      Packet_Bytes : Natural := 0;
      Completion_Status : W.Word := CuBit.Messages.COMPLETION_OK;
      Acceptance : M.Acceptance := M.Malformed;
   end record;
   type Session is limited private;
   function Observe (IO : Session) return Report;
   -- All provisioning fields must come from one trusted bundle. Tokens are
   -- process-wide unique, nonzero, distinct and never reused by another owner.
   -- Encoding completes before any channel allocation/submission. A false
   -- Submitted result never permits restarting this same Session.
   procedure Begin_Open (IO : in out Session; Endpoint : CuBit.Handles.Endpoint;
      Peer : P.Process_ID; Schema : CuBit.Protocols.Schema_Contract;
      Connector : Interfaces.Unsigned_16; Open_Token, Data_Token, Deadline, Now : W.Word;
      Info : S.Header; Items : W.Rows; Source : S.Bytes; Submitted : out Boolean);
   -- Exactly one copied original open completion. Unknown/mismatched input
   -- is refused and retains the original pending setup, never a new open.
   -- A matching kernel TARGET_DIED/CANCELLED completion ends the pending
   -- request without interpreting its message as a peer reply. Closing the
   -- local channel still leaves its original grant retirement to confirm.
   -- Unknown statuses (including reserved QUEUE_OVERFLOW) retain the setup.
   procedure Complete_Open (IO : in out Session; Item : CuBit.Messages.CompletionEntry;
      Handled : out Boolean);
   -- One request, immutable until its original receipt. False has no new
   -- publication; a failed wake after publication still returns True.
   procedure Publish (IO : in out Session; Now : W.Word; Submitted : out Boolean);
   procedure Poll (IO : in out Session);
   procedure Note_Time (IO : in out Session; Now : W.Word);
   procedure Prepare_Wait (IO : in out Session; Armed : out Boolean);
   procedure Disarm (IO : in out Session);
   -- Event is already kernel-authenticated/decoded by the shared event router.
   procedure Note_Event (IO : in out Session; Event : CuBit.Control_Events.Event; Ends : out Boolean);
   -- Open pending, an uncertain/malformed receipt, or an unacknowledged
   -- publication prevents close. Expiry alone never cancels or replays it.
   -- A grant-end event after a validated receipt may close: ring detachment
   -- and watch removal touch private state only, with no ended mapping read
   -- or write. Original owned-region retirement is still required. An end
   -- event never replaces an earlier uncertain failure reason.
   procedure Close (IO : in out Session; Completed : out Boolean);
   function Retired (IO : Session) return Boolean;
private
   package GR renames CuBit.Grant_References;
   type Region_State is (Unknown_Region, No_Region, Owned_Region);
   type Session is limited record
      View : Report;
      Attempted, Working, Pending_Open, Bound, Watching, Routed : Boolean := False;
      Owner, Peer : P.Process_ID := P.No_Process;
      Channel : CuBit.Channels.Channel;
      Contract : CuBit.Channel_Contracts.Contract;
      Ring : RC.Client;
      Region : Region_State := Unknown_Region;
      Own_Grant, Peer_Grant : GR.Reference;
   end record;
end ACPI_Host_Bridge_Channel;
