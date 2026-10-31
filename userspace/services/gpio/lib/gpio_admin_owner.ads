pragma Ada_2022;
with CuBit.Process_IDs;
with GPIO_Admin_Wire;
with GPIO_Control_Wire;
with GPIO_Service_Loop;
generic
   type Reply_Authority is private;
   type Event_Sink is private;
   type Offer_Receipt is private;
   with package Registry is new GPIO_Service_Loop
     (Reply_Authority => Reply_Authority, Event_Sink => Event_Sink, others => <>);
   with function Authorized (Caller : CuBit.Process_IDs.Process_ID) return Boolean;
   with procedure Take_Offer (Caller : CuBit.Process_IDs.Process_ID;
      Receipt : out Offer_Receipt; Owned : out Boolean);
   with procedure Inspect (Receipt : Offer_Receipt; Sink : out Event_Sink;
      Owner : out CuBit.Process_IDs.Process_ID; Badge : out GPIO_Control_Wire.Word;
      Valid : out Boolean);
   with procedure Discard (Receipt : Offer_Receipt; Closed : out Boolean);
   with procedure Save (Reply : out Reply_Authority; Saved : out Boolean);
   with procedure Answer_Saved (Reply : Reply_Authority;
      Label : GPIO_Control_Wire.Operation; Status : GPIO_Control_Wire.Completion_Status;
      Delivered : out Boolean);
   with procedure Answer_Current (Caller : CuBit.Process_IDs.Process_ID;
      Label : GPIO_Control_Wire.Operation; Status : GPIO_Control_Wire.Completion_Status;
      Delivered : out Boolean);
package GPIO_Admin_Owner with SPARK_Mode is
   package A renames GPIO_Admin_Wire;
   package W renames GPIO_Control_Wire;
   package P renames CuBit.Process_IDs;
   package I renames Registry.I;
   type Phase is (Idle, Await_Admission, Await_Retirement, Drain_Lost_Admission, Quarantined);
   type Report is record
      At_Phase : Phase;
      Receipt_Owned, Reply_Owned, Stopping, Reply_Delivered : Boolean;
      Last_Status : W.Completion_Status;
      Binding_ID : Registry.Channel_ID;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   function Quiescent (S : State) return Boolean;
   procedure Handle (S : in out State; Caller : P.Process_ID;
      Label : W.Operation; Protocol : W.Word; Words : W.Message_Words;
      Frame_Valid, Has_Offer : Boolean; Setup_Deadline, Cleanup_Deadline : I.Tick);
   procedure Step (S : in out State);
   procedure Request_Stop (S : in out State);
   -- Only authentic service inbox entries. Authorized matches kernel sender
   -- against Describe of typed launcher Process authority; never payload/badge.
   -- Take installs at most one capability and returns an opaque owned receipt.
   -- Inspect must return only kernel Describe-derived sink/owner/badge after
   -- checking endpoint kind and exact Write-only rights. Invalid receipts still
   -- require Discard. False Discard retains the receipt and freezes acceptance.
   -- Take/check precedes Save (offers belong to current reply). Failed Save
   -- discards unused offer and never enrolls. Accepted enrollment transfers sink
   -- ownership to Registry. One saved admin reply remains until confirmed result.
   -- Saved answer consumes authority on either outcome. A lost successful
   -- admission reply starts one bounded retirement; no replay. Retirement reply
   -- is attempted before acknowledging Registry's retained completion/reusing slot.
   -- Step only observes Registry, never advances it. Outer loop steps Registry
   -- once then this owner. Quiescent covers admin ownership, not live channels.
   -- All callbacks bounded/nonraising/nonreentrant. Deadlines supplied explicitly.
private
   type State is limited record
      At_Phase : Phase := Idle;
      Receipt : Offer_Receipt;
      Reply : Reply_Authority;
      Receipt_Owned, Reply_Owned, Stopping, Delivered : Boolean := False;
      Last_Status : W.Completion_Status := W.Refused;
      Label : W.Operation := A.OP_GPIO_ADMIT_SINK;
      ID : Registry.Channel_ID := Registry.No_Channel;
      Owner : P.Process_ID := P.No_Process;
      Badge : W.Word := 0;
      Cleanup_Deadline : I.Tick := 0;
   end record;
end GPIO_Admin_Owner;
