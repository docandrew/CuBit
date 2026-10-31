pragma Ada_2022;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
generic
   with procedure Clock (Now : out Intel_GPIO_IRQ.Tick; Available : out Boolean);
   with procedure Read
     (ID : Intel_GPIO_IRQ.Read_ID; Value : out Intel_GPIO_IRQ.Word;
      Status : out Intel_GPIO_IRQ.Access_Status);
   with procedure Write
     (ID : Intel_GPIO_IRQ.Write_ID; Value : Intel_GPIO_IRQ.Word;
      Status : out Intel_GPIO_IRQ.Access_Status);
package Intel_GPIO_IRQ_Owner with SPARK_Mode is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   use type I.Ticket;
   type Phase is (Cold, Read_Owner, Read_Host, Read_Lock, Read_Config,
      Read_Mask, Mask_Write, Mask_Confirm, Config_Write, Config_Confirm,
      Clear_Stale, Enable_Read, Enable_Write, Enable_Confirm, Listening,
      Park_Mask_Read, Park_Mask_Write, Park_Mask_Confirm, Held_Masked,
      Poll_Status, Event_Mask_Read, Event_Mask_Write, Event_Mask_Confirm,
      Acknowledge, Event_Ready, Faulted);
   type Failure is (None, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Budget_Exhausted, Register_Refused, Register_Indeterminate,
      Ownership_Refused, Readback_Refused, Tickets_Exhausted);
   type Admission is (Accepted, Busy, Invalid_Plan, Clock_Refused, Expired, Wrong_Ticket);
   type Report is record
      At_Phase : Phase := Cold;
      Failed_At : Phase := Cold;
      Reason : Failure := None;
      Event_Pending : Boolean := False;
      Event : I.Ticket := I.No_Ticket;
      Last_Read : I.Read_ID := I.Pad_Owner;
      Last_Write : I.Write_ID := I.Write_Enable;
      Last_Value : I.Word := 0;
      Last_Status : I.Access_Status := I.Access_OK;
      Was_Write, Access_Attempted, Effects_Possible : Boolean := False;
      Pin_May_Be_Enabled : Boolean := True;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   procedure Start
     (S : in out State; Plan : G.Plan_Result; Mode : I.Trigger;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission);
   procedure Start_Parked
     (S : in out State; Plan : G.Plan_Result; Mode : I.Trigger;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission);
   -- Same ownership/configuration/stale-status checks as Start, ending with
   -- confirmed mask still held. Never executes an enable write. Resume arms
   -- later without clearing edges latched after this initialization.
   procedure Retarget_Parked
     (S : in out State; Plan : G.Plan_Result; Mode : I.Trigger;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission)
     with Post => Observe (S).Event = Observe (S)'Old.Event and then
       (if Result = Accepted then Observe (S).At_Phase = Read_Owner and then not Observe (S).Event_Pending);
   -- Internal slot reuse only after its prior client/lease has detached. Requires
   -- confirmed Held_Masked and no retained event; preserves clock continuity
   -- and event-ticket high water. Starts complete parked ownership/config checks.
   procedure Poll
     (S : in out State; Deadline : I.Tick; Budget : I.Limit; Result : out Admission);
   procedure Pause (S : in out State; Deadline : I.Tick; Budget : I.Limit; Result : out Admission);
   procedure Resume (S : in out State; Deadline : I.Tick; Budget : I.Limit; Result : out Admission);
   -- Pause masks only the selected pin with readback, retaining latched status.
   -- Resume validates ownership then enables without clearing pending status.
   procedure Retire_Event (S : in out State; Event : I.Ticket; Result : out Admission);
   -- Explicitly retire this exact masked event without re-enabling the pin.
   -- Pure local cleanup: no register IO, status clear or new ticket allocation.
   procedure Rearm
     (S : in out State; Event : I.Ticket;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission);
   procedure Advance (S : in out State);
   -- One already-authorized, exclusively owned pin in one admitted window.
   -- All community RMW operations MUST be serialized by the enclosing owner
   -- across steps; no other pin owner/firmware may change those registers.
   -- Callbacks are bounded, nonraising/nonreentrant and use only typed offsets.
   -- No peer calls, waiting, delivery or IRQ capability authority is created.
   -- Event stays masked/retained until exact-ticket Rearm. Native owner sends
   -- authenticated async events and invokes Poll on IRQ/poll wakeups. Rearm
   -- never clears newly latched status, preserving edges arriving while masked.
   -- Failures latch terminal diagnostics; no implicit retry or cleanup IO.
private
   function Valid_State (S : State) return Boolean with Ghost;
   type State is limited record
      Info : Report;
      Plan : G.Plan_Result;
      Mode : I.Trigger := I.Level_High;
      Owner, Host, Desired_Config, Desired_Enable : I.Word := 0;
      Deadline, Previous : I.Tick := 0;
      Remaining : I.Count := 0;
      Rearming, Pausing, Initial_Masked : Boolean := False;
   end record with Type_Invariant => Valid_State (State);
   function Valid_State (S : State) return Boolean is
     (if S.Info.At_Phase in Cold | Faulted then True
      else (case S.Plan.Status is when G.Ready => True, when others => False));
end Intel_GPIO_IRQ_Owner;
