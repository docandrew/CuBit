pragma Ada_2022;
with CuBit.Process_IDs;
with Intel_GPIO_Controller;
with GPIO_Lease_Control;
generic
   with package Controller is new Intel_GPIO_Controller (<>);
   with procedure Clock (Now : out Controller.I.Tick; Available : out Boolean);
   with procedure Next_Lease (Lease : out GPIO_Lease_Control.Lease_ID; Available : out Boolean);
package GPIO_Pin_Leases with SPARK_Mode is
   package C renames Controller;
   package I renames C.I;
   package L renames GPIO_Lease_Control;
   package P renames CuBit.Process_IDs;
   type Phase is (Unbound, Binding, Parked, Arming, Armed, Polling,
      Event_Retained, Event_Delivered, Acknowledging, Releasing, Faulted);
   type Failure is (None, Invalid_Binding, Clock_Unavailable, Clock_Reversed,
      Expired, Lease_Unavailable, Controller_Refused, Controller_Uncertain, Invalid_Event);
   type Event is record
      Lease : L.Lease_ID := L.No_Lease;
      Ticket : I.Ticket := I.No_Ticket;
      Pin : C.Pin_ID := C.No_Pin;
   end record;
   type Report is record
      At_Phase : Phase := Unbound;
      Reason : Failure := None;
      Pin : C.Pin_ID := C.No_Pin;
      Lease : L.Lease_ID := L.No_Lease;
      Current_Event : Event;
      Had_Event_At_Release : Boolean := False;
      Last_Controller : C.Admission := C.Accepted;
      Last_Engine : C.Engine.Admission := C.Engine.Accepted;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   function Has_Reply (S : State) return Boolean;
   procedure Take_Reply (S : in out State; Reply : out L.Reply) with Pre => Has_Reply (S);
   function Has_Event (S : State) return Boolean;
   function Head (S : State) return Event with Pre => Has_Event (S);
   procedure Delivered (S : in out State) with Pre => Has_Event (S);
   function Quiescent (S : State) return Boolean;
   procedure Bind (S : in out State; Pin : C.Pin_ID; Owner : P.Process_ID;
      Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean);
   procedure Unbind (S : in out State; Accepted : out Boolean);
   -- Internal owner operation only after confirmed Quiescent, no outstanding
   -- reply/event and no lease. Releases the pin reservation for later binding.
   -- Global lease allocation and per-state clock continuity are NOT reset.
   -- Binding a confirmed Held_Masked pin is local metadata only; no MMIO or
   -- clock is needed. It still retains one internal completion to consume.
   procedure Arm (S : in out State; Caller : P.Process_ID;
      Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean);
   procedure Release (S : in out State; Caller : P.Process_ID; Lease : L.Lease_ID;
      Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean);
   procedure Acknowledge_Event (S : in out State; Caller : P.Process_ID;
      Lease : L.Lease_ID; Ticket : I.Ticket; Deadline : I.Tick; Budget : I.Limit;
      Accepted : out Boolean);
   procedure Poll (S : in out State; Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean);
   procedure Step (S : in out State);
   -- Instantiate once per Controller; reservations prevent duplicate bindings
   -- inside this owner and are retained after uncertain effects.
   -- Native owner authenticates the pin capability/badge BEFORE calling Bind.
   -- This layer binds that admitted pin to one opaque process lifetime; IDs are
   -- not authority. Step never advances Controller: the enclosing single event
   -- loop calls Controller.Advance once, then updates each lease with Step.
   -- One control reply retained until Take_Reply; one masked event retained until
   -- actual nonblocking delivery succeeds and Delivered is called. Client ACK
   -- needs both exact lease and event ticket. No peer waits or implicit retries.
private
   type Operation is (No_Operation, Bind_Operation, Arm_Operation, Poll_Operation,
      Ack_Operation, Release_Operation);
   type State is limited record
      Info : Report;
      Owner : P.Process_ID := P.No_Process;
      Op : Operation := No_Operation;
      Before : Phase := Unbound;
      Started, Reply_Owned, Reply_Ready : Boolean := False;
      Reply : L.Reply;
      Deadline, Previous : I.Tick := 0;
      Budget : I.Limit := I.Limit'First;
   end record;
   function Observe (S : State) return Report is (S.Info);
   function Has_Reply (S : State) return Boolean is (S.Reply_Owned and then S.Reply_Ready);
   function Has_Event (S : State) return Boolean is (S.Info.At_Phase = Event_Retained);
   function Head (S : State) return Event is (S.Info.Current_Event);
end GPIO_Pin_Leases;
