pragma Ada_2022;
with GPIO_Pin_Leases;
with GPIO_Control_Wire;
generic
   with package Leases is new GPIO_Pin_Leases (<>);
   Operation_Budget : Leases.I.Limit;
   type Reply_Authority is private;
   type Event_Sink is private;
   with procedure Answer (Authority : Reply_Authority;
      Value : GPIO_Control_Wire.Completion; Delivered : out Boolean);
   with procedure Try_Event (Sink : Event_Sink;
      Value : GPIO_Control_Wire.Event; Accepted : out Boolean);
package GPIO_Channel_Owner with SPARK_Mode is
   package W renames GPIO_Control_Wire;
   package P renames Leases.P;
   package C renames Leases.C;
   package I renames Leases.I;
   type Admission is (Accepted, Not_Ready, Unauthorized, Busy, Reply_Lost);
   type Binding_Phase is (Unconfigured, Binding, Ready, Failed);
   type Lifecycle is (Running, Draining, Stopped, Unresolved);
   type Report is record
      Binding : Binding_Phase := Unconfigured;
      Life : Lifecycle := Running;
      Control_Pending, Completion_Ready, Answer_Lost : Boolean := False;
      Last_Completion : W.Completion := (W.Arm, W.Refused, 0, 0);
      Pin : Leases.Report;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   function Quiescent (S : State) return Boolean;
   function Needs_Step (S : State) return Boolean;
   function Polling_Needed (S : State) return Boolean;
   procedure Configure (S : in out State; Pin : C.Pin_ID; Owner : P.Process_ID;
      Sink : Event_Sink; Deadline : I.Tick; Accepted : out Boolean);
   procedure Offer (S : in out State; Caller : P.Process_ID;
      Item : W.Request; Reply : Reply_Authority; Result : out Admission);
   procedure Poll (S : in out State; Deadline : I.Tick; Accepted : out Boolean);
   procedure Request_Stop (S : in out State; Cleanup_Deadline : I.Tick);
   procedure Detach (S : in out State; Accepted : out Boolean);
   -- Trusted registry cleanup AFTER separately closing the admitted sink.
   -- Requires Stopped plus confirmed quiescence; releases the lease binding
   -- and resets the channel slot for a new admitted process. No endpoint is
   -- closed here and no lease allocator/clock continuity is reset. Old private
   -- reply/sink values are inactive and replaced before a later use.
   procedure Step (S : in out State);
   -- One owner instance per lease/controller instance; one State per admitted
   -- channel. Configure is trusted startup glue AFTER kernel badge admission,
   -- using a registered pin and launcher-installed destination-limited sink.
   -- Payload IDs never select a channel or mint authority. Native dispatch must
   -- authenticate inbox class, badge, sender and frame before typed Offer.
   -- Accepted transfers one SAVED reply authority to this State. Other results
   -- leave that authority with the native dispatcher to answer immediately.
   -- Answer is bounded/nonblocking and CONSUMES authority on either outcome,
   -- matching IPC.Answer. Failure freezes admission; it is never retried.
   -- A failed answer may leave an armed lease. Observe retains the obligation;
   -- no implicit hardware cleanup/quiescence is claimed. Supervisor calls
   -- Request_Stop with its explicit cleanup deadline. First stop is final:
   -- later calls cannot extend the deadline or repeat a refused release.
   -- Draining finishes any owned control reply, suppresses event delivery,
   -- then releases a known lease once. Stopped requires actual parked/masked
   -- lease quiescence; uncertain effects or refused cleanup stay Unresolved.
   -- A retained event may be retired locally while already masked, including
   -- after clock loss/expiry, under the lease layer's exact-ticket contract.
   -- Try_Event is bounded/nonblocking. False means no accepted delivery; retain
   -- the same masked event. True transfers it to the installed sink exactly
   -- once, enabling a matching lease+generation ACK. Never Call a client.
   -- Step advances ONLY this channel. Outer loop advances Controller once per
   -- turn and fairly steps/polls its channels. Callbacks nonraising/nonreentrant.
private
   type State is limited record
      Pin_State : Leases.State;
      Owner : P.Process_ID := P.No_Process;
      Sink : Event_Sink;
      Reply : Reply_Authority;
      Request : W.Request;
      Binding : Binding_Phase := Unconfigured;
      Life : Lifecycle := Running;
      Pending, Ready_Reply, Lost : Boolean := False;
      Cleanup_Started : Boolean := False;
      Cleanup_Lease : W.Word := 0;
      Cleanup_Deadline : I.Tick := 0;
      Completion : W.Completion := (W.Arm, W.Refused, 0, 0);
   end record;
end GPIO_Channel_Owner;
