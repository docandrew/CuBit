pragma Ada_2022;
with GPIO_Channel_Owner;
with GPIO_Control_Wire;
with GPIO_Channel_Badges;
with GPIO_Controller_Resources;
with GPIO_Community_Types;
with Intel_ADLN_GPIO;
generic
   type Reply_Authority is private;
   type Event_Sink is private;
   with package Channels is new GPIO_Channel_Owner
     (Reply_Authority => Reply_Authority, Event_Sink => Event_Sink, others => <>);
   Capacity : GPIO_Controller_Resources.Pin_Limit;
   with procedure Close_Sink (Sink : Event_Sink; Closed : out Boolean);
   with function Startup_Parked return Boolean;
   with procedure Admit_Pin (Pin : Intel_ADLN_GPIO.ACPI_Pin;
      Result : out GPIO_Community_Types.Pin_Admission; Plan : out Intel_ADLN_GPIO.Plan_Result);
package GPIO_Service_Loop with SPARK_Mode is
   package C renames Channels.C;
   package I renames Channels.I;
   package P renames Channels.P;
   package W renames GPIO_Control_Wire;
   package B renames GPIO_Channel_Badges;
   type Channel_ID is new GPIO_Controller_Resources.Pin_Count;
   subtype Channel_Count is Channel_ID range 0 .. Channel_ID (Capacity);
   No_Channel : constant Channel_ID := 0;
   type Phase is (Free, Registering, Binding, Live, Retiring, Retired, Close_Failed, Unresolved);
   type Admission is (Accepted, Not_Ready, Invalid_Owner, Invalid_Badge,
      Unknown_Pin, Trigger_Mismatch, Already_Bound, Bind_Refused, Wrong_Binding, Busy);
   type Summary is record
      Registered, Bound, Retired, Unresolved : Channel_Count := 0;
      Sealed, Operational, Stopping, Layout_Changed : Boolean := False;
      Next_Channel : Channel_ID := No_Channel;
   end record;
   type Binding_Report is record
      At_Phase : Phase;
      Owner : P.Process_ID;
      Badge : W.Word;
      Channel : Channels.Report;
   end record;
   procedure Seal (Accepted : out Boolean);
   procedure Enroll (Owner : P.Process_ID; Badge : W.Word; Sink : Event_Sink;
      Setup_Deadline : I.Tick; ID : out Channel_ID; Result : out Admission);
   procedure Retire (Owner : P.Process_ID; Badge : W.Word; Cleanup_Deadline : I.Tick;
      ID : out Channel_ID; Result : out Admission);
   procedure Acknowledge_Retirement (ID : Channel_ID; Accepted : out Boolean);
   function Observe return Summary;
   function Observe_Channel (ID : Channel_ID) return Binding_Report
      with Pre => ID /= No_Channel and then ID <= Observe.Registered;
   function Authorized (Caller : P.Process_ID; Badge : W.Word) return Boolean;
   procedure Offer (Caller : P.Process_ID; Badge : W.Word; Item : W.Request;
      Reply : Reply_Authority; Taken : out Boolean);
   function Polling_Needed return Boolean;
   procedure Poll_All (Deadline : I.Tick);
   procedure Request_Stop (Cleanup_Deadline : I.Tick);
   procedure Step (Controller_Worked : out Boolean);
   function Terminal return Boolean;
   function Quiescent return Boolean;
   function Needs_Step return Boolean;
   -- Includes retained event delivery under backpressure. A native loop uses a
   -- bounded progress wake for this, never busy-spins or sleeps indefinitely.
   -- Terminal uncertainty needs supervision, not further controller stepping.
   -- Seal requires completed community parking and an empty controller. Runtime
   -- enrollment registers only the exact authenticated badge pin/mode. Trusted admin
   -- supplies Describe-derived owner/badge and accepted sink after authenticating
   -- launcher. Matching geometry never creates authority. One channel per pin.
   -- Full opaque owner+badge lookup; slots never PID-derived. Step advances
   -- controller once and one pin/channel round-robin. No waits or hidden periods.
   -- Retire stops routing, drains controls/lease, closes sink once, detaches,
   -- and releases the confirmed-masked controller slot before reuse.
   -- Retired retains its completion identity until Acknowledge_Retirement so
   -- admin can handle its saved reply before reuse. Failed close/uncertain effects
   -- retain slot/sink, never retry/free implicitly. External admin reply ownership
   -- is outside this core; Terminal covers channels, not pending admin replies.
private
   subtype Slot is Channel_ID range 1 .. Channel_ID (Capacity);
   type States_Array is array (Slot) of Channels.State;
   States : States_Array;
   Owners : array (Slot) of P.Process_ID := [others => P.No_Process];
   Badges : array (Slot) of W.Word := [others => 0];
   Sinks : array (Slot) of Event_Sink;
   Phases : array (Slot) of Phase := [others => Free];
   Stop_Pending : array (Slot) of Boolean := [others => False];
   Setup_Deadlines, Cleanup_Deadlines : array (Slot) of I.Tick := [others => 0];
   Count : Channel_Count := 0;
   Cursor : Slot := Slot'First;
   Is_Sealed, Stopping, Layout_Fault : Boolean := False;
end GPIO_Service_Loop;
