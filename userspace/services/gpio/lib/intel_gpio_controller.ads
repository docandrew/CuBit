pragma Ada_2022;
with GPIO_Controller_Resources;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with Intel_GPIO_IRQ_Owner;
generic
   Capacity : GPIO_Controller_Resources.Pin_Limit;
   with procedure Clock (Now : out Intel_GPIO_IRQ.Tick; Available : out Boolean);
   with procedure Read_Register
     (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Intel_GPIO_IRQ.Read_ID;
      Value : out Intel_GPIO_IRQ.Word; Status : out Intel_GPIO_IRQ.Access_Status);
   with procedure Write_Register
     (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Intel_GPIO_IRQ.Write_ID;
      Value : Intel_GPIO_IRQ.Word; Status : out Intel_GPIO_IRQ.Access_Status);
package Intel_GPIO_Controller with SPARK_Mode is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   type Pin_ID is new GPIO_Controller_Resources.Pin_Count;
   No_Pin : constant Pin_ID := 0;
   procedure Dispatched_Read
     (ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status);
   procedure Dispatched_Write
     (ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status);
   package Engine is new Intel_GPIO_IRQ_Owner (Clock, Dispatched_Read, Dispatched_Write);
   -- Internal engine adapter: direct reads/writes refuse outside Advance's
   -- dispatch context. These declarations are not a client wire/capability API.
   type Admission is (Accepted, Busy, Controller_Faulted, Invalid_Plan,
      Duplicate_Pin, Capacity_Refused, Unknown_Pin, Owner_Refused);
   type Summary is record
      Registered : GPIO_Controller_Resources.Pin_Count := 0;
      Active, Last_Fault : Pin_ID := No_Pin;
      Faulted : Boolean := False;
   end record;
   type Pin_Observation (Present : Boolean := False) is record
      case Present is
         when True =>
            Plan : G.Plan_Result (G.Ready);
            Mode : I.Trigger;
            Report : Engine.Report;
         when False => null;
      end case;
   end record;
   function Observe return Summary;
   function Observe_Pin (Pin : Pin_ID) return Pin_Observation;
   procedure Register_Pin
     (Plan : G.Plan_Result; Mode : I.Trigger; Deadline : I.Tick; Budget : I.Limit;
      Pin : out Pin_ID; Result : out Admission; Owner_Result : out Engine.Admission);
   procedure Register_Parked
     (Plan : G.Plan_Result; Mode : I.Trigger; Deadline : I.Tick; Budget : I.Limit;
      Pin : out Pin_ID; Result : out Admission; Owner_Result : out Engine.Admission);
   -- Startup registration that never enables this pin; successful completion
   -- is Held_Masked. Parent IRQ must independently stay disabled throughout
   -- setup. Resume is only used later by an authenticated client lease.
   function Released (Pin : Pin_ID) return Boolean;
   procedure Release_Parked (Pin : Pin_ID; Result : out Admission);
   procedure Retarget_Parked
     (Pin : Pin_ID; Plan : G.Plan_Result; Mode : I.Trigger;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission; Owner_Result : out Engine.Admission);
   -- Registry-only handoff: detach the quiescent channel/lease before Release.
   -- Release performs no IO and retains the confirmed-masked cached plan.
   -- Retarget requires Released, rejects duplicates of reserved pads, and keeps
   -- per-slot clock/event generations. Refusal leaves the old released slot.
   -- Registered is the bounded slot high-water count, not a live-client count.
   procedure Poll
     (Pin : Pin_ID; Deadline : I.Tick; Budget : I.Limit;
      Result : out Admission; Owner_Result : out Engine.Admission);
   procedure Pause (Pin : Pin_ID; Deadline : I.Tick; Budget : I.Limit;
      Result : out Admission; Owner_Result : out Engine.Admission);
   procedure Resume (Pin : Pin_ID; Deadline : I.Tick; Budget : I.Limit;
      Result : out Admission; Owner_Result : out Engine.Admission);
   procedure Retire_Event (Pin : Pin_ID; Event : I.Ticket;
      Result : out Admission; Owner_Result : out Engine.Admission);
   procedure Rearm
     (Pin : Pin_ID; Event : I.Ticket; Deadline : I.Tick; Budget : I.Limit;
      Result : out Admission; Owner_Result : out Engine.Admission);
   procedure Advance (Worked : out Boolean);
   -- One controller per package instance, one serialized event-loop caller.
   -- A logical operation lease spans every engine step until Listening or
   -- Event_Ready. RMW read/write cannot interleave across pins. A child fault
   -- freezes the whole controller for supervision, without implicit cleanup IO.
   -- Released slots can be retargeted only through the explicit registry-only
   -- handoff above. IDs are NOT authority: native caller authenticates pin
   -- capability, exact owner/badge, detached lease and fresh global lease epoch.
   -- Plans/callbacks are trusted bound to this admitted controller. Native
   -- backend must support the selected resource/plan; single-pin native window
   -- is not yet a multi-pin backend. Callbacks nonraising; no concurrent threads.
end Intel_GPIO_Controller;
