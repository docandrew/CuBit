pragma Ada_2022;
with GPIO_Service_Loop;
generic
   with package Registry is new GPIO_Service_Loop (<>);
   with procedure Hold_Requests (Hold : Boolean);
   with function Requests_Held return Boolean;
package GPIO_IRQ_Sweep with SPARK_Mode is
   package R renames Registry;
   type Phase is (Idle, Await_Controls, Await_Pins, Source_Clear, Uncertain);
   type State is private;
   procedure Start (S : in out State; Deadline : R.I.Tick; Accepted : out Boolean);
   procedure Advance (S : in out State);
   procedure Finish (S : in out State; Parent_ACK_Accepted : Boolean);
   function Current (S : State) return Phase;
   -- Registry.Step remains the only MMIO advancer. This adapter inspects its
   -- actual controller/lease reports and requests exactly one Poll_All sweep.
   -- Native request consumption must stay suspended until Finish; Stop events
   -- and already accepted controls still advance through the existing pump.
   -- Event sink backpressure never prevents Source_Clear after mask/clear.
   -- Source_Clear is a prerequisite for parent ACK, not client-ticket rearm.
   -- The enclosing GPIO_IRQ_Turn supplies finite time and supervision bounds.
private
   type State is record
      At_Phase : Phase := Idle;
      Deadline : R.I.Tick := 0;
   end record;
end GPIO_IRQ_Sweep;
