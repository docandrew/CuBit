pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Power_Requests;
with CuBit.Power_Event_Exchange;

--  power.svc's decisions (PWR-003 slice 1), without I/O: what a request is
--  answered, what a button press asks for, and which step follows a stop.
package Power_Decisions with SPARK_Mode is
   package PR renames CuBit.Power_Requests;
   package PE renames CuBit.Power_Event_Exchange;
   use type PR.Requester, PR.Answer, PR.Action, PE.Watch_Status;

   --  Where power.svc is: one stop at most, ever.
   type Phase is
     (Idle,       --  serving requests and the button
      Stopping,   --  procmgr's root stop runs
      Final,      --  the firmware step runs
      Halted);    --  everything is stopped; safe to switch off

   --  What a press does (its typed startup parameter).
   type Button_Policy is (Power_Off, Halt, Ignore);

   --  The answer to a request: Requester from its badge, Ready when
   --  power.svc can ask procmgr (it registered its role).
   function Answer_For
     (State : Phase; Requester : PR.Requester; Ready : Boolean;
      Length : Unsigned_64; Id, Action_Word : Unsigned_64) return PR.Answer
   is (if Requester not in PR.Desktop | PR.Console then PR.Not_Approved
       elsif Length /= PR.Request_Words or else Id = 0 or else not PR.Valid_Action (Action_Word)
       then PR.Malformed
       elsif State /= Idle then PR.Busy
       elsif not Ready then PR.Not_Ready
       else PR.Accepted)
   with Post => (if Answer_For'Result = PR.Accepted then
                   State = Idle and then Ready and then Requester in PR.Desktop | PR.Console
                   and then PR.Valid_Action (Action_Word));

   --  A press while Idle: the stop it asks for, if any.
   function Press_Starts (State : Phase; Policy : Button_Policy) return Boolean is
     (State = Idle and then Policy /= Ignore);
   function Press_Action (Policy : Button_Policy) return PR.Action is
     (if Policy = Halt then PR.Halt else PR.Power_Off)
   with Pre => Policy /= Ignore;

   --  What a watch reply means for the next watch: Seen is the count to
   --  watch from, Again whether to watch again.
   procedure After_Watch
     (Status : PE.Watch_Status; Count : Unsigned_64; Seen : in out Unsigned_64; Again : out Boolean;
      Pressed : out Boolean)
   with Post => (if Pressed then Status = PE.Pressed and then Count > Seen'Old)
                and then (if Again then Status in PE.Pressed | PE.Superseded | PE.Counter_Reset)
                and then (if Status in PE.Pressed | PE.Counter_Reset then Seen = Count);

   --  A watch to submit again after PE.Watch_Retry_Milliseconds.
   function Retry_Later (Status : PE.Watch_Status) return Boolean is (Status = PE.Watch_Unavailable);

   --  After procmgr's stop: the firmware step when power-off was asked and
   --  acpi.svc's power endpoint is there; else halted.
   function Needs_Firmware (Action : PR.Action; Firmware_Endpoint : Boolean) return Boolean is
     (Action = PR.Power_Off and then Firmware_Endpoint);
end Power_Decisions;
