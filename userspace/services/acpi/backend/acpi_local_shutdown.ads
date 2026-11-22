pragma Ada_2022;
with Interfaces;
generic
   with procedure Close_Consumers (Accepted : out Boolean);
   with procedure Close_Local;
   with procedure Begin_Hardware_Stop;
   with procedure Maintain_Hardware (Wake_Deadline_Milliseconds : out Interfaces.Unsigned_64;
      May_Continue : out Boolean);
   with procedure Cleanup_Fields (Completed : out Boolean);
   with function Hardware_May_Exit return Boolean;
   with procedure Other_Owners_View (Quiescent : out Boolean;
      Wake_Deadline_Milliseconds : out Interfaces.Unsigned_64);
-- Serialized root shutdown. Actuals are bounded, normally returning and must
-- retain backing; Other_Owners_View reads observations only, without AML/IPC.
-- Every deadline is already in the kernel wait clock's millisecond domain.
package ACPI_Local_Shutdown with SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   No_Deadline : constant Word := Word'Last;
   type Phase is (Open, Closing, Draining, Waiting, Retained, Quiescent, Refused);
   type Report is record
      Stage : Phase := Open;
      Working : Boolean := False;
      Consumers_Closed, Local_Closed, Hardware_Closing : Boolean := False;
      Fields_Cleaned, Hardware_Idle, Others_Idle : Boolean := False;
      Hardware_Continues : Boolean := False;
      Wake_Deadline_Milliseconds : Word := No_Deadline;
   end record;
   type Context is limited private;
   function Retirement_Ready (View : Report) return Boolean is
     (View.Stage = Quiescent and then not View.Working
      and then View.Consumers_Closed and then View.Local_Closed
      and then View.Hardware_Closing and then View.Fields_Cleaned
      and then View.Hardware_Idle and then View.Others_Idle) with Ghost;
   function Stop_Transition (Before, After : Report; Accepted : Boolean) return Boolean is
     (if Before.Working or else Before.Stage /= Open then
        After = Before and then Accepted = (not Before.Working and then
          Before.Stage in Draining | Waiting | Retained | Quiescent)
      else not After.Working and then After.Stage in Draining | Refused
        and then Accepted = (After.Stage = Draining)
        and then not Retirement_Ready (After)
        and then (if Accepted then After.Consumers_Closed and then After.Local_Closed
          and then After.Hardware_Closing)) with Ghost;
   function Advance_Transition (Before, After : Report) return Boolean is
     (if Before.Working or else Before.Stage not in Draining | Waiting | Retained then
        After = Before
      else not After.Working and then After.Stage in Quiescent | Waiting | Retained
        and then After.Consumers_Closed = Before.Consumers_Closed
        and then After.Local_Closed = Before.Local_Closed
        and then After.Hardware_Closing = Before.Hardware_Closing
        and then (if Before.Fields_Cleaned then After.Fields_Cleaned)
        and then (After.Stage = Quiescent) =
          (After.Fields_Cleaned and then After.Hardware_Idle and then After.Others_Idle)
        and then (if After.Stage /= Quiescent then
          (After.Stage = Retained) =
            Interfaces."=" (After.Wake_Deadline_Milliseconds, No_Deadline))) with Ghost;
   function Observe (State : Context) return Report;
   function May_Retire (State : Context) return Boolean with
     Post => May_Retire'Result = Retirement_Ready (Observe (State));
   procedure Request_Stop (State : in out Context; Accepted : out Boolean) with
     Post => Stop_Transition (Observe (State)'Old, Observe (State), Accepted);
   -- Accepted means ingress closure completed, never safe exit. One attempt;
   -- refusal/exception/reentry cannot replay callbacks or reset this owner.
   procedure Advance (State : in out Context) with
     Post => Advance_Transition (Observe (State)'Old, Observe (State));
   -- One explicit maintenance/cleanup/observation turn. Completed Field
   -- cleanup is retained and never repeated. The hardware continue flag is
   -- not quiescence evidence: Hardware_May_Exit must independently agree.
   -- Other_Owners_View must include EC delivery, report/grant retirement,
   -- unaccepted capability carriage and every other retained root obligation.
private
   type Context is limited record
      Info : Report;
   end record;
end ACPI_Local_Shutdown;
