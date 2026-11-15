pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Protocol;
generic
   type Lease is limited private;
   with function Admission_Open return Boolean;
   with function Needs_Cleanup (Item : Lease) return Boolean;
   with procedure Begin_Window (Deadline_Microseconds : out Interfaces.Unsigned_64;
      Budget : out ACPI_Global_Lock_Protocol.Attempt_Budget; Available : out Boolean);
   with procedure Acquire (Item : in out Lease; Deadline_Microseconds : Interfaces.Unsigned_64;
      Budget : ACPI_Global_Lock_Protocol.Attempt_Budget;
      Result : out ACPI_Global_Lock_Protocol.Lock_Outcome);
   with procedure Check_Access (Item : Lease; Result : out ACPI_Global_Lock_Protocol.Lock_Outcome);
   with procedure Release (Item : in out Lease; Result : out ACPI_Global_Lock_Protocol.Lock_Outcome);
   Maximum_Depth : Positive;
   with procedure Inspect_IO_Window (Deadline_Microseconds : out Interfaces.Unsigned_64;
      Available : out Boolean);
-- One retained whole-Field scope over the SAME local physical lock owner.
-- No transport, new physical lock, ticket decoding or deadline extension.
-- Inspect_IO_Window observes the trusted owner's existing finite microsecond
-- window, including for unlocked effects. It never opens/replenishes one and
-- uses the same clock epoch as Begin_Window. Callbacks are bounded/nonreentrant.
package ACPI_Local_Field_Scope with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package P renames ACPI_Global_Lock_Protocol;
   use type Interfaces.Unsigned_64, P.Lock_Outcome;
   type Depth_Count is new Natural range 0 .. Maximum_Depth;
   type Statistics is record
      Depth : Depth_Count := 0;
      Working, Faulted, Held, Cleanup_Required : Boolean := False;
      First_Failure : P.Lock_Outcome := P.Configured;
   end record;
   type Context is limited private;
   function Observe (State : Context) return Statistics with Global => null;
   function Ready (State : Context) return Boolean with Global => null;
   function Pending (State : Context) return Boolean with Global => null;
   function Captured_Window (State : Context) return Interfaces.Unsigned_64 with Ghost, Global => null;
   function Fault_Stays (Before, After : Statistics) return Boolean is
     (if Before.Faulted then After.Faulted and then After.First_Failure = Before.First_Failure) with Ghost;
   function Entry_Transition (Before, After : Statistics;
      Old_Window, New_Window : Interfaces.Unsigned_64; Accepted : Boolean) return Boolean is
     (After.Depth = (if Accepted then Before.Depth + 1 else Before.Depth)
      and then (if Accepted then not After.Working and then not After.Faulted
        and then New_Window not in 0 | Interfaces.Unsigned_64'Last)
      and then (if Before.Depth > 0 then New_Window = Old_Window)
      and then Fault_Stays (Before,After)) with Ghost;
   function Datum_Transition (Before, After : Statistics;
      Old_Window, New_Window : Interfaces.Unsigned_64; Accepted : Boolean) return Boolean is
     (After.Depth = Before.Depth and then New_Window = Old_Window
      and then After.Held = Before.Held and then After.Cleanup_Required = Before.Cleanup_Required
      and then (if Accepted then After.Depth > 0 and then not After.Working and then not After.Faulted
        and then New_Window not in 0 | Interfaces.Unsigned_64'Last)
      and then Fault_Stays (Before,After)) with Ghost;
   function End_Transition (Before, After : Statistics;
      Old_Window, New_Window : Interfaces.Unsigned_64; Accepted : Boolean) return Boolean is
     (After.Depth = (if Before.Working or else Before.Depth = 0 then Before.Depth else Before.Depth - 1)
      and then New_Window = (if not Before.Working and then Before.Depth = 1 then 0 else Old_Window)
      and then (if Accepted then not After.Working and then not After.Faulted)
      and then Fault_Stays (Before,After)) with Ghost;
   function Cleanup_Transition (Before, After : Statistics;
      Old_Window, New_Window : Interfaces.Unsigned_64; Completed : Boolean) return Boolean is
     (After.Depth = Before.Depth and then New_Window = Old_Window
      and then (if Before.Working then After = Before and then not Completed
        else After.Faulted and then not After.Working
          and then (if Before.Depth > 0 then not Completed
            and then After.Held = Before.Held and then After.Cleanup_Required = Before.Cleanup_Required))
      and then (if Completed then After.Depth = 0 and then not After.Working and then not After.Cleanup_Required)
      and then Fault_Stays (Before,After)) with Ghost;
   procedure Begin_Field (State : in out Context; Requires_Lock, For_Write : Boolean;
      Accepted : out Boolean) with Post =>
        Entry_Transition (Observe (State)'Old,Observe (State),Captured_Window (State)'Old,Captured_Window (State),Accepted);
   procedure Check_Datum (State : in out Context; Accepted : out Boolean) with Post =>
     Datum_Transition (Observe (State)'Old,Observe (State),Captured_Window (State)'Old,Captured_Window (State),Accepted);
   -- Every scalar callback must check this immediately before its effect.
   -- Target authority, write policy and exact geometry remain provider duties.
   procedure End_Field (State : in out Context; Accepted : out Boolean) with Post =>
     End_Transition (Observe (State)'Old,Observe (State),Captured_Window (State)'Old,Captured_Window (State),Accepted);
   -- End unwinds exactly one ACCEPTED Begin, including after a data failure.
   -- A rejected child Begin opens no frame and must not consume its parent.
   procedure Mark_Fault (State : in out Context);
   procedure Cleanup (State : in out Context; Completed : out Boolean) with Post =>
     Cleanup_Transition (Observe (State)'Old,Observe (State),Captured_Window (State)'Old,Captured_Window (State),Completed);
   -- Explicit stop/cleanup at depth zero. Never resumes or retries AML/data.
   -- Uncertain lock outcomes retain the original lease solely for maintenance.
private
   subtype Position is Depth_Count range 1 .. Depth_Count'Last;
   type Frames is array (Position) of Boolean;
   type Context is limited record
      Info : Statistics;
      Owners : Frames := [others => False];
      Item : Lease;
      Window_Deadline : Interfaces.Unsigned_64 := 0;
   end record;
   procedure Read_Window (State : Context; Deadline : out Interfaces.Unsigned_64;
      Available : out Boolean) with Post =>
        (if Available then Deadline not in 0 | Interfaces.Unsigned_64'Last
          and then (Observe (State).Depth = 0 or else Deadline = Captured_Window (State))
         else Deadline = 0);
end ACPI_Local_Field_Scope;
