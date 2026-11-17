pragma Ada_2022;
with ACPI_Event_Waits;
with ACPI_Delay_Waits; with ACPI_Native_Startup_Timing;
with ACPI_Clock_Source;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with ACPI_Delay_Domains;
package body ACPI_Native_Delays is
   package K renames CuBit.Kernel_ABI;
   use type Interfaces.Unsigned_64;
   use type AML_Delays.Delay_Kind;
   Maximum_Sleep_Slice_Microseconds : constant Positive := 1_000;
   Sleep_Observation_Budget : constant := 4_096;
   Stall_Observation_Budget : constant := 65_536;
   Domain : ACPI_Delay_Domains.State;
   procedure Sleep_Until (Target : Interfaces.Unsigned_64; Accepted : out Boolean) is
      Yielded, Slept : Interfaces.Unsigned_64;
   begin
      -- Sleep(0) also relinquishes the processor. The second syscall may see
      -- an already-due target; neither syscall return alone proves elapsed time.
      Yielded := CuBit.Kernel_Calls.Call (K.Yield);
      if Yielded /= 0 then Accepted := False; return; end if;
      Slept := CuBit.Kernel_Calls.Call (K.Sleep_Until_Monotonic_Microsecond, Target);
      Accepted := Slept = 0;
   end Sleep_Until;
   package Waits is new ACPI_Delay_Waits
     (ACPI_Clock_Source.Read, Sleep_Until, Maximum_Sleep_Slice_Microseconds);
   use type Waits.Status;
   procedure Begin_Startup (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean) is
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      ACPI_Clock_Source.Read (Now, Available);
      ACPI_Delay_Domains.Start (Domain, Now, Deadline, Available, Accepted);
   end Begin_Startup;
   procedure Pause_Startup (Accepted : out Boolean) is
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      ACPI_Clock_Source.Read (Now, Available);
      ACPI_Delay_Domains.Pause_Startup (Domain, Now, Available, Accepted);
   end Pause_Startup;
   procedure Resume_Startup (Accepted : out Boolean) is
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      ACPI_Clock_Source.Read (Now, Available);
      ACPI_Delay_Domains.Resume_Startup (Domain, Now, Available, Accepted);
   end Resume_Startup;
   procedure Inspect_Paused_Startup
      (Deadline : out Interfaces.Unsigned_64; Accepted : out Boolean) is
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      ACPI_Clock_Source.Read(Now,Available);
      ACPI_Delay_Domains.Inspect_Paused_Startup(Domain,Now,Available,Deadline,Accepted);
   end Inspect_Paused_Startup;
   procedure Enter_Runtime (Accepted : out Boolean) is
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      ACPI_Clock_Source.Read (Now, Available);
      ACPI_Delay_Domains.Enter_Runtime (Domain, Now, Available, Accepted);
   end Enter_Runtime;
   procedure Begin_Event (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean) is
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      ACPI_Clock_Source.Read (Now, Available);
      ACPI_Delay_Domains.Begin_Event (Domain, Now, Deadline, Available, Accepted);
   end Begin_Event;
   procedure End_Event (Accepted : out Boolean) is
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      ACPI_Clock_Source.Read (Now, Available);
      ACPI_Delay_Domains.End_Event (Domain, Now, Available, Accepted);
   end End_Event;
   procedure Inspect_Window (Deadline : out Interfaces.Unsigned_64; Accepted : out Boolean) is
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      ACPI_Clock_Source.Read (Now, Available);
      ACPI_Delay_Domains.Inspect_Window (Domain, Now, Available, Deadline, Accepted);
   end Inspect_Window;
   procedure Close_Startup is
   begin
      ACPI_Delay_Domains.Stop (Domain);
   end Close_Startup;
   procedure Perform (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      Observation : Waits.Report;
      Timing : ACPI_Native_Startup_Timing.Token;
      Until_Time : Interfaces.Unsigned_64;
      Accepted : Boolean;
      procedure Execute is
      begin
      Result := AML_Delays.Unavailable;
      if ACPI_Delay_Domains.Working (Domain) then
         ACPI_Delay_Domains.Stop (Domain); Result := AML_Delays.Failed; return;
      end if;
      ACPI_Delay_Domains.Begin_Wait (Domain, Until_Time, Accepted);
      if not Accepted then return; end if;
      Waits.Run (Item, Until_Time,
        (if Item.Kind = AML_Delays.Sleep_Delay then Sleep_Observation_Budget
         else Stall_Observation_Budget), Observation);
      ACPI_Delay_Domains.End_Wait
        (Domain, Observation.Began, Observation.Last, Observation.Code = Waits.Completed, Accepted);
      if Accepted then Result := AML_Delays.Completed;
      else
         Result := (if Observation.Code = Waits.Clock_Unavailable then AML_Delays.Unavailable else AML_Delays.Failed);
         -- A failed native wait closes the startup delay domain. Recovery must
         -- not replay/replenish waits or continue with a fresh time allowance.
         ACPI_Delay_Domains.Stop (Domain);
      end if;
      end Execute;
   begin
      ACPI_Native_Startup_Timing.Enter
        ((if Item.Kind = AML_Delays.Sleep_Delay then ACPI_Native_Startup_Timing.P.AML_Sleep
          else ACPI_Native_Startup_Timing.P.AML_Stall),Timing);
      Execute;
      ACPI_Native_Startup_Timing.Leave (Timing);
   end Perform;
   package Event_Waits is new ACPI_Event_Waits (Perform);
   procedure Wait_Event_Timeout
     (Milliseconds : AML_Events.Timeout; Outcome : out AML_Events.Timeout_Outcome) is
      Observation : Event_Waits.Report;
   begin
      Event_Waits.Run (Milliseconds, Observation);
      Outcome := Observation.Code;
   end Wait_Event_Timeout;
end ACPI_Native_Delays;
