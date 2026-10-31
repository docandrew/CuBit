pragma Ada_2022;
with ACPI_Delay_Waits;
with ACPI_Clock_Source;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
package body ACPI_Native_Delays is
   package K renames CuBit.Kernel_ABI;
   use type Interfaces.Unsigned_64;
   use type AML_Delays.Delay_Kind;
   Maximum_Sleep_Slice_Microseconds : constant Positive := 1_000;
   Sleep_Observation_Budget : constant := 4_096;
   Stall_Observation_Budget : constant := 65_536;
   type Phase is (Unused, Open, Closed);
   State : Phase := Unused;
   Until_Time : Interfaces.Unsigned_64 := 0;
   In_Call : Boolean := False;
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
   begin
      Accepted := State = Unused and not In_Call;
      if Accepted then State := Open; Until_Time := Deadline; end if;
   end Begin_Startup;
   procedure Close_Startup is
   begin
      State := Closed;
   end Close_Startup;
   procedure Perform (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      Observation : Waits.Report;
   begin
      Result := AML_Delays.Unavailable;
      if In_Call then State := Closed; Result := AML_Delays.Failed; return; end if;
      if State /= Open then return; end if;
      In_Call := True;
      Waits.Run (Item, Until_Time,
        (if Item.Kind = AML_Delays.Sleep_Delay then Sleep_Observation_Budget
         else Stall_Observation_Budget), Observation);
      In_Call := False;
      if State /= Open then Result := AML_Delays.Failed;
      elsif Observation.Code = Waits.Completed then Result := AML_Delays.Completed;
      else
         Result := (if Observation.Code = Waits.Clock_Unavailable then AML_Delays.Unavailable else AML_Delays.Failed);
         -- A failed native wait closes the startup delay domain. Recovery must
         -- not replay/replenish waits or continue with a fresh time allowance.
         State := Closed;
      end if;
   end Perform;
end ACPI_Native_Delays;
