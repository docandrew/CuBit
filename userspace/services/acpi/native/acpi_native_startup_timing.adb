pragma Ada_2022;
with System.Machine_Code;
with Interfaces; with ACPI_Clock_Source; with ACPI_Native_Log;
package body ACPI_Native_Startup_Timing is
   use type Interfaces.Unsigned_64, P.Token;
   State : P.Context;
   Reported : Boolean := False;
   Maximum_Work_Depth : constant := 8;
   subtype Work_Depth is Natural range 0 .. Maximum_Work_Depth;
   subtype Work_Index is Work_Depth range 1 .. Maximum_Work_Depth;
   type Work_Frame is record
      Kind : AML_Work_Kinds.Kind := AML_Work_Kinds.Namespace_Lookup;
      Handle : Token := P.No_Token;
   end record;
   Frames : array (Work_Index) of Work_Frame;
   Depth : Work_Depth := 0;
   Overflow : Natural := 0;
   Dropped : Interfaces.Unsigned_64 := 0;
   Work_Broken : Boolean := False;
   use type AML_Work_Kinds.Kind;
   function Work_Kind (Item : AML_Work_Kinds.Kind) return P.Kind is
     (case Item is when AML_Work_Kinds.Namespace_Lookup => P.Work_Lookup,
      when AML_Work_Kinds.Namespace_Store => P.Work_Store,when AML_Work_Kinds.Value_Clone => P.Work_Clone,
      when AML_Work_Kinds.Allocation_Preparation => P.Work_Allocation,
      when AML_Work_Kinds.Namespace_Snapshot => P.Work_Snapshot);
   Maximum_Sampled_Scopes : constant := 16;
   subtype Sample_Depth is Natural range 0 .. Maximum_Sampled_Scopes;
   subtype Sample_Index is Sample_Depth range 1 .. Maximum_Sampled_Scopes;
   type Sample_Frame is record Handle : Token := P.No_Token; Kind : P.Kind := P.Startup; end record;
   Samples : array (Sample_Index) of Sample_Frame;
   Sample_Count : Sample_Depth := 0;
   Seen_Sample, Previous_Entering, Gap_From_Entering, Gap_To_Entering : Boolean := False;
   Previous_Kind, Gap_From, Gap_To : P.Kind := P.Startup;
   Previous_Time, Largest_Gap : Interfaces.Unsigned_64 := 0;
   -- Diagnostic wall-counter ticks, not process CPU time. Preemption and the
   -- ordered counter-read overhead are included. No clock syscall is added.
   Clock_Calls, Clock_Ticks, Clock_Max, Counter_Regressions : Interfaces.Unsigned_64 := 0;
   Span_Start : Interfaces.Unsigned_64 := 0;
   Counter_Saturated : Boolean := False;
   function Counter return Interfaces.Unsigned_64 is
      Low, High : Interfaces.Unsigned_32;
      use System.Machine_Code;
   begin
      Asm ("lfence; rdtsc; lfence",
        Outputs => [Interfaces.Unsigned_32'Asm_Output ("=a",Low),
                    Interfaces.Unsigned_32'Asm_Output ("=d",High)],
        Clobber => "memory", Volatile => True);
      return Interfaces.Shift_Left (Interfaces.Unsigned_64 (High),32) or Interfaces.Unsigned_64 (Low);
   end Counter;
   procedure Add (Target : in out Interfaces.Unsigned_64; Amount : Interfaces.Unsigned_64) is
   begin
      if Amount > Interfaces.Unsigned_64'Last-Target then
         Target := Interfaces.Unsigned_64'Last; Counter_Saturated := True;
      else Target := Target+Amount; end if;
   end Add;
   procedure Measured_Read (Now : out Interfaces.Unsigned_64; Known : out Boolean) is
      Before : constant Interfaces.Unsigned_64 := Counter;
      After, Elapsed : Interfaces.Unsigned_64;
   begin
      ACPI_Clock_Source.Read (Now,Known);
      After := Counter; Add (Clock_Calls,1);
      if After < Before then Add (Counter_Regressions,1); return; end if;
      Elapsed := After-Before; Add (Clock_Ticks,Elapsed);
      Clock_Max := Interfaces.Unsigned_64'Max (Clock_Max,Elapsed);
   end Measured_Read;
   procedure Sample (Item : P.Kind; Entering : Boolean;
      Now : out Interfaces.Unsigned_64; Known : out Boolean) is
   begin
      Measured_Read (Now,Known);
      if Known and then Seen_Sample and then Now >= Previous_Time
        and then Now-Previous_Time > Largest_Gap
      then
         Largest_Gap := Now-Previous_Time; Gap_From := Previous_Kind; Gap_To := Item;
         Gap_From_Entering := Previous_Entering; Gap_To_Entering := Entering;
      end if;
      Seen_Sample := Known; Previous_Time := Now; Previous_Kind := Item; Previous_Entering := Entering;
   end Sample;
   procedure Start is begin Span_Start := Counter; P.Start (State); end Start;
   procedure Enter (Item : P.Kind; Handle : out Token) is
      Now : Interfaces.Unsigned_64 := 0; Known : Boolean := False;
   begin
      Handle := P.No_Token; if not P.Active (State) then return; end if;
      Sample (Item,True,Now,Known); P.Enter (State,Item,Now,Known,Handle);
      if Handle /= P.No_Token and then Sample_Count < Maximum_Sampled_Scopes then
         Sample_Count := Sample_Count+1; Samples (Sample_Count) := (Handle,Item);
      end if;
   end Enter;
   procedure Leave (Handle : in out Token) is
      Now : Interfaces.Unsigned_64 := 0; Known : Boolean := False;
   begin
      if Handle = P.No_Token then return; end if;
      if P.Active (State) then
         if Sample_Count /= 0 and then Samples (Sample_Count).Handle = Handle then
            Sample (Samples (Sample_Count).Kind,False,Now,Known); Sample_Count := Sample_Count-1;
         else
            -- The policy diagnoses malformed token order; diagnostics never
            -- unwind another scope or change the caller's execution result.
            Work_Broken := True; Measured_Read (Now,Known);
         end if;
      end if;
      P.Leave (State,Handle,Now,Known);
   end Leave;
   procedure Observe_Work (Item : AML_Work_Kinds.Kind; Entering : Boolean) is
   begin
      if not P.Active (State) or Work_Broken then return; end if;
      if Entering then
         if Depth = Maximum_Work_Depth or Overflow /= 0 then
            if Dropped < Interfaces.Unsigned_64'Last then Dropped := Dropped+1; end if;
            if Overflow = Natural'Last then Work_Broken := True;
            else Overflow := Overflow+1; end if;
         else
            Depth := Depth+1; Frames (Depth).Kind := Item;
            Enter (Work_Kind (Item),Frames (Depth).Handle);
         end if;
      elsif Overflow /= 0 then Overflow := Overflow-1;
      elsif Depth = 0 or else Frames (Depth).Kind /= Item then Work_Broken := True;
      else Leave (Frames (Depth).Handle); Depth := Depth-1;
      end if;
   end Observe_Work;
   function Number (Value : Interfaces.Unsigned_64) return String is
      Text : String (1 .. 20); First : Positive := Text'Last;
      Remaining : Interfaces.Unsigned_64 := Value;
   begin
      loop
         Text (First) := Character'Val (Character'Pos ('0')+Natural (Remaining mod 10));
         Remaining := Remaining/10; exit when Remaining = 0; First := First-1;
      end loop;
      return Text (First .. Text'Last);
   end Number;
   function Snapshot (Item : P.Kind) return P.Counters is (P.Read (State,Item));
   procedure Report is
      Span_End : Interfaces.Unsigned_64;
   begin
      if Reported then return; end if;
      Reported := True; P.Stop (State); Span_End := Counter;
      if Span_End < Span_Start then Add (Counter_Regressions,1); end if;
      ACPI_Native_Log.Info ("acpi: stage timing ok quality=" &
        (if P.Complete (State) and then Depth = 0 and then Overflow = 0 and then Dropped = 0 and then not Work_Broken then "COMPLETE" else "INCOMPLETE") &
        " clock_samples=" & Number (P.Clock_Samples (State)) & " unit=us");
      for Item in P.Kind loop
         declare C : constant P.Counters := P.Read (State,Item); begin
            if C.Calls /= 0 or else P."=" (Item, P.Work_Allocation) then
               if P.External (Item) then
                  ACPI_Native_Log.Info ("acpi: stage timing-call ok kind=" & P.Name (Item) &
                    " n=" & Number (C.Calls) & " samples=" & Number (C.Samples) &
                    " us=" & Number (C.Total_US) & " max_us=" & Number (C.Worst_US));
               else
                  ACPI_Native_Log.Info ("acpi: stage timing-stage ok kind=" & P.Name (Item) &
                    " us=" & Number (C.Total_US) & " external_us=" & Number (C.External_US) &
                    " unaccounted_us=" & Number (C.Total_US-C.External_US) &
                    " n=" & Number (C.Calls) & " samples=" & Number (C.Samples) & " max_us=" & Number (C.Worst_US));
               end if;
            end if;
         end;
      end loop;
      ACPI_Native_Log.Info ("acpi: stage timing-clock ok basis=TSC_WALL n=" & Number (Clock_Calls) &
        " total_ticks=" & Number (Clock_Ticks) & " max_ticks=" & Number (Clock_Max) &
        " regressions=" & Number (Counter_Regressions) & " saturated=" & Counter_Saturated'Image);
      ACPI_Native_Log.Info ("acpi: stage timing-span ok basis=TSC_WALL ticks=" &
        Number (if Span_End >= Span_Start then Span_End-Span_Start else 0) &
        " includes=PREEMPTION_AND_PROBE_OVERHEAD");
      ACPI_Native_Log.Info ("acpi: stage timing-work ok dropped=" & Number (Dropped) &
        " open=" & Number (Interfaces.Unsigned_64 (Depth)) & " overflow=" & Number (Interfaces.Unsigned_64 (Overflow)));
      ACPI_Native_Log.Info ("acpi: stage timing-gap ok basis=WALL_ONLY us=" & Number (Largest_Gap) &
        " from=" & P.Name (Gap_From) & (if Gap_From_Entering then ":ENTER" else ":LEAVE") &
        " to=" & P.Name (Gap_To) & (if Gap_To_Entering then ":ENTER" else ":LEAVE"));
   end Report;
end ACPI_Native_Startup_Timing;
