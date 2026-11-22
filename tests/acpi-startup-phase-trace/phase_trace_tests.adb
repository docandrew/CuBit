pragma Ada_2022;
with ACPI_Startup_Phase_Trace;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Interfaces;
procedure Phase_Trace_Tests is
   package T renames ACPI_Startup_Phase_Trace;
   use type T.Phase_Code, T.Phase_Edge, T.Outcome;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Why; end if;
   end Check;
   Initial : T.State;
   Maximum_Line : Natural := 0;
begin
   Check (T.Observe (Initial).Last_Phase = T.No_Phase and then
     T.Observe (Initial).Edge = T.Not_Started and then
     not T.Observe (Initial).Failure_Present and then T.Observe (Initial).Consistent,
     "fresh trace makes no phase/completion claim");
   for Phase in T.Measured_Phase loop
      declare
         Item : T.State;
         Later : constant T.Measured_Phase :=
           (if Phase = T.Measured_Phase'First then T.Measured_Phase'Last
            else T.Measured_Phase'First);
         Saved : T.Outcome;
      begin
         T.Enter (Item, Phase);
         Check (T.Observe (Item).Last_Phase = Phase and then
           T.Observe (Item).Edge = T.Phase_Entered and then
           T.Text (T.Observe (Item).Result) = "ENTERED" and then
           not T.Observe (Item).Succeeded and then T.Observe (Item).Consistent,
           "entry cannot stand for successful return");
         T.Leave (Item, Phase, "REFUSED", False);
         Saved := T.Observe (Item).Failed_Result;
         Check (T.Observe (Item).Failure_Present and then
           T.Observe (Item).Failed_Phase = Phase and then T.Text (Saved) = "REFUSED",
           "failed return has exact copied phase/outcome");
         T.Enter (Item, Later); T.Leave (Item, Later, "COMPLETE", True);
         Check (T.Observe (Item).Last_Phase = Later and then T.Observe (Item).Succeeded
           and then T.Observe (Item).Consistent and then T.Observe (Item).Failure_Present
           and then T.Observe (Item).Failed_Phase = Phase
           and then T.Observe (Item).Failed_Result = Saved,
           "later successful diagnostics cannot erase a prior failure");
         T.Leave (Item, Later, "REFUSED_AGAIN", False);
         Check (not T.Observe (Item).Consistent and then
           T.Observe (Item).Failed_Phase = Later and then
           T.Text (T.Observe (Item).Failed_Result) = "REFUSED_AGAIN",
           "duplicate exit is inconsistent and retains latest failed outcome");
         T.Enter (Item, Phase);
         Check (not T.Observe (Item).Consistent, "inconsistent history never silently heals");
      end;
      declare Item : T.State; begin
         T.Leave (Item, Phase, "WITHOUT_ENTRY", True);
         Check (not T.Observe (Item).Consistent and then
           T.Observe (Item).Edge = T.Phase_Exited and then
           not T.Observe (Item).Failure_Present,
           "exit without entry is explicit, independent of result severity");
      end;
   end loop;
   for Length in 0 .. T.Maximum_Outcome_Characters + 9 loop
      declare
         Item : T.State;
         Lower_Bound : constant Positive := 7;
         Value : String (Lower_Bound .. Lower_Bound + Length - 1) := [others => 'X'];
         Expected : constant Natural := Natural'Min (Length, T.Maximum_Outcome_Characters);
      begin
         T.Enter (Item, T.Early_Node_Errors);
         T.Leave (Item, T.Early_Node_Errors, Value, True);
         Value := [others => 'Y'];
         Check (T.Observe (Item).Result.Length = Expected and then
           T.Text (T.Observe (Item).Result) = String'(1 .. Expected => 'X'),
           "empty/non-one-based/long outcomes copied independently of caller storage");
         Check (T.Observe (Item).Result.Truncated = (Length > T.Maximum_Outcome_Characters),
           "diagnostic truncation is never hidden");
      end;
   end loop;
   for Length in 0 .. T.Maximum_Outcome_Characters + 9 loop
      declare
         Item : T.State;
         Lower_Bound : constant Positive := Positive'Last - Natural'Max (Length, 1) + 1;
         Value : constant String (Lower_Bound .. Lower_Bound + (Length - 1)) := [others => 'Z'];
         Expected : constant Natural := Natural'Min (Length, T.Maximum_Outcome_Characters);
      begin
         T.Enter (Item, T.Power_Discovery);
         T.Leave (Item, T.Power_Discovery, Value, False);
         Check (T.Text (T.Observe (Item).Failed_Result) = String'(1 .. Expected => 'Z'),
           "valid strings ending at Positive Last copy without intermediate overflow");
         Check (T.Observe (Item).Failed_Result.Truncated = (Length > T.Maximum_Outcome_Characters),
           "high-bound empty and truncated outcomes retain exact policy");
      end;
   end loop;
   for Phase in T.Phase_Code loop
      Check (T.Name (Phase) = T.Phase_Code'Image (Phase), "explicit native phase label");
   end loop;
   for Edge in T.Phase_Edge loop
      Check (T.Name (Edge) = T.Phase_Edge'Image (Edge), "explicit native edge label");
   end loop;
   for Phase in T.Phase_Code loop
      for Edge in T.Phase_Edge loop
         for Failed_Phase in T.Phase_Code loop
            declare
               type Counters is array (Boolean) of Interfaces.Unsigned_64;
               Values : constant Counters := [False => 0, True => Interfaces.Unsigned_64'Last];
            begin
               for Dropped of Values loop
                  for Result_Truncated in Boolean loop
                     for Consistent in Boolean loop
                        for Has_Failure in Boolean loop
                           for Failed_Truncated in Boolean loop
                  declare
                     View : constant T.Checkpoint :=
                       (Last_Phase => Phase, Edge => Edge,
                        Result => (Data => [others => 'X'], Length => T.Maximum_Outcome_Characters, Truncated => Result_Truncated),
                        Failed_Phase => Failed_Phase,
                        Failed_Result => (Data => [others => 'Y'], Length => T.Maximum_Outcome_Characters, Truncated => Failed_Truncated),
                        Consistent => Consistent, Failure_Present => Has_Failure, others => <>);
                     Line : constant String := T.Image (View, Dropped);
                     Expected : constant String :=
                       "acpi: phase-checkpoint last=" & T.Phase_Code'Image (Phase) &
                       " edge=" & T.Phase_Edge'Image (Edge) & " result=" & String'(1 .. T.Maximum_Outcome_Characters => 'X') &
                       " truncated=" & Boolean'Image (Result_Truncated) &
                       " consistent=" & Boolean'Image (Consistent) &
                       " non-success=" & Boolean'Image (Has_Failure) &
                       " failed-phase=" & T.Phase_Code'Image (Failed_Phase) &
                       " failed-result=" & String'(1 .. T.Maximum_Outcome_Characters => 'Y') &
                       " failed-truncated=" & Boolean'Image (Failed_Truncated) &
                       " publisher-dropped=" & Interfaces.Unsigned_64'Image (Dropped);
                  begin
                     Maximum_Line := Natural'Max (Maximum_Line, Line'Length);
                     Check (Line = Expected and then Line'Length <= T.Maximum_Diagnostic_Characters
                       and then Ada.Strings.Fixed.Index (Line, "last=" & T.Name (Phase)) /= 0
                       and then Ada.Strings.Fixed.Index (Line, "edge=" & T.Name (Edge)) /= 0
                       and then Ada.Strings.Fixed.Index (Line, "failed-phase=" & T.Name (Failed_Phase)) /= 0
                       and then Ada.Strings.Fixed.Index (Line, "publisher-dropped=" & Interfaces.Unsigned_64'Image (Dropped)) /= 0,
                       "actual checkpoint formatter remains bounded with full-width counters and maximum outcomes");
                  end;
                           end loop;
                        end loop;
                     end loop;
                  end loop;
               end loop;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Startup phase trace maximum" & Maximum_Line'Image);
   Ada.Text_IO.Put_Line ("Startup phase trace checks" & Checks'Image);
end Phase_Trace_Tests;
