--  CCL VM dispatch-loop benchmark: compiles a few loop-heavy programs once,
--  then times only Continue_Execution (Initialize, which clears the run's
--  regions, is outside the timed part). Prints ns per executed instruction;
--  the best of the rounds is the figure to compare (tests/ccl-vm-bench).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CCL.Catalog;
with CCL.Language;
with CCL.Compiler;
with CCL.VM;

procedure VM_Bench is
   use type CCL.Compiler.Compilation_Status;
   use type CCL.VM.Validation_Error;
   use type CCL.VM.Execution_Status;

   Fuel : constant := 50_000_000;
   Rounds : constant Positive :=
     (if Ada.Command_Line.Argument_Count > 0 then Positive'Value (Ada.Command_Line.Argument (1)) else 5);
   Failed : Boolean := False;

   procedure Bench (Name, Source : String; Repeats : Positive) is
      Catalog : CCL.Catalog.Interface_Catalog;
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Program : CCL.VM.Validated_Program;
      Valid : CCL.VM.Validation_Error;
      State : CCL.VM.Machine_State;
      Result : CCL.VM.Execution_Result;
      Best : Duration := Duration'Last;
      Steps : Unsigned_64 := 0;
      Answer : Integer_64 := 0;
   begin
      CCL.Catalog.Initialize (Catalog);
      CCL.Language.Analyze (Source, Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      if Compiled.Status /= CCL.Compiler.Compilation_Succeeded then
         Put_Line (Name & ": compile " & Compiled.Status'Image); Failed := True; return;
      end if;
      CCL.VM.Verify (Compiled.Program, Program, Valid);
      if Valid /= CCL.VM.Valid then
         Put_Line (Name & ": verify " & Valid'Image); Failed := True; return;
      end if;
      for Round in 1 .. Rounds loop
         declare
            Elapsed : Time_Span := Time_Span_Zero;
         begin
            Steps := 0;
            for R in 1 .. Repeats loop
               CCL.VM.Initialize (Program, Fuel, State);
               declare
                  Start : constant Time := Clock;
               begin
                  CCL.VM.Continue_Execution (Program, State, Result);
                  Elapsed := Elapsed + (Clock - Start);
               end;
               if Result.Status /= CCL.VM.Completed then
                  Put_Line (Name & ": status " & Result.Status'Image); Failed := True; return;
               end if;
               Steps := Steps + Unsigned_64 (Result.Steps);
               Answer := Result.Result_Value.Integer;
            end loop;
            Best := Duration'Min (Best, To_Duration (Elapsed));
         end;
      end loop;
      Put_Line (Name & ":" & Steps'Image & " instructions/round, best" &
                Duration'Image (Best * 1000.0) & " ms, ns/instruction" &
                Integer'Image (Integer (Float (Best) * 1.0E9 / Float (Steps))) &
                " (result" & Answer'Image & ")");
   end Bench;
begin
   Bench ("fold-arith",
          "(fold (fn ((acc Integer) (n Integer)) (mod (+ (* acc 3) n) 1000003)) 0 (range 1 4000))", 50);
   Bench ("count-compare",
          "(count (fn ((n Integer)) (if (< (mod n 3) 2) (> n 10) false)) (range 1 4000))", 50);
   Bench ("straight-line",
          "(let ((a 1)) (let ((b 2)) (let ((c 3)) (+ (+ (* a b) (- c a)) (+ (* (+ a b) (- b c)) " &
          "(+ (mod (+ a c) 7) (+ (* a (+ b c)) (- (+ a b) c))))))))", 20_000);
   if Failed then
      Put_Line ("FAILED");
   end if;
end VM_Bench;
