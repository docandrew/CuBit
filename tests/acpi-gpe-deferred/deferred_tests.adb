pragma Ada_2022;
with Ada.Text_IO; with Interfaces; with ACPI_GPE_Lifecycle;
procedure Deferred_Tests is
   package G renames ACPI_GPE_Lifecycle;
   use type G.Action_Result, G.Phase, G.Report, G.Source_State, G.Failure_Reason;
   use type G.Handler_Outcome, G.Trigger_Kind;
   type Source_ID is new Interfaces.Unsigned_64;
   No_Source : constant Source_ID := 0;
   Source : constant Source_ID := 16#FEED_1234_0000_0001#;
   Handler_Allowance : constant Positive := 7;
   type Fault_Kind is (None, Mask_Refused, Enable_Unknown);
   Fault : Fault_Kind := None;
   Checks, Masks, Clears, Enables, Invocations : Natural := 0;
   Masked, Reenter : Boolean := False;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with "deferred check" & Checks'Image; end if; end Check;
   procedure Mask (Item : Source_ID; Result : out G.IO_Outcome) is
   begin Check (Item = Source); Masks := Masks + 1;
      if Fault = Mask_Refused then Result := G.Refused;
      else Masked := True; Result := G.Completed; end if;
   end Mask;
   procedure Clear (Item : Source_ID; Result : out G.IO_Outcome) is
   begin Check (Item = Source and Masked); Clears := Clears + 1; Result := G.Completed; end Clear;
   procedure Enable (Item : Source_ID; Result : out G.IO_Outcome) is
   begin Check (Item = Source and Masked); Enables := Enables + 1;
      Masked := False; Result := (if Fault = Enable_Unknown then G.Completion_Unknown else G.Completed);
   end Enable;
   procedure Invoke (Item : Source_ID; Budget : Natural; Result : out G.Handler_Outcome; Charged : out Natural);
   package D is new G.Driver (Source_ID, No_Source, Mask, Clear, Enable, Invoke);
   Active : D.Session;
   procedure Invoke (Item : Source_ID; Budget : Natural; Result : out G.Handler_Outcome; Charged : out Natural) is
      Action : G.Action_Result; Before : constant G.Report := D.Observe (Active);
   begin
      Check (Item = Source and Masked and Budget = Handler_Allowance); Invocations := Invocations + 1;
      if Reenter then
         D.Complete_Deferred (Active, G.Handler_Succeeded, 0, Action);
         Check (Action = G.Wrong_Phase and D.Observe (Active) = Before);
         D.Begin_Deferred (Active, Handler_Allowance, Action);
         Check (Action = G.Wrong_Phase and D.Observe (Active) = Before);
      end if;
      Result := G.Handler_Succeeded; Charged := 2;
   end Invoke;
   procedure Reset is
   begin Masks := 0; Clears := 0; Enables := 0; Invocations := 0; Masked := False; Fault := None; end Reset;
   procedure Refused (Run : in out D.Session) is
      Before : constant G.Report := D.Observe (Run); Action : G.Action_Result;
      Ops : constant Natural := Masks + Clears + Enables + Invocations;
   begin
      D.Begin_Deferred (Run, Handler_Allowance, Action); Check (Action = G.Wrong_Phase);
      D.Complete_Deferred (Run, G.Handler_Succeeded, 0, Action); Check (Action = G.Wrong_Phase);
      D.Service (Run, Handler_Allowance, Action); Check (Action = G.Wrong_Phase);
      D.Finish (Run, Action); Check (Action = G.Wrong_Phase);
      Check (D.Observe (Run) = Before and Masks + Clears + Enables + Invocations = Ops);
   end Refused;
begin
   for Trigger in G.Trigger_Kind loop
      declare Run : D.Session; Action : G.Action_Result; Before : G.Report; Ops : Natural; begin
         Reset;
         D.Begin_Deferred (Run, Handler_Allowance, Action); Check (Action = G.Wrong_Phase and Masks = 0);
         D.Complete_Deferred (Run, G.Handler_Succeeded, 0, Action); Check (Action = G.Wrong_Phase and Masks = 0);
         D.Start (Run, Source, Trigger, Action); Check (Action = G.Accepted and Masked and D.Observe (Run).Stage = G.Ready);
         Check (Masks = 1 and Clears = (if Trigger = G.Edge_Triggered then 1 else 0));
         Before := D.Observe (Run); D.Begin_Deferred (Run, 0, Action);
         Check (Action = G.No_Budget and D.Observe (Run) = Before);
         Ops := Masks + Clears + Enables; D.Begin_Deferred (Run, Handler_Allowance, Action);
         Check (Action = G.Accepted and D.Observe (Run).Stage = G.Executing and D.Observe (Run).Deferred
            and D.Observe (Run).Handler_Budget = Handler_Allowance and Masked and Invocations = 0
            and Masks + Clears + Enables = Ops);
         Before := D.Observe (Run);
         D.Begin_Deferred (Run, Handler_Allowance, Action); Check (Action = G.Wrong_Phase);
         D.Service (Run, Handler_Allowance, Action); Check (Action = G.Wrong_Phase);
         D.Finish (Run, Action); Check (Action = G.Wrong_Phase and D.Observe (Run) = Before and Masked);
         D.Complete_Deferred (Run, G.Handler_Succeeded, 3, Action);
         Check (Action = G.Accepted and D.Observe (Run).Stage = G.Handler_Done and not D.Observe (Run).Deferred
            and D.Observe (Run).Charged = 3 and Masked and Enables = 0 and Invocations = 0);
         Before := D.Observe (Run); D.Complete_Deferred (Run, G.Handler_Failed, 0, Action);
         Check (Action = G.Wrong_Phase and D.Observe (Run) = Before);
         D.Finish (Run, Action); Check (Action = G.Accepted and D.Observe (Run).Stage = G.Finished and not Masked
            and Masks = 1 and Clears = 1 and Enables = 1 and Invocations = 0);
         Refused (Run);
      end;
   end loop;
   for Outcome in G.Handler_Outcome loop
      if Outcome /= G.Handler_Succeeded then
         declare Run : D.Session; Action : G.Action_Result; begin
            Reset; D.Start (Run, Source, G.Level_Triggered, Action); D.Begin_Deferred (Run, Handler_Allowance, Action);
            D.Complete_Deferred (Run, Outcome, 3, Action);
            Check (Action = G.Accepted and D.Observe (Run).Stage = G.Failed and not D.Observe (Run).Deferred
               and D.Observe (Run).Handler_Status = Outcome and D.Observe (Run).Charged = 3 and Masked
               and D.Observe (Run).Source = G.Known_Masked and Clears = 0 and Enables = 0 and Invocations = 0);
            Refused (Run);
         end;
      end if;
   end loop;
   declare Run : D.Session; Action : G.Action_Result; begin
      Reset; D.Start (Run, Source, G.Level_Triggered, Action); D.Begin_Deferred (Run, Handler_Allowance, Action);
      D.Complete_Deferred (Run, G.Handler_Succeeded, Handler_Allowance + 1, Action);
      Check (Action = G.Accepted and D.Observe (Run).Reason = G.Provider_Contract_Error and D.Observe (Run).Charged = 0
         and D.Observe (Run).Reported_Charge = Handler_Allowance + 1 and Masked);
      Refused (Run);
   end;
   Reset; Reenter := True; declare Action : G.Action_Result; begin
      D.Start (Active, Source, G.Level_Triggered, Action); D.Service (Active, Handler_Allowance, Action);
      Check (Action = G.Accepted and D.Observe (Active).Stage = G.Handler_Done and Invocations = 1 and not D.Observe (Active).Deferred);
      D.Finish (Active, Action); Check (Action = G.Accepted and not Masked);
   end;
   declare Run : D.Session; Action : G.Action_Result; begin
      Reset; Fault := Mask_Refused; D.Start (Run, Source, G.Level_Triggered, Action);
      Check (D.Observe (Run).Stage = G.Failed and D.Observe (Run).Source = G.Unknown_State and not Masked);
      Refused (Run);
   end;
   declare Run : D.Session; Action : G.Action_Result; begin
      Reset; D.Start (Run, Source, G.Level_Triggered, Action); D.Begin_Deferred (Run, Handler_Allowance, Action);
      D.Complete_Deferred (Run, G.Handler_Succeeded, 0, Action); Fault := Enable_Unknown; D.Finish (Run, Action);
      Check (D.Observe (Run).Stage = G.Failed and D.Observe (Run).Source = G.Unknown_State
         and D.Observe (Run).Reason = G.Uncertain_Completion); Refused (Run);
   end;
   Ada.Text_IO.Put_Line ("Deferred checks" & Checks'Image);
end Deferred_Tests;
