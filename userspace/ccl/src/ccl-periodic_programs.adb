package body CCL.Periodic_Programs with SPARK_Mode is
   use type CCL.Language.Interpretation_Status;

   procedure Evaluate_Due
     (Item : in out Program;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context;
      Updated : out Boolean)
   is
      procedure Evaluate is new CCL.Language.Interpret_With_Host
        (Host_Context, Invoke);
      Ticket : Invocation;
      Ready : Boolean;
      Outcome : CCL.Language.Interpretation_Result;
   begin
      Updated := False;
      Claim_Due (Item, Now (Context), Ticket, Ready);
      if not Ready then return; end if;
      Evaluate (Source_Text (Item), Fuel (Item), Catalog, Grants, Context, Outcome);
      Complete (Item, Ticket, Now (Context), Outcome, Updated);
   end Evaluate_Due;

   procedure Load
     (Item : in out Program; Source : String; Now : Timestamp;
      Interval : Interval_Ms; Fuel : Fuel_Budget; Result : out Load_Result)
   is
      Generation : constant Timestamp := Item.Generation;
   begin
      if Item.Status in Waiting | Executing | Stopping then
         Result := Busy;
      elsif Source'Length > Item.Text'Length then
         Result := Source_Too_Long;
      elsif Generation = Timestamp'Last then
         Result := Identity_Exhausted;
      else
         Item := (Status => Waiting, Generation => Generation + 1,
                  Due => Now, Period => Interval, Budget => Fuel, others => <>);
         Item.Length := Source'Length;
         Item.Text (1 .. Item.Length) := Source;
         Result := Loaded;
      end if;
   end Load;

   procedure Stop (Item : in out Program) is
   begin
      case Item.Status is
         when Executing | Stopping => Item.Status := Stopping;
         when Empty => null;
         when others => Item.Status := Stopped;
      end case;
   end Stop;

   procedure Claim_Due
     (Item : in out Program; Now : Timestamp;
      Ticket : out Invocation; Ready : out Boolean)
   is
   begin
      Ticket := (others => <>);
      Ready := False;
      if Item.Status /= Waiting or else Now < Item.Due then
         return;
      elsif Item.Runs = Timestamp'Last then
         Item.Status := Faulted;
      else
         Ticket := (Generation => Item.Generation, Sequence => Item.Runs + 1);
         Item.Status := Executing;
         Ready := True;
      end if;
   end Claim_Due;

   procedure Complete
     (Item : in out Program; Ticket : Invocation; Now : Timestamp;
      Outcome : CCL.Language.Interpretation_Result; Accepted : out Boolean)
   is
   begin
      Accepted := False;
      if Item.Status not in Executing | Stopping or else
        Ticket.Generation /= Item.Generation or else
        Ticket.Sequence = 0 or else Ticket.Sequence - 1 /= Item.Runs
      then
         return;
      end if;
      Item.Runs := Ticket.Sequence;
      Item.Outcome := Outcome;
      if Item.Status = Stopping then
         Item.Status := Stopped;
      elsif Outcome.Status /= CCL.Language.Succeeded or else
        Now > Timestamp'Last - Timestamp (Item.Period)
      then
         Item.Status := Faulted;
      else
         -- Reschedule from completion, not the previous deadline: no catch-up
         -- burst after a delayed or non-cancellable service invocation.
         Item.Due := Now + Timestamp (Item.Period);
         Item.Status := Waiting;
      end if;
      Accepted := True;
   end Complete;
end CCL.Periodic_Programs;
