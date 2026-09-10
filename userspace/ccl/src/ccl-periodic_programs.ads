with Interfaces;
with CCL.Language;
with CCL.Catalog;
with CCL.VM;

-- Transport- and UI-independent lifecycle for one bounded recurring program.
-- The owning host supplies time and performs evaluation with its explicit
-- bindings. This record grants no authority and creates no thread or process.
package CCL.Periodic_Programs with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   subtype Timestamp is Interfaces.Unsigned_64;
   subtype Interval_Ms is Positive range 1_000 .. 60_000;
   subtype Fuel_Budget is Positive range 1 .. 4_096;
   type Lifecycle is (Empty, Waiting, Executing, Stopping, Stopped, Faulted);
   type Load_Result is (Loaded, Busy, Source_Too_Long, Identity_Exhausted);
   type Program is private;
   type Invocation is private;

   procedure Load
     (Item : in out Program; Source : String; Now : Timestamp;
      Interval : Interval_Ms; Fuel : Fuel_Budget; Result : out Load_Result);
   procedure Stop (Item : in out Program)
     with Post => State (Item) in Empty | Stopping | Stopped and then
       Next_Deadline (Item) = Timestamp'Last;
   -- At most one outstanding invocation. Late timer events coalesce; they
   -- never accumulate a queue of evaluations that must be caught up.
   procedure Claim_Due
     (Item : in out Program; Now : Timestamp;
      Ticket : out Invocation; Ready : out Boolean);
   procedure Complete
     (Item : in out Program; Ticket : Invocation; Now : Timestamp;
      Outcome : CCL.Language.Interpretation_Result; Accepted : out Boolean);

   -- A trusted host supplies bindings and time; the source cannot manufacture
   -- either. Synchronous scalar host calls for now (not cancellable mid-call).
   -- This runner is independent of the output surface and of the Workbench.
   generic
      type Host_Context is limited private;
      with function Now (Context : Host_Context) return Timestamp;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.VM.Value; Value : out CCL.VM.Value;
         Success : out Boolean);
   procedure Evaluate_Due
     (Item : in out Program;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context;
      Updated : out Boolean);

   function State (Item : Program) return Lifecycle;
   function Identity (Item : Program) return Timestamp;
   function Next_Deadline (Item : Program) return Timestamp;
   function Source_Text (Item : Program) return String;
   function Fuel (Item : Program) return Fuel_Budget;
   function Interval (Item : Program) return Interval_Ms;
   function Completed_Runs (Item : Program) return Timestamp;
   function Last_Result (Item : Program) return CCL.Language.Interpretation_Result;

private
   type Invocation is record
      Generation : Timestamp := 0;
      Sequence : Timestamp := 0;
   end record;
   type Program is record
      Status : Lifecycle := Empty;
      Generation : Timestamp := 0;
      Runs : Timestamp := 0;
      Due : Timestamp := Timestamp'Last;
      Period : Interval_Ms := 1_000;
      Budget : Fuel_Budget := 4_096;
      Text : String (1 .. CCL.Language.MAX_SOURCE_LENGTH) := [others => ' '];
      Length : Natural range 0 .. CCL.Language.MAX_SOURCE_LENGTH := 0;
      Outcome : CCL.Language.Interpretation_Result;
   end record;
   function State (Item : Program) return Lifecycle is (Item.Status);
   function Identity (Item : Program) return Timestamp is (Item.Generation);
   function Next_Deadline (Item : Program) return Timestamp is
     (if Item.Status = Waiting then Item.Due else Timestamp'Last);
   function Source_Text (Item : Program) return String is (Item.Text (1 .. Item.Length));
   function Fuel (Item : Program) return Fuel_Budget is (Item.Budget);
   function Interval (Item : Program) return Interval_Ms is (Item.Period);
   function Completed_Runs (Item : Program) return Timestamp is (Item.Runs);
   function Last_Result (Item : Program) return CCL.Language.Interpretation_Result is (Item.Outcome);
end CCL.Periodic_Programs;
