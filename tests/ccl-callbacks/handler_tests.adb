with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog; use CCL.Catalog;
with CCL.Language; use CCL.Language;
with CCL.Language.Handlers; use CCL.Language.Handlers;
with CCL.Callbacks;
with CCL.Host_Values;
with CCL.Interfaces.Workbench_UI;
with CCL.UI_Labels;
procedure Handler_Tests is
   package C renames CCL.Callbacks;
   use type C.Register_Result;
   use type C.Events.Enqueue_Result;
   use type C.Events.Lifecycle;
   type Host_State is record
      Calls : Natural := 0;
      Label : CCL.UI_Labels.Model;
      Fail : Boolean := False;
   end record;
   procedure Invoke
     (Context : in out Host_State; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Value : out CCL.Host_Values.Value; Success : out Boolean) is
   begin
      Context.Calls := Context.Calls + 1;
      Success := Binding = 78 and not Context.Fail;
      if Success then CCL.UI_Labels.Apply_Value (Context.Label, CCL.UI_Labels.Set_Text, Argument, Success); end if;
      Value := CCL.Host_Values.Boolean_Constant (Success);
   end Invoke;
   procedure Run is new Execute (Host_State, Invoke);
   procedure Dispatch is new C.Dispatch_One (Host_State, Invoke);
   Catalog : Interface_Catalog;
   Grants, Missing, Rebound : Granted_Bindings;
   Error : Catalog_Error;
   Operation : Resolved_Operation;
   Grant : Grant_Result;
   Found, Ran : Boolean;
   H, Invalid : Handler;
   Status : Preparation_Status;
   Outcome : Interpretation_Result;
   Host : Host_State;
   Registration : C.Registration;
   Target, Old_Target : C.Events.Reference;
   Registered : C.Register_Result;
   Enqueued : C.Events.Enqueue_Result;
   Discarded : C.Events.Pending_Count;
   Source : String :=
     "(define (caption) String ""clicked"") " &
     "(define (clicked) Boolean (ui.label-text (caption))) " &
     "(ui.label-text ""must not execute the main expression"")";
begin
   Initialize (Catalog); Initialize (Grants); Initialize (Missing); Initialize (Rebound);
   CCL.Interfaces.Workbench_UI.Publish (Catalog, Error); pragma Assert (Error = Catalog_Valid);
   Resolve (Catalog, "ui.label-text", Operation, Found); pragma Assert (Found);
   Install (Grants, Operation, 78, Grant); pragma Assert (Grant = Grant_Added);
   Install (Rebound, Operation, 79, Grant); pragma Assert (Grant = Grant_Added);
   Prepare (Source, "clicked", Boolean_Action, Catalog, Missing, H, Status, Outcome);
   pragma Assert (Status = Admission_Denied and not Ready (H) and Host.Calls = 0);
   Prepare (Source, "absent", Boolean_Action, Catalog, Grants, H, Status, Outcome);
   pragma Assert (Status = Unknown_Entry and not Ready (H));
   Prepare (Source, "caption", Boolean_Action, Catalog, Grants, H, Status, Outcome);
   pragma Assert (Status = Wrong_Profile and not Ready (H));
   Prepare ("(define (f (b Boolean)) Boolean b) true", "f", Boolean_Action,
            Catalog, Grants, H, Status, Outcome);
   pragma Assert (Status = Wrong_Profile and not Ready (H));
   Prepare ("(define (f) Boolean 42) true", "f", Boolean_Action,
            Catalog, Grants, H, Status, Outcome);
   pragma Assert (Status = Invalid_Source and not Ready (H));
   C.Register (Registration, Invalid, 4096, Target, Registered);
   pragma Assert (Registered = C.Invalid_Handler and C.State (Registration) = C.Events.Empty);
   Prepare (Source, "clicked", Boolean_Action, Catalog, Grants, H, Status, Outcome);
   pragma Assert (Status = Prepared and Ready (H) and Host.Calls = 0);
   Source := [others => 'x']; -- caller's editor/document can change immediately
   Run (H, 4096, Missing, Host, Outcome);
   pragma Assert (Outcome.Status = Host_Authority_Denied and Host.Calls = 0);
   Run (H, 4096, Rebound, Host, Outcome);
   pragma Assert (Outcome.Status = Host_Authority_Denied and Host.Calls = 0);
   Run (H, 0, Grants, Host, Outcome);
   pragma Assert (Outcome.Status = Evaluation_Fuel_Exhausted and Host.Calls = 0);
   Run (H, 4096, Grants, Host, Outcome);
   pragma Assert (Outcome.Status = Succeeded and Host.Calls = 1);
   pragma Assert (CCL.UI_Labels.Image (Host.Label) = " clicked");
   C.Register (Registration, H, 4096, Target, Registered); pragma Assert (Registered = C.Registered);
   Old_Target := Target;
   H := Invalid; -- registration owns its copy too
   for I in 1 .. C.Events.Capacity loop
      C.Enqueue (Registration, Target, Enqueued); pragma Assert (Enqueued = C.Events.Enqueued);
   end loop;
   C.Enqueue (Registration, Target, Enqueued); pragma Assert (Enqueued = C.Events.Queue_Full);
   for I in 1 .. C.Events.Capacity loop
      Dispatch (Registration, Grants, Host, Ran, Outcome, Discarded);
      pragma Assert (Ran and Outcome.Status = Succeeded and Discarded = 0);
   end loop;
   pragma Assert (Host.Calls = 1 + C.Events.Capacity);
   Dispatch (Registration, Grants, Host, Ran, Outcome, Discarded); pragma Assert (not Ran);
   C.Enqueue (Registration, Target, Enqueued);
   C.Enqueue (Registration, Target, Enqueued);
   Dispatch (Registration, Missing, Host, Ran, Outcome, Discarded);
   pragma Assert (Ran and Outcome.Status = Host_Authority_Denied and Discarded = 1);
   pragma Assert (C.State (Registration) = C.Events.Faulted and Host.Calls = 9);
   Prepare ("(define (f) Boolean false) true", "f", Boolean_Action, Catalog, Grants, H, Status, Outcome);
   C.Register (Registration, H, 4096, Target, Registered); pragma Assert (Registered = C.Registered);
   C.Enqueue (Registration, Old_Target, Enqueued); pragma Assert (Enqueued = C.Events.Stale_Target);
   C.Enqueue (Registration, Target, Enqueued);
   Dispatch (Registration, Grants, Host, Ran, Outcome, Discarded);
   pragma Assert (Ran and Outcome.Status = Succeeded and not Outcome.Result_Value.Boolean);
   pragma Assert (C.State (Registration) = C.Events.Listening); -- false is NOT a retry/failure
   C.Close (Registration, Discarded);
   Prepare ("(define (f) Boolean (ui.label-text ""failure"")) true", "f", Boolean_Action,
            Catalog, Grants, H, Status, Outcome);
   C.Register (Registration, H, 4096, Target, Registered);
   Host.Fail := True;
   C.Enqueue (Registration, Target, Enqueued);
   Dispatch (Registration, Grants, Host, Ran, Outcome, Discarded);
   pragma Assert (Ran and Outcome.Status = Host_Call_Failed and C.State (Registration) = C.Events.Faulted);
   C.Close (Registration, Discarded);
   C.Register (Registration, H, 1, Target, Registered);
   C.Enqueue (Registration, Target, Enqueued);
   Dispatch (Registration, Grants, Host, Ran, Outcome, Discarded);
   pragma Assert (Ran and Outcome.Status = Evaluation_Fuel_Exhausted);
   Put_Line ("PASS: retained typed handler, no main execution, owned snapshots, current-grant pinning, queued dispatch and failure isolation");
end Handler_Tests;
