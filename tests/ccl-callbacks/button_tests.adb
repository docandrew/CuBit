with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Callbacks;
with CCL.Catalog; use CCL.Catalog;
with CCL.Host_Values;
with CCL.Interfaces.Workbench_UI;
with CCL.Language; use CCL.Language;
with CCL.Language.Views;
with CCL.UI_Buttons;
with CCL.UI_Labels;
procedure Button_Tests is
   package Buttons renames CCL.UI_Buttons;
   use type CCL.Callbacks.Events.Enqueue_Result;
   use type CCL.Language.Views.Conversion_Status;
   Catalog : Interface_Catalog;
   Grants, Missing : Granted_Bindings;
   Button : Buttons.Model;
   type Host_State is record
      Calls : Natural := 0;
      Label_Calls : Natural := 0;
      Label : CCL.UI_Labels.Model;
   end record;
   Bindings : constant array (Buttons.Operation) of Unsigned_32 := [101, 102, 103];
   procedure Invoke
     (Context : in out Host_State; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result) is
      Accepted : Boolean := False;
   begin
      Context.Calls := Context.Calls + 1;
      Reply.Success := True;
      if Binding = 100 then
         Context.Label_Calls := Context.Label_Calls + 1;
         CCL.UI_Labels.Apply_Value (Context.Label, CCL.UI_Labels.Set_Text, Argument, Accepted);
      else
         Reply.Success := False;
         for Op in Buttons.Operation loop
            if Binding = Bindings (Op) then
               Buttons.Apply (Button, Op, Argument, Catalog, Grants, Accepted);
               Reply.Success := True;
            end if;
         end loop;
      end if;
      Reply.Value := CCL.Host_Values.Boolean_Constant (Accepted);
   end Invoke;
   procedure Run is new Interpret_With_Values (Host_State, Invoke);
   procedure Dispatch is new Buttons.Dispatch_One (Host_State, Invoke);
   Host : Host_State;
   Outcome : Interpretation_Result;
   Error : Catalog_Error;
   Resolved : Resolved_Operation;
   Granted : Grant_Result;
   Found, Ran, Accepted : Boolean;
   Enqueued : CCL.Callbacks.Events.Enqueue_Result;
   Source : String :=
     "(define (clicked) Boolean (ui.label-text ""Clicked!"")) " &
     "(let ((h (handler clicked))) (ui.button-on-click h))";
   procedure Install_Operation (Name : String; Binding : Unsigned_32) is
   begin
      Resolve (Catalog, Name, Resolved, Found); pragma Assert (Found);
      Install (Grants, Resolved, Binding, Granted); pragma Assert (Granted = Grant_Added);
   end Install_Operation;
   procedure Execute_Source (Text : String) is
   begin
      Run (Text, 4096, Catalog, Grants, Host, Outcome);
      if Outcome.Status /= Succeeded then
         Put_Line (Text & " -> " & Outcome.Status'Image & " " & Outcome.Diagnostic'Image);
      end if;
      pragma Assert (Outcome.Status = Succeeded);
   end Execute_Source;
   procedure Click is
   begin
      Buttons.Click (Button, Enqueued);
      pragma Assert (Enqueued = CCL.Callbacks.Events.Enqueued);
   end Click;
   procedure Reject (Text : String; Code : Diagnostic_Code) is
      Before : constant Natural := Host.Calls;
   begin
      Run (Text, 4096, Catalog, Grants, Host, Outcome);
      if Outcome.Diagnostic /= Code then
         Put_Line (Text & " -> " & Outcome.Status'Image & " " & Outcome.Diagnostic'Image);
      end if;
      pragma Assert (Outcome.Status = Type_Check_Failed and Outcome.Diagnostic = Code);
      pragma Assert (Host.Calls = Before);
   end Reject;
begin
   Initialize (Catalog); Initialize (Grants); Initialize (Missing);
   CCL.Interfaces.Workbench_UI.Publish (Catalog, Error); pragma Assert (Error = Catalog_Valid);
   Install_Operation ("ui.label-text", 100);
   for Op in Buttons.Operation loop Install_Operation ("ui." & Buttons.Name (Op), Bindings (Op)); end loop;
   Run (Source, 4096, Catalog, Missing, Host, Outcome);
   pragma Assert (Outcome.Status = Host_Authority_Denied and Host.Calls = 0);
   pragma Assert (not Buttons.Visible (Button));
   Execute_Source (Source);
   pragma Assert (Outcome.Result_Value.Boolean and Buttons.Enabled (Button) and Host.Label_Calls = 0);
   --  Source ownership: edits and subsequent interpreter invocations cannot
   --  replace an existing registration, even when the function name matches.
   Source := [others => 'x'];
   Execute_Source ("42");
   Click;
   Dispatch (Button, Grants, Host, Ran, Outcome);
   pragma Assert (Ran and Outcome.Status = Succeeded and Host.Label_Calls = 1);
   pragma Assert (CCL.UI_Labels.Image (Host.Label) = " Clicked!");
   Execute_Source ("(define (clicked) Boolean (ui.label-text ""wrong"")) " &
                   "(ui.button-on-click (handler clicked))");
   pragma Assert (not Outcome.Result_Value.Boolean);
   Click;
   Dispatch (Button, Grants, Host, Ran, Outcome);
   pragma Assert (Ran and Host.Label_Calls = 2 and CCL.UI_Labels.Image (Host.Label) = " Clicked!");
   Reject ("(define (f) Integer 42) (ui.button-on-click (handler f))", Invalid_Handler_Profile);
   Reject ("(define (f (x Boolean)) Boolean x) (handler f)", Invalid_Handler_Profile);
   Reject ("(define (f) Boolean true) (handler f)", Handler_Result_Not_Exportable);
   Reject ("(ui.button-on-click 42)", Expected_Handler);
   Buttons.Apply (Button, Buttons.Close_Button, CCL.Host_Values.Boolean_Constant (True),
                  Catalog, Grants, Accepted);
   pragma Assert (not Accepted and Buttons.Visible (Button));
   Execute_Source ("(ui.button-close)");
   pragma Assert (not Buttons.Visible (Button));
   --  A callback may close itself; pending clicks are discarded, and the
   --  active callback must finish normally without leaving a busy registration.
   Execute_Source ("(define (close) Boolean (ui.button-close)) " &
                   "(ui.button-on-click (handler close))");
   Click; Click; Click;
   Dispatch (Button, Grants, Host, Ran, Outcome);
   pragma Assert (Ran and Outcome.Status = Succeeded and Outcome.Result_Value.Boolean);
   pragma Assert (not Buttons.Visible (Button) and Buttons.Pending (Button) = 0);
   pragma Assert (Buttons.Discarded (Button) = 2);
   Dispatch (Button, Grants, Host, Ran, Outcome); pragma Assert (not Ran);
   Execute_Source ("(define (f) Boolean false) " &
                   "(ui.button-on-click (if true (handler f) (handler f)))");
   pragma Assert (Outcome.Result_Value.Boolean);
   Click;
   Dispatch (Button, Missing, Host, Ran, Outcome);
   pragma Assert (Ran and Outcome.Status = Host_Authority_Denied and not Buttons.Enabled (Button));
   Buttons.Close (Button);
   declare
      Basic, Lisp : CCL.Language.Views.Conversion;
      Text : constant String := "(define (f) Boolean false) (ui.button-on-click (handler f))";
   begin
      CCL.Language.Views.Convert (Text, CCL.Language.Views.Lisp,
        CCL.Language.Views.Basic, Catalog, Basic);
      pragma Assert (Basic.Status = CCL.Language.Views.Converted);
      CCL.Language.Views.Convert (Basic.Rendered.Data (1 .. Basic.Rendered.Length),
        CCL.Language.Views.Basic, CCL.Language.Views.Lisp, Catalog, Lisp);
      pragma Assert (Lisp.Status = CCL.Language.Views.Converted);
      Execute_Source (Lisp.Rendered.Data (1 .. Lisp.Rendered.Length));
      pragma Assert (Outcome.Result_Value.Boolean);
   end;
   Put_Line ("PASS: CCL button registration, owned handler, busy replacement, typed rejection, grant admission, self-close and BASIC views");
end Button_Tests;
