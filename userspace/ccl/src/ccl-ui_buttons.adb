with CCL.Handler_References;
with CCL.Language.Handlers;
package body CCL.UI_Buttons with SPARK_Mode is
   use type Interfaces.Integer_64;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Callbacks.Register_Result;
   use type CCL.Language.Handlers.Preparation_Status;
   procedure Apply
     (Item : in out Model; Op : Operation; Argument : CCL.Host_Values.Value;
      Catalog : CCL.Catalog.Interface_Catalog; Grants : CCL.Catalog.Granted_Bindings;
      Success : out Boolean) is
      Handler : CCL.Language.Handlers.Handler;
      Prepared : CCL.Language.Handlers.Preparation_Status;
      Diagnostic : CCL.Language.Interpretation_Result;
      Target : CCL.Callbacks.Events.Reference;
      Registered : CCL.Callbacks.Register_Result;
   begin
      Success := False;
      case Op is
         when Set_Text =>
            if Argument.Kind /= CCL.Host_Values.Text_Value then return; end if;
            Item.Text := Argument.Content; Item.Dirty := True; Success := True;
         when Close_Button =>
            if Argument.Kind /= CCL.Host_Values.Integer_Value or else Argument.Integer /= 0 then
               return;
            end if;
            Close (Item); Success := True;
         when On_Click =>
            if Argument.Kind /= CCL.Host_Values.Handler_Value or else
              not CCL.Handler_References.Valid (Argument.Action)
            then return; end if;
            --  Validate and retain once at registration; clicks never reparse.
            --  The reference carries code, NOT authority. Only this owner's
            --  explicit catalog/grants may admit it.
            CCL.Language.Handlers.Prepare
              (CCL.Handler_References.Source (Argument.Action), CCL.Handler_References.Name (Argument.Action),
               CCL.Language.Handlers.Boolean_Action, Catalog, Grants, Handler, Prepared, Diagnostic);
            if Prepared /= CCL.Language.Handlers.Prepared then return; end if;
            CCL.Callbacks.Register (Item.Registration, Handler, 4096, Target, Registered);
            Success := Registered = CCL.Callbacks.Registered;
            if Success then
               Item.Target := Target; Item.Shown := True; Item.Dirty := True; Item.Dropped := 0;
            end if;
      end case;
   end Apply;
   procedure Click (Item : in out Model; Result : out CCL.Callbacks.Events.Enqueue_Result) is
   begin
      if Enabled (Item) then CCL.Callbacks.Enqueue (Item.Registration, Item.Target, Result);
      else Result := CCL.Callbacks.Events.Inactive; end if;
   end Click;
   procedure Close (Item : in out Model) is
   begin
      CCL.Callbacks.Close (Item.Registration, Item.Dropped);
      Item.Shown := False; Item.Dirty := True;
   end Close;
   procedure Painted (Item : in out Model) is
   begin Item.Dirty := False; end Painted;
   procedure Dispatch_One
     (Item : in out Model; Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Ran : out Boolean; Outcome : out CCL.Language.Interpretation_Result) is
      procedure Dispatch is new CCL.Callbacks.Dispatch_One (Host_Context, Invoke);
      Dropped : CCL.Callbacks.Events.Pending_Count;
   begin
      if Pending (Item) > 0 then Item.Dropped := 0; end if;
      Dispatch (Item.Registration, Grants, Context, Ran, Outcome, Dropped);
      if Ran then
         --  A callback may close its own button while executing. Preserve
         --  that close's discard count; completion only discards on failure.
         if Dropped > 0 then Item.Dropped := Dropped; end if;
         if not Enabled (Item) then Item.Dirty := True; end if;
      end if;
   end Dispatch_One;
end CCL.UI_Buttons;
