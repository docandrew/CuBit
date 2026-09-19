package body CCL.Callbacks with SPARK_Mode is
   use type CCL.Language.Interpretation_Status;
   procedure Register
     (Item : in out Registration; Handler : CCL.Language.Handlers.Handler;
      Fuel : Fuel_Budget; Target : out Events.Reference; Result : out Register_Result) is
      Opened : Events.Open_Result;
   begin
      if not CCL.Language.Handlers.Ready (Handler) then
         declare Empty_Target : Events.Reference;
         begin Target := Empty_Target; end;
         Result := Invalid_Handler;
         return;
      end if;
      Events.Open (Item.Queue, Target, Opened);
      case Opened is
         when Events.Opened => Item.Program := Handler; Item.Budget := Fuel; Result := Registered;
         when Events.Busy => Result := Busy;
         when Events.Identity_Exhausted => Result := Identity_Exhausted;
      end case;
   end Register;
   procedure Enqueue
     (Item : in out Registration; Target : Events.Reference; Result : out Events.Enqueue_Result) is
   begin Events.Enqueue (Item.Queue, Target, Result); end Enqueue;
   procedure Close (Item : in out Registration; Discarded : out Events.Pending_Count) is
   begin Events.Close (Item.Queue, Discarded); end Close;
   procedure Dispatch_One
     (Item : in out Registration; Current_Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Ran : out Boolean;
      Outcome : out CCL.Language.Interpretation_Result;
      Discarded : out Events.Pending_Count) is
      procedure Execute is new CCL.Language.Handlers.Execute (Host_Context, Invoke);
      Ticket : Events.Invocation;
      Accepted : Boolean;
   begin
      Outcome := (others => <>);
      Discarded := 0;
      Events.Claim (Item.Queue, Ticket, Ran);
      if not Ran then return; end if;
      Execute (Item.Program, Item.Budget, Current_Grants, Context, Outcome);
      Events.Complete (Item.Queue, Ticket, Outcome.Status = CCL.Language.Succeeded, Accepted, Discarded);
   end Dispatch_One;
end CCL.Callbacks;
