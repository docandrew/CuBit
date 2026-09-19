package body CuBit.Protocols.Stream_Bindings with SPARK_Mode is
   use type Connections.Decision;
   use type Connections.Binding_Reference;

   function Inspect (Item : Binding) return View is
     (Generation => Item.Generation, State => Item.Change.State,
      Ticket => (if Item.Change.State = Idle then 0 else Item.Change.Ticket),
      Next_Ticket => Item.Next_Ticket, Active => Item.Active,
      Staged => (if Item.Change.State = Idle then (Present => False)
                 else (Present => True,
                       Transport_Ticket => Item.Change.Transport_Ticket,
                       Value => Item.Change.Route)));

   function Reference (Item : Binding) return Connections.Binding_Reference is
     (Identity => Item.Identity, Generation => Item.Generation);

   procedure Prepare
     (Item : in out Binding; Desired : Connections.Request;
      Evidence : Connections.Approvals; Ticket : out Unsigned_64;
      Outcome : out Result) is
   begin
      Ticket := 0;
      if Connections.Check (Desired, Evidence) /=
        Connections.Connection_Allowed
      then
         Outcome := Not_Authorized;
      elsif Desired.Binding /= Reference (Item) then
         Outcome := Stale_Binding;
      elsif Item.Change.State /= Idle then
         Outcome := Busy;
      elsif Item.Next_Ticket = 0 or Item.Generation = Live_Id'Last then
         Outcome := Exhausted;
      else
         Ticket := Item.Next_Ticket;
         if Item.Next_Ticket = Unsigned_64'Last then
            Item.Next_Ticket := 0;
         else
            Item.Next_Ticket := Item.Next_Ticket + 1;
         end if;
         Item.Change := (State => Preparing, Ticket => Ticket,
                         Transport_Ticket => Ticket,
                         Route => Desired);
         Outcome := Succeeded;
      end if;
   end Prepare;

   procedure Commit
     (Item : in out Binding; Actor, Ticket : Unsigned_64;
      Evidence : Connections.Approvals; Ready : Boolean;
      Outcome : out Result) is
      Previous : constant Optional_Route := Item.Active;
   begin
      if Item.Change.State /= Preparing then
         Outcome := Wrong_Transition;
      elsif Item.Change.Ticket /= Ticket or else
        Item.Change.Route.Controller_Instance /= Actor
      then
         Outcome := Not_Authorized;
      elsif Connections.Check (Item.Change.Route, Evidence) /=
        Connections.Connection_Allowed
      then
         Outcome := Not_Authorized;
      elsif Item.Change.Route.Binding /= Reference (Item) then
         Outcome := Stale_Binding;
      elsif not Ready then
         Outcome := Resources_Not_Ready;
      elsif Item.Generation = Live_Id'Last then
         Outcome := Exhausted;
      else
         Item.Active :=
           (Present => True,
            Transport_Ticket => Item.Change.Transport_Ticket,
            Value => Item.Change.Route);
         Item.Generation := Item.Generation + 1;
         if Previous.Present then
            Item.Change := (State => Retiring, Ticket => Item.Change.Ticket,
                            Transport_Ticket => Previous.Transport_Ticket,
                            Route => Previous.Value);
         else
            Item.Change := (State => Idle);
         end if;
         Outcome := Succeeded;
      end if;
   end Commit;

   procedure Abort_Preparation
     (Item : in out Binding; Actor, Ticket : Unsigned_64;
      Outcome : out Result) is
   begin
      if Item.Change.State /= Preparing then
         Outcome := Wrong_Transition;
      elsif Item.Change.Ticket /= Ticket or else
        Item.Change.Route.Controller_Instance /= Actor
      then
         Outcome := Not_Authorized;
      else
         Item.Change := (State => Retiring, Ticket => Item.Change.Ticket,
                         Transport_Ticket => Item.Change.Transport_Ticket,
                         Route => Item.Change.Route);
         Outcome := Succeeded;
      end if;
   end Abort_Preparation;

   procedure Finish_Retirement
     (Item : in out Binding; Ticket : Unsigned_64; Quiescent : Boolean;
      Outcome : out Result) is
   begin
      if Item.Change.State /= Retiring then
         Outcome := Wrong_Transition;
      elsif Item.Change.Ticket /= Ticket then
         Outcome := Not_Authorized;
      elsif not Quiescent then
         Outcome := Resources_Not_Ready;
      else
         Item.Change := (State => Idle);
         Outcome := Succeeded;
      end if;
   end Finish_Retirement;
end CuBit.Protocols.Stream_Bindings;
