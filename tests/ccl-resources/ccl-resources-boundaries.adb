package body CCL.Resources.Boundaries is
   procedure Check is
      Owner : Registry (99);
      Types : CCL.Types.Registry;
      Kind : CCL.Types.Type_Reference;
      Defined : CCL.Types.Definition_Result;
      Session : Run;
      Factory, Call : Ticket;
      Item : Reference;
      Result : Outcome;
      use type CCL.Types.Definition_Result;
   begin
      CCL.Types.Define (Types, (Identifier => CCL.Types.Named ("Resource"),
        Form => CCL.Types.Resource, others => <>), Kind, Defined);
      pragma Assert (Defined = CCL.Types.Defined);
      Owner.Session := Serial'Last - 1;
      Start (Owner, Types, Session, Result);
      pragma Assert (Result = Succeeded and Session.Number = Serial'Last);
      Owner.Issued := Serial'Last - 1;
      Reserve (Owner, Session, Kind, Factory, Result);
      pragma Assert (Result = Succeeded and Factory.Number = Serial'Last);
      Publish (Owner, Factory, True, Item, Result);
      pragma Assert (Result = Succeeded);
      Begin_Use (Owner, Item, Kind, Call, Result);
      pragma Assert (Result = Identity_Exhausted and Call = No_Ticket and Current (Owner, Item));
      Reserve (Owner, Session, Kind, Call, Result);
      pragma Assert (Result = Identity_Exhausted and Call = No_Ticket);
      Retire (Owner, Item, Result);
      pragma Assert (Result = Succeeded);
      Begin_Cleanup (Owner, Factory, Call, Result);
      pragma Assert (Result = Identity_Exhausted and Call = No_Ticket and Phase (Owner, 1) = Retiring);
      -- No real backing object in this metadata-only boundary fixture.
      Reclaim (Owner, Factory, Result);
      pragma Assert (Result = Succeeded and Empty (Owner));
      Stop (Owner, Session, Result);
      pragma Assert (Result = Succeeded);
      Start (Owner, Types, Session, Result);
      pragma Assert (Result = Identity_Exhausted and Session = No_Run);
      pragma Assert (Owner.Session = Serial'Last and Owner.Issued = Serial'Last);
   end Check;
end CCL.Resources.Boundaries;
