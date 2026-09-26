package body Config_Object_Client.Resources is
   package R renames CCL.Resources;
   package T renames CCL.Types;
   package W renames Config_Object_Messages;
   use type R.Outcome;
   use type R.Reference;
   use type R.Ticket;
   use type T.Shape;
   use type W.Operation;
   use type W.Status;
   use type Number;

   function State (Object : Collection) return Lifetime is (Object.Current);
   function Reference_Of (Object : Collection; Owner : R.Registry) return R.Reference is
     (if Object.Current in Available | Calling and then R.Current (Owner, Object.Reference)
      then Object.Reference else R.No_Reference);
   function Contract_Of (Object : Collection) return CCL.Objects.Binding is (Object.Backend.Contract);
   function Belongs (Object : Collection; Owner : R.Registry) return Boolean is
     (Object.Lease /= R.No_Ticket and then R.Position_Of (Owner, Object.Lease) /= 0);

   procedure Create
     (Object : in out Collection; Owner : in out R.Registry;
      Session : R.Run; Kind : T.Type_Reference;
      Endpoint : CuBit.Messages.CapabilitySlot; Name : String;
      Contract : CCL.Objects.Binding; Access_Rights : W.Access_Mode;
      Context, Token : Number; Result : out Submission)
   is
      Types : constant T.Registry := R.Declared_Types (Owner);
      Definition : constant T.Description := T.Describe (Types, Kind);
      Outcome : R.Outcome;
      Good : Boolean;
      Ignored : R.Reference;
   begin
      Result := Busy;
      if Object.Current /= Vacant then return; end if;
      Result := Invalid_Request;
      if not CuBit.Async_Requests.Can_Reserve (Object.Backend.Request, Token) or else
        Definition.Form /= T.Resource or else Definition.Count /= 1 or else
        not CCL.Objects.Matches_Type (Contract, Types, Definition.Parts (1).Payload)
      then return; end if;
      R.Reserve (Owner, Session, Kind, Object.Lease, Outcome);
      if Outcome /= R.Succeeded then
         if Outcome in R.Busy | R.Capacity_Exhausted then Result := Busy; end if;
         return;
      end if;
      Object.Kind := Kind; Object.Call := Object.Lease;
      Object.Reference := R.No_Reference; Object.Unknown_Resource := False;
      Object.Current := Acquiring;
      Config_Object_Client.Initialize (Object.Backend, Endpoint, Good);
      Result := Unavailable;
      if Good then
         Config_Object_Client.Create (Object.Backend, Name, Contract, Access_Rights, Context, Token, Result);
      end if;
      if Result /= Submitted then
         R.Publish (Owner, Object.Call, False, Ignored, Outcome);
         Object.Call := R.No_Ticket; Object.Current := Draining;
      end if;
   end Create;

   procedure Begin_Call
     (Object : in out Collection; Owner : in out R.Registry;
      Reference : R.Reference; Result : out Submission)
   is
      Outcome : R.Outcome;
   begin
      Result := Invalid_Request;
      if Reference = R.No_Reference or else Reference /= Object.Reference or else
        not Belongs (Object, Owner) or else not R.Current (Owner, Reference)
      then return; end if;
      if Object.Current /= Available then Result := Busy; return; end if;
      R.Begin_Use (Owner, Reference, Object.Kind, Object.Call, Outcome);
      if Outcome = R.Succeeded then Result := Submitted;
      elsif Outcome = R.Busy then Result := Busy; end if;
   end Begin_Call;

   procedure Finish_Submission
     (Object : in out Collection; Owner : in out R.Registry; Sent : Submission)
   is
      Outcome : R.Outcome;
   begin
      if Sent = Submitted then Object.Current := Calling;
      else
         R.Finish_Use (Owner, Object.Call, True, Outcome);
         Object.Call := R.No_Ticket;
      end if;
   end Finish_Submission;

   procedure Get
     (Object : in out Collection; Owner : in out R.Registry;
      Reference : R.Reference; Token : Number; Result : out Submission) is
   begin
      Begin_Call (Object, Owner, Reference, Result); if Result /= Submitted then return; end if;
      Config_Object_Client.Get (Object.Backend, Token, Result);
      Finish_Submission (Object, Owner, Result);
   end Get;

   procedure Set
     (Object : in out Collection; Owner : in out R.Registry;
      Reference : R.Reference; Value : CCL.Objects.Image;
      Expected_Revision, Token : Number; Result : out Submission) is
   begin
      Begin_Call (Object, Owner, Reference, Result); if Result /= Submitted then return; end if;
      Config_Object_Client.Set (Object.Backend, Value, Expected_Revision, Token, Result);
      Finish_Submission (Object, Owner, Result);
   end Set;

   procedure Close
     (Object : in out Collection; Owner : in out R.Registry;
      Reference : R.Reference; Token : Number; Result : out Submission) is
   begin
      Begin_Call (Object, Owner, Reference, Result); if Result /= Submitted then return; end if;
      Config_Object_Client.Close (Object.Backend, Token, Result);
      Finish_Submission (Object, Owner, Result);
   end Close;

   procedure Complete
     (Object : in out Collection; Owner : R.Registry;
      Completion : CuBit.Messages.CompletionEntry; Result : out Completion_Result) is
   begin
      Result := Ignored;
      if Belongs (Object, Owner) and then R.Valid_Ticket (Owner, Object.Call) then
         Config_Object_Client.Complete (Object.Backend, Completion, Result);
      end if;
   end Complete;

   procedure Take_Result
     (Object : in out Collection; Owner : in out R.Registry;
      Item : out Response; Taken : out Boolean)
   is
      Outcome : R.Outcome;
      Keep : Boolean;
   begin
      Item := (others => <>); Taken := False;
      if not Belongs (Object, Owner) or else not R.Valid_Ticket (Owner, Object.Call) then return; end if;
      Config_Object_Client.Take_Result (Object.Backend, Item, Taken);
      if not Taken then return; end if;
      if Object.Current = Acquiring then
         Object.Unknown_Resource := not Item.Valid or else Item.Code = W.Uncertain;
         R.Publish (Owner, Object.Call, Item.Valid and then Item.Code = W.Success,
           Object.Reference, Outcome);
         Object.Current := (if Outcome = R.Succeeded then Available else Draining);
      else
         Keep := Object.Current = Calling and then Object.Backend.Action /= W.Close_Collection and then
           Item.Valid and then Item.Code /= W.Uncertain;
         if Object.Backend.Action = W.Close_Collection and then
           (not Item.Valid or else Item.Code = W.Uncertain)
         then Object.Unknown_Resource := True; end if;
         R.Finish_Use (Owner, Object.Call, Keep, Outcome);
         Object.Current := (if Keep and then R.Current (Owner, Object.Reference) then Available else Draining);
      end if;
      Object.Call := R.No_Ticket;
   end Take_Result;

   procedure Retire (Object : in out Collection; Owner : in out R.Registry) is
      Outcome : R.Outcome;
   begin
      if not Belongs (Object, Owner) then return; end if;
      -- During acquisition there is no public reference yet. Stop is recorded
      -- on the reserved lease so a successful late acquisition cannot publish.
      R.Retire_Lease (Owner, Object.Lease, Outcome);
      if Object.Current = Available then Object.Current := Draining; end if;
   end Retire;

   procedure Cleanup
     (Object : in out Collection; Owner : in out R.Registry;
      Token : Number; Result : out Cleanup_Result)
   is
      Outcome : R.Outcome;
      Sent : Submission;
      Confirmed : Boolean;
   begin
      if Object.Current = Vacant then Result := Released; return; end if;
      Result := Invalid_Owner;
      if not Belongs (Object, Owner) then return; end if;
      Retire (Object, Owner);
      if Object.Backend.Current in Waiting | Result_Ready then Result := Completion_Pending; return; end if;
      if Object.Unknown_Resource then
         Config_Object_Client.Retire (Object.Backend, Confirmed);
         Object.Current := Quarantined; Result := Quarantine_Required; return;
      end if;
      if Object.Backend.Handle /= 0 then
         if not CuBit.Async_Requests.Can_Reserve (Object.Backend.Request, Token) then Result := Invalid_Token; return; end if;
         R.Begin_Cleanup (Owner, Object.Lease, Object.Call, Outcome);
         if Outcome /= R.Succeeded then Result := Completion_Pending; return; end if;
         -- Cleanup alone may close a known handle after an uncertain Get/Set.
         -- No failed data operation is retried or made script-accessible.
         Config_Object_Client.Start (Object.Backend, W.Close_Collection, Token, 0, Sent);
         if Sent = Submitted then Object.Current := Closing; Result := Close_Submitted;
         else
            R.Finish_Use (Owner, Object.Call, False, Outcome);
            Object.Call := R.No_Ticket; Result := Not_Submitted;
         end if;
         return;
      end if;
      Object.Current := Retiring;
      Config_Object_Client.Retire (Object.Backend, Confirmed);
      if not Confirmed then Result := Grant_Pending; return; end if;
      R.Reclaim (Owner, Object.Lease, Outcome);
      if Outcome /= R.Succeeded then Result := Completion_Pending; return; end if;
      -- The only rearm path: no remote handle, no outstanding call, confirmed
      -- grant retirement, and old registry lease reclaimed. Preserve Last_Token.
      Object.Backend.Current := Fresh;
      Object.Backend.Revocation_Requested := False;
      Object.Backend.Poisoned := False;
      Object.Lease := R.No_Ticket; Object.Call := R.No_Ticket;
      Object.Reference := R.No_Reference; Object.Kind := T.Invalid_Type;
      Object.Current := Vacant; Result := Released;
   end Cleanup;
end Config_Object_Client.Resources;
