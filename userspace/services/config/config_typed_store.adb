with Interfaces;
package body Config_Typed_Store with SPARK_Mode is
   package Catalog renames Config_Collections;
   package Values renames Config_Objects;
   package P renames Config_Worker_Protocol;
   use type Number;
   use type Catalog.Result;
   use type Catalog.Collection_ID;
   use type Values.Outcome;
   use type P.Reply_Kind;
   use type P.Operation;
   use type Values.Phase;
   use type Interfaces.Unsigned_32;

   function Published (Object : State) return Published_Values is
     ([for I in Catalog.Registered_ID =>
        (Value => Values.Visible (Object.Cache (I)), Revision => Values.Revision (Object.Cache (I)))]);

   function Fresh (Object : State; Token : Number) return Boolean is
     (Token > Object.Last_Token and Token /= Number'Last);
   function Ready_In_Session
     (Object : State; ID : Catalog.Registered_ID; Session : Number) return Boolean is
     (Values.Status (Object.Cache (ID)) = Values.Ready and then Values.Session (Object.Cache (ID)) = Session);

   procedure Register
     (Object : in out State; Name : String; Contract : CCL.Objects.Binding;
      ID : out Catalog.Collection_ID; Result : out Catalog.Result)
   is
      Initialized : Boolean;
   begin
      Catalog.Register (Object.Catalog, Name, Contract, ID, Result);
      if Result = Catalog.Registered then
         Values.Initialize (Object.Cache (ID), Contract, Initialized);
         if not Initialized then Result := Catalog.Invalid_Definition; ID := Catalog.No_Collection; end if;
      end if;
   end Register;

   procedure Check_Registration
     (Object : State; Name : String; Contract : CCL.Objects.Binding;
      ID : out Catalog.Collection_ID; Result : out Catalog.Result) is
   begin
      Catalog.Check_Registration (Object.Catalog, Name, Contract, ID, Result);
   end Check_Registration;

   procedure Open
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Config_Authority.Subject_ID; Name : String; Context : Number;
      Requested : Config_Authority.Rights; Expected : CCL.Objects.Schema_Key;
      Handle : out Catalog.Handle; Result : out Catalog.Result) is
   begin
      Catalog.Open (Object.Catalog, Authority, Subject, Name, Context, Requested, Expected, Handle, Result);
   end Open;

   procedure Close
     (Object : in out State; Subject : Config_Authority.Subject_ID;
      Handle : Catalog.Handle; Result : out Catalog.Result) is
   begin
      Catalog.Close (Object.Catalog, Subject, Handle, Result);
   end Close;

   procedure Revoke_Subject (Object : in out State; Subject : Config_Authority.Subject_ID) is
   begin
      Catalog.Revoke_Subject (Object.Catalog, Subject);
   end Revoke_Subject;

   function Check_Access
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Config_Authority.Subject_ID; Handle : Catalog.Handle;
      Operation : Config_Authority.Operation) return Boolean
   is
      ID : Catalog.Collection_ID;
      Result : Catalog.Result;
   begin
      Catalog.Resolve (Object.Catalog, Authority, Subject, Handle, Operation, ID, Result);
      return Result = Catalog.Resolved;
   end Check_Access;

   procedure Get
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Config_Authority.Subject_ID; Handle : Catalog.Handle;
      Value : out CCL.Objects.Image; Revision : out Number;
      Authorized : out Boolean; Result : out Values.Read_Result)
   is
      ID : Catalog.Collection_ID;
      Access_Result : Catalog.Result;
   begin
      Value := (others => <>); Revision := 0; Result := Values.Unavailable;
      Catalog.Resolve (Object.Catalog, Authority, Subject, Handle, Config_Authority.Read_Config, ID, Access_Result);
      Authorized := Access_Result = Catalog.Resolved;
      if Authorized then
         Values.Read (Object.Cache (ID), Catalog.Schema (Object.Catalog, ID), Value, Revision, Result);
      end if;
   end Get;

   --  Snapshot the staged candidate, not the caller's potentially changing
   --  buffer. Only this owned request crosses the storage channel.
   procedure Prepare
     (Object : in out State; ID : Catalog.Registered_ID; Action : P.Operation;
      Token : Number; Result : out Values.Outcome)
   is
      Name : Catalog.Collection_Name;
      Length : Catalog.Name_Length;
      Value : CCL.Objects.Image;
      Revision : Number;
      Found, Available, Built : Boolean;
   begin
      Catalog.Describe (Object.Catalog, ID, Name, Length, Object.Contract, Found);
      if Action = P.Commit then
         Values.Export_Pending (Object.Cache (ID), Value, Revision, Available);
      else
         Revision := 0;
         Value := CCL.Objects.Empty (Object.Contract);
         Available := Found;
      end if;
      if Found and Available then
         P.Make_Request (Action, Values.Session (Object.Cache (ID)), Token, Revision,
           Name (1 .. Length), "machine", Object.Contract, Value, Object.Request, Built);
      else Built := False;
      end if;
      if Built then
         Object.Pending_ID := ID;
         Result := Values.Accepted;
      else
         Values.Lose_Worker (Object.Cache (ID), Values.Session (Object.Cache (ID)));
         Result := Values.Needs_Recovery;
      end if;
   end Prepare;

   procedure Set
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Config_Authority.Subject_ID; Handle : Catalog.Handle;
      Value : CCL.Objects.Image; Expected_Revision, Token : Number;
      Authorized : out Boolean; Result : out Values.Outcome)
   is
      ID : Catalog.Collection_ID;
      Access_Result : Catalog.Result;
   begin
      Result := Values.Rejected;
      Catalog.Resolve (Object.Catalog, Authority, Subject, Handle, Config_Authority.Write_Config, ID, Access_Result);
      Authorized := Access_Result = Catalog.Resolved;
      if not Authorized then return; end if;
      if Object.Pending_ID /= Catalog.No_Collection then Result := Values.Busy; return; end if;
      if not Fresh (Object, Token) then Result := Values.Invalid_Request; return; end if;
      Object.Last_Token := Token;
      Values.Begin_Commit (Object.Cache (ID), Value, Expected_Revision, Token, Result);
      if Result = Values.Accepted then Prepare (Object, ID, P.Commit, Token, Result); end if;
   end Set;

   procedure Restore
     (Object : in out State; ID : Catalog.Registered_ID;
      Session, Token : Number; Result : out Values.Outcome) is
   begin
      if Object.Pending_ID /= Catalog.No_Collection then Result := Values.Busy; return; end if;
      if not Fresh (Object, Token) then Result := Values.Invalid_Request; return; end if;
      Object.Last_Token := Token;
      Values.Attach (Object.Cache (ID), Session, Result);
      if Result /= Values.Accepted then return; end if;
      Values.Begin_Load (Object.Cache (ID), Token, Result);
      if Result = Values.Accepted then Prepare (Object, ID, P.Load, Token, Result); end if;
   end Restore;

   procedure Pending
     (Object : State; Request : out P.Frame;
      Contract : out CCL.Objects.Binding; Available : out Boolean) is
      Empty_Binding : CCL.Objects.Binding;
   begin
      Available := Object.Pending_ID /= Catalog.No_Collection;
      Request := (others => <>); Contract := Empty_Binding;
      if Available then Request := Object.Request; Contract := Object.Contract; end if;
   end Pending;

   function Pending_Session (Object : State) return Number is
     (if Object.Pending_ID = Catalog.No_Collection then 0 else Object.Request.Session);
   function Pending_Token (Object : State) return Number is
     (if Object.Pending_ID = Catalog.No_Collection then 0 else Object.Request.Token);

   procedure Complete
     (Object : in out State; Response : P.Frame; Result : out Values.Outcome)
   is
      Reply : constant P.Frame := Response;
      ID : constant Catalog.Collection_ID := Object.Pending_ID;
   begin
      Result := Values.Ignored;
      if ID = Catalog.No_Collection or else Reply.Token /= Object.Request.Token or else
        Reply.Session /= Object.Request.Session
      then return; end if;
      Object.Pending_ID := Catalog.No_Collection;
      if not P.Valid_Reply (Reply, Object.Request, Object.Contract) then
         Values.Lose_Worker (Object.Cache (ID), Object.Request.Session);
         Result := Values.Needs_Recovery;
         return;
      end if;
      if Object.Request.Action = P.Operation'Enum_Rep (P.Load) then
         Values.Finish_Load (Object.Cache (ID), Reply.Session, Reply.Token,
           (case Reply.Reply is when P.Reply_Kind'Enum_Rep (P.Loaded) => Values.Loaded,
             when P.Reply_Kind'Enum_Rep (P.Absent) => Values.Absent,
             when others => Values.Failed), Reply.Revision, Reply.Value, Result);
      else
         Values.Finish_Commit (Object.Cache (ID), Reply.Session, Reply.Token,
           (case Reply.Reply is when P.Reply_Kind'Enum_Rep (P.Committed) => Values.Committed,
             when P.Reply_Kind'Enum_Rep (P.Rejected) => Values.Definitely_Rejected,
             when P.Reply_Kind'Enum_Rep (P.Conflict) => Values.Conflicted,
             when others => Values.Indeterminate), Reply.Revision, Result);
      end if;
   end Complete;

   procedure Worker_Lost (Object : in out State; Session : Number) is
   begin
      for Item of Object.Cache loop Values.Lose_Worker (Item, Session); end loop;
      if Object.Pending_ID /= Catalog.No_Collection and then Object.Request.Session = Session then
         Object.Pending_ID := Catalog.No_Collection;
      end if;
   end Worker_Lost;
end Config_Typed_Store;
