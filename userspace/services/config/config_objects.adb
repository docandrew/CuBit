package body Config_Objects with SPARK_Mode is
   use type CCL.Objects.Schema_Key;
   function Other (Index : Buffer_Index) return Buffer_Index is
     (if Index = First_Buffer then Second_Buffer else First_Buffer);
   function Status (Object : State) return Phase is (Object.Current_Phase);
   function Revision (Object : State) return Number is (Object.Current_Revision);
   function Session (Object : State) return Number is (Object.Worker_Session);
   function Pending_Request (Object : State) return Number is (Object.Active_Request);
   function Visible (Object : State) return CCL.Objects.Image is (Object.Data (Object.Active));
   function Candidate (Object : State) return CCL.Objects.Image is (Object.Data (Other (Object.Active)));
   function Valid_Candidate (Object : State) return Boolean is
     (Object.Current_Phase = Committing and then
      CCL.Objects.Validate (Object.Data (Other (Object.Active)), Object.Contract));
   function Fresh_Request (Object : State; Request : Number) return Boolean is
     (Request /= Number'Last and Request > Object.Last_Request);
   function Matching (Object : State; From_Session, Request : Number; Expected : Phase) return Boolean is
     (Object.Current_Phase = Expected and From_Session /= 0 and
      From_Session = Object.Worker_Session and Request /= 0 and Request = Object.Active_Request);

   procedure Initialize
     (Object : in out State; Contract : CCL.Objects.Binding; Accepted : out Boolean) is
   begin
      Accepted := Object.Current_Phase = Unbound and CCL.Objects.Is_Bound (Contract);
      if Accepted then Object.Contract := Contract; Object.Current_Phase := Detached; end if;
   end Initialize;

   procedure Attach (Object : in out State; New_Session : Number; Result : out Outcome) is
   begin
      if Object.Current_Phase = Unbound then Result := Not_Bound;
      elsif Object.Current_Phase not in Detached | Recovery_Required then Result := Busy;
      elsif New_Session = 0 or New_Session <= Object.Worker_Session then Result := Invalid_Request;
      else
         Object.Worker_Session := New_Session;
         Object.Active_Request := 0;
         Object.Current_Phase := Recovering;
         Result := Accepted;
      end if;
   end Attach;

   procedure Begin_Load (Object : in out State; Request : Number; Result : out Outcome) is
   begin
      if Object.Current_Phase = Unbound then Result := Not_Bound;
      elsif Object.Current_Phase /= Recovering then Result := Needs_Recovery;
      elsif not Fresh_Request (Object, Request) then Result := Invalid_Request;
      else
         Object.Last_Request := Request;
         Object.Active_Request := Request;
         Object.Current_Phase := Loading;
         Result := Accepted;
      end if;
   end Begin_Load;

   procedure Finish_Load
     (Object : in out State; From_Session, Request : Number; Completion : Load_Outcome;
      Saved_Revision : Number; Value : CCL.Objects.Image; Result : out Outcome) is
      Target : constant Buffer_Index := Other (Object.Active);
   begin
      if not Matching (Object, From_Session, Request, Loading) then Result := Ignored; return; end if;
      Object.Active_Request := 0;
      Object.Current_Phase := Recovery_Required;
      Result := Needs_Recovery;
      case Completion is
         when Failed => return;
         when Absent =>
            if Saved_Revision /= 0 or Object.Current_Revision /= 0 then return; end if;
            Object.Current_Phase := Ready;
            Result := Accepted;
         when Loaded =>
            if Saved_Revision not in 1 .. Maximum_Revision or else
              Saved_Revision < Object.Current_Revision then return; end if;
            Object.Data (Target) := Value;
            if not CCL.Objects.Validate (Object.Data (Target), Object.Contract) then return; end if;
            if Saved_Revision = Object.Current_Revision and then
              Object.Data (Target) /= Object.Data (Object.Active) then return; end if;
            Object.Active := Target;
            Object.Current_Revision := Saved_Revision;
            Object.Current_Phase := Ready;
            Result := Published;
      end case;
   end Finish_Load;

   procedure Begin_Commit
     (Object : in out State; Value : CCL.Objects.Image;
      Expected_Revision, Request : Number; Result : out Outcome)
   is
      Candidate : constant Buffer_Index :=
        (if Object.Active = First_Buffer then Second_Buffer else First_Buffer);
   begin
      if Object.Current_Phase = Unbound then Result := Not_Bound; return; end if;
      if Object.Current_Phase in Loading | Committing then Result := Busy; return; end if;
      if Object.Current_Phase /= Ready then Result := Needs_Recovery; return; end if;
      if Expected_Revision /= Object.Current_Revision then Result := Revision_Conflict; return; end if;
      if Object.Current_Revision = Maximum_Revision then Result := Revision_Exhausted; return; end if;
      if not Fresh_Request (Object, Request) then Result := Invalid_Request; return; end if;
      --  Snapshot before validation. Even a sender-writable source can only
      --  influence these copied bytes, never mutate our validated candidate.
      Object.Data (Candidate) := Value;
      if not CCL.Objects.Validate (Object.Data (Candidate), Object.Contract) then
         Result := Invalid_Value;
         return;
      end if;
      Object.Last_Request := Request;
      Object.Active_Request := Request;
      Object.Current_Phase := Committing;
      Result := Accepted;
   end Begin_Commit;

   procedure Export_Pending
     (Object : State; Value : out CCL.Objects.Image; Expected_Revision : out Number;
      Available : out Boolean) is
   begin
      Available := Object.Current_Phase = Committing;
      if Available then Value := Object.Data (Other (Object.Active)); Expected_Revision := Object.Current_Revision;
      else Value := (others => <>); Expected_Revision := 0;
      end if;
   end Export_Pending;

   procedure Finish_Commit
     (Object : in out State; From_Session, Request : Number; Completion : Commit_Outcome;
      Saved_Revision : Number; Result : out Outcome) is
   begin
      if not Matching (Object, From_Session, Request, Committing) then Result := Ignored; return; end if;
      Object.Active_Request := 0;
      Object.Current_Phase := Recovery_Required;
      Result := Needs_Recovery;
      case Completion is
         when Committed =>
            if Object.Current_Revision = Maximum_Revision or else
              Saved_Revision /= Object.Current_Revision + 1 then return; end if;
            Object.Active := Other (Object.Active);
            Object.Current_Revision := Saved_Revision;
            Object.Current_Phase := Ready;
            Result := Published;
         when Definitely_Rejected =>
            if Saved_Revision /= Object.Current_Revision then return; end if;
            Object.Current_Phase := Ready;
            Result := Rejected;
         when Conflicted | Indeterminate => null;
      end case;
   end Finish_Commit;

   procedure Lose_Worker (Object : in out State; From_Session : Number) is
   begin
      if From_Session /= 0 and From_Session = Object.Worker_Session and
        Object.Current_Phase not in Unbound | Detached
      then
         Object.Current_Phase := Recovery_Required;
         Object.Active_Request := 0;
      end if;
   end Lose_Worker;

   procedure Read
     (Object : State; Expected_Schema : CCL.Objects.Schema_Key;
      Value : out CCL.Objects.Image; Saved_Revision : out Number;
      Result : out Read_Result) is
   begin
      if not CCL.Objects.Is_Bound (Object.Contract) then
         Result := Unavailable;
      elsif Expected_Schema /= CCL.Objects.Identity (Object.Contract) then
         Result := Schema_Mismatch;
      elsif Object.Current_Revision > 0 then
         Result := (if Object.Current_Phase in Ready | Committing then Found else Stale);
      elsif Object.Current_Phase in Ready | Committing then Result := Missing;
      else Result := Unavailable;
      end if;
      if Result in Found | Stale then
         Value := Object.Data (Object.Active);
         Saved_Revision := Object.Current_Revision;
      else
         Value := (others => <>);
         Saved_Revision := 0;
      end if;
   end Read;
end Config_Objects;
