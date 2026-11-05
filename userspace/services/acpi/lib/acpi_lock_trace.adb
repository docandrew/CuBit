pragma Ada_2022;
package body ACPI_Lock_Trace with SPARK_Mode is
   use type Word, P.Operation, P.Reply_Kind;
   procedure Append (State : in out Context) is
   begin
      if State.Finished then return; end if;
      State.Items (State.Next) := State.Current;
      if State.Used < Maximum_Records then State.Used := State.Used+1;
      elsif State.Lost < Word'Last then State.Lost := State.Lost+1; end if;
      State.Next := (if State.Next = Index'Last then Index'First else State.Next+1);
   end Append;
   procedure Begin_Request (State : in out Context; Caller : Principal; Scope : Scope_Tag; Kind : P.Operation) is
   begin
      State.Active := not State.Finished and then Scope = Allowed_Scope and then Caller /= No_Principal
        and then (State.Owner = No_Principal or else State.Owner = Caller);
      State.Trusted_Request := State.Active; State.Caller := Caller;
      State.Current := (Request => Kind,others => <>);
   end Begin_Request;
   procedure Finish_Request (State : in out Context; Reply : P.Reply) is
   begin
      if not State.Active or else not State.Trusted_Request then return; end if;
      if State.Owner = No_Principal then
         if State.Current.Request /= P.Prepare or else not Reply.Valid or else Reply.Kind /= P.Ready then
            State.Active := False; return;
         end if;
         State.Owner := State.Caller;
      end if;
      State.Current.Reply_Valid := Reply.Valid; State.Current.Reply := Reply.Kind;
      State.Current.Outcome := Reply.Outcome; Append (State); State.Active := False;
   end Finish_Request;
   procedure Begin_Maintenance (State : in out Context) is
   begin
      State.Active := not State.Finished and then State.Owner /= No_Principal;
      State.Trusted_Request := False; State.Current := (Maintenance => True,others => <>);
   end Begin_Maintenance;
   procedure Finish_Maintenance (State : in out Context; Outcome : P.Lock_Outcome) is
   begin
      if not State.Active or else State.Trusted_Request then return; end if;
      if State.Current.Read_Present or State.Current.CAS_Present or State.Current.Signal /= No_Signal then
         State.Current.Reply_Valid := True; State.Current.Reply := P.Recorded;
         State.Current.Outcome := Outcome; Append (State);
      end if;
      State.Active := False;
   end Finish_Maintenance;
   procedure Observe_Read (State : in out Context; Value : Interfaces.Unsigned_32; Available : Boolean) is
   begin if State.Active then State.Current.Read_Present := True; State.Current.Read_Available := Available; State.Current.Read_Word := Value; end if; end Observe_Read;
   procedure Observe_CAS (State : in out Context; Expected,Replacement,Observed : Interfaces.Unsigned_32; Known,Attempted,Exchanged : Boolean) is
   begin
      if not State.Active then return; end if;
      State.Current.CAS_Present := True; State.Current.Expected := Expected; State.Current.Replacement := Replacement;
      State.Current.Observed := Observed; State.Current.CAS_Known := Known;
      State.Current.CAS_Attempted := Attempted; State.Current.CAS_Exchanged := Exchanged;
      if State.Current.CAS_Count < Word'Last then State.Current.CAS_Count := State.Current.CAS_Count+1; end if;
   end Observe_CAS;
   procedure Observe_Signal (State : in out Context; Result : Signal_Result; Reads,Writes : Natural) is
   begin
      if not State.Active then return; end if;
      State.Current.Signal := Result;
      State.Current.Signal_Reads := Natural'Min (Reads,2); State.Current.Signal_Writes := Natural'Min (Writes,2);
   end Observe_Signal;
   procedure Request_Dump (State : in out Context; Caller : Principal; Scope : Scope_Tag; Result : out Dump_Outcome) is
   begin
      if Caller = No_Principal or else State.Owner = No_Principal or else State.Owner /= Caller or else Scope /= Allowed_Scope then Result := Unauthorized;
      elsif State.Dumped then Result := Already_Dumped;
      else State.Dumped := True; State.Dump_Pending := True; Result := Accepted; end if;
   end Request_Dump;
   function Pending (State : Context) return Boolean is (State.Dump_Pending and not State.Finished);
   function Length (State : Context) return Count is (State.Used);
   function Dropped (State : Context) return Word is (State.Lost);
   procedure Read (State : Context; Number : Index; Item : out Snapshot; Found : out Boolean) is
      First,At_Index : Index;
   begin
      Found := Number <= State.Used; Item := (others => <>); if not Found then return; end if;
      First := (if State.Used < Maximum_Records then 1 else State.Next);
      At_Index := Index ((Natural (First)-1+Natural (Number)-1) mod Maximum_Records+1);
      Item := State.Items (At_Index);
   end Read;
   procedure Finish_Dump (State : in out Context) is
   begin
      if State.Dump_Pending then State.Finished := True; State.Dump_Pending := False; State.Active := False; end if;
   end Finish_Dump;
end ACPI_Lock_Trace;
