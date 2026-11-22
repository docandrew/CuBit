with CCL.Types.Correspondence;

package body CCL.Resources with SPARK_Mode is
   use type CCL.Types.Shape;

   function Matches_Type
     (Owner : Registry; Item : Reference; Types : CCL.Types.Registry;
      Expected : CCL.Types.Type_Reference) return Boolean is
     (Current (Owner, Item) and then Expected /= CCL.Types.Invalid_Type and then
      CCL.Types.Correspondence.Resolve (Owner.Types, Type_Of (Owner, Item), Types) = Expected);

   procedure Start
     (Owner : in out Registry; Types : CCL.Types.Registry;
      Session : out Run; Result : out Outcome) is
   begin
      Session := No_Run;
      if Owner.Running or else not Empty (Owner) then Result := Busy;
      elsif Owner.Session = Serial'Last then Result := Identity_Exhausted;
      else
         Owner.Session := Owner.Session + 1;
         Owner.Types := Types;
         Owner.Running := True;
         Session := (Owner.Context, Owner.Session);
         Result := Succeeded;
      end if;
   end Start;

   procedure Stop (Owner : in out Registry; Session : Run; Result : out Outcome) is
   begin
      if not Active (Owner, Session) then Result := Stale_Run; return; end if;
      Owner.Running := False;
      for I in Owner.Entries'Range loop
         if Owner.Entries (I).Current /= Vacant then Owner.Entries (I).Current := Retiring; end if;
         pragma Loop_Invariant
           (for all J in Occupied_Slot'First .. I => Phase (Owner, J) in Vacant | Retiring);
      end loop;
      Result := Succeeded;
   end Stop;

   procedure Reserve
     (Owner : in out Registry; Session : Run; Kind : CCL.Types.Type_Reference;
      Call : out Ticket; Result : out Outcome) is
   begin
      Call := No_Ticket;
      if not Active (Owner, Session) then Result := Stale_Run; return; end if;
      if not CCL.Types.Known (Owner.Types, Kind) or else
        CCL.Types.Describe (Owner.Types, Kind).Form /= CCL.Types.Resource
      then Result := Invalid_Type; return; end if;
      if Owner.Issued = Serial'Last then Result := Identity_Exhausted; return; end if;
      for I in Owner.Entries'Range loop
         if Owner.Entries (I).Current = Vacant then
            Owner.Issued := Owner.Issued + 1;
            Owner.Entries (I) := (Acquiring, Owner.Issued, Owner.Issued, Kind, Factory);
            Call := ((Session, I, Owner.Issued), Owner.Issued, Factory);
            Result := Succeeded;
            return;
         end if;
      end loop;
      Result := Capacity_Exhausted;
   end Reserve;

   procedure Publish
     (Owner : in out Registry; Call : Ticket; Acquired : Boolean;
      Item : out Reference; Result : out Outcome) is
   begin
      Item := No_Reference;
      if not Valid_Ticket (Owner, Call) or else Call.Kind /= Factory then
         Result := Stale_Ticket; return;
      end if;
      declare
         E : Resource_Entry renames Owner.Entries (Call.Item.Position);
      begin
         E.Call_Number := 0;
         if Acquired and Active (Owner, Call.Item.Session) and E.Current = Acquiring then
            E.Current := Available; Item := Call.Item; Result := Succeeded;
         else
            E.Current := Retiring; Result := Not_Ready;
         end if;
      end;
   end Publish;

   procedure Begin_Use
     (Owner : in out Registry; Item : Reference; Expected : CCL.Types.Type_Reference;
      Call : out Ticket; Result : out Outcome) is
   begin
      Call := No_Ticket;
      if not Current (Owner, Item) then Result := Stale_Reference; return; end if;
      declare
         E : Resource_Entry renames Owner.Entries (Item.Position);
      begin
         if E.Kind /= Expected then Result := Wrong_Type;
         elsif E.Current /= Available then Result := Busy;
         elsif Owner.Issued = Serial'Last then Result := Identity_Exhausted;
         else
            Owner.Issued := Owner.Issued + 1;
            E.Current := In_Flight; E.Call_Number := Owner.Issued; E.Call_Type := Operation;
            Call := (Item, Owner.Issued, Operation); Result := Succeeded;
         end if;
      end;
   end Begin_Use;

   procedure Finish_Use
     (Owner : in out Registry; Call : Ticket; Keep : Boolean; Result : out Outcome) is
   begin
      if not Valid_Ticket (Owner, Call) or else Call.Kind /= Operation then
         Result := Stale_Ticket; return;
      end if;
      declare
         E : Resource_Entry renames Owner.Entries (Call.Item.Position);
      begin
         E.Call_Number := 0;
         E.Current := (if Keep and Owner.Running and E.Current = In_Flight then Available else Retiring);
      end;
      Result := Succeeded;
   end Finish_Use;

   procedure Begin_Cleanup
     (Owner : in out Registry; Lease : Ticket; Call : out Ticket; Result : out Outcome) is
   begin
      Call := No_Ticket;
      if not Same_Lease (Owner, Lease.Item) then Result := Stale_Ticket; return; end if;
      declare
         E : Resource_Entry renames Owner.Entries (Lease.Item.Position);
      begin
         if E.Current /= Retiring or else E.Call_Number /= 0 then Result := Not_Ready;
         elsif Owner.Issued = Serial'Last then Result := Identity_Exhausted;
         else
            Owner.Issued := Owner.Issued + 1;
            E.Call_Number := Owner.Issued; E.Call_Type := Operation;
            Call := (Lease.Item, Owner.Issued, Operation); Result := Succeeded;
         end if;
      end;
   end Begin_Cleanup;

   procedure Retire (Owner : in out Registry; Item : Reference; Result : out Outcome) is
   begin
      if not Same_Lease (Owner, Item) then Result := Stale_Reference; return; end if;
      Owner.Entries (Item.Position).Current := Retiring;
      Result := Succeeded;
   end Retire;

   procedure Retire_Lease (Owner : in out Registry; Lease : Ticket; Result : out Outcome) is
      Position : constant Slot := Position_Of (Owner, Lease);
   begin
      if Position = 0 then Result := Stale_Ticket; return; end if;
      Owner.Entries (Position).Current := Retiring;
      Result := Succeeded;
   end Retire_Lease;

   procedure Reclaim
     (Owner : in out Registry; Call : Ticket; Result : out Outcome) is
   begin
      if not Same_Lease (Owner, Call.Item) then Result := Stale_Ticket; return; end if;
      if Phase (Owner, Call.Item.Position) /= Retiring or else Pending (Owner, Call.Item.Position) then
         Result := Not_Ready; return;
      end if;
      Owner.Entries (Call.Item.Position) := (others => <>);
      Result := Succeeded;
   end Reclaim;
end CCL.Resources;
