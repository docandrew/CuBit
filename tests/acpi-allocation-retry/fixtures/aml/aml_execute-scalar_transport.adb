package body AML_Execute.Scalar_Transport with SPARK_Mode is
   use type AML_Frame_Handles.Invocation_Domain;
   use type Ledger.Handle;
   use type Ledger.Status;
   use type Ledger.Integer_Origin;
   use type AML_Decode.Integer_Origin;
   use type AML_Decode.Integer_Width;
   use type AML_References.Object_Handle;
   function Empty (Domain : AML_Frame_Handles.Invocation_Domain) return State is
     (Scalars => Ledger.Empty (Domain), Last_Stamp => No_Stamp, Slots => [others => <>]);
   function Issued_Stamps (Store : State) return Natural is
     (Natural (Store.Last_Stamp));
   function Initialized
     (Store : State; Domain : AML_Frame_Handles.Invocation_Domain) return Boolean
   is
   begin
      if not Ledger.Initialized (Store.Scalars, Domain) or else Store.Last_Stamp /= No_Stamp then return False; end if;
      for Item of Store.Slots loop
         if Item /= Envelope'(others => <>) then return False; end if;
      end loop;
      return True;
   end Initialized;
   procedure Initialize
     (Store : out State; Domain : AML_Frame_Handles.Invocation_Domain)
   is
   begin
      Ledger.Initialize (Store.Scalars, Domain);
      Store.Last_Stamp := No_Stamp;
      for Index in Slot loop Store.Slots (Index) := (others => <>); end loop;
   end Initialize;
   function Current_Slot_Identity (Store : State; At_Slot : Slot) return Slot_Identity is
     (if Store.Slots (At_Slot).Present
        and then AML_Frame_Handles.Has_Authority (Ledger.Invocation_Domain (Store.Scalars))
      then (Ledger.Invocation_Domain (Store.Scalars), At_Slot, Store.Slots (At_Slot).Stamp)
      else No_Slot_Identity);
   function Matches_Slot (Store : State; Item : Slot_Identity) return Boolean is
     (Item.At_Slot /= 0 and then Item.Stamp /= No_Stamp
      and then AML_Frame_Handles.Has_Authority (Item.Domain)
      and then Item.Domain = Ledger.Invocation_Domain (Store.Scalars)
      and then Store.Slots (Item.At_Slot).Present
      and then Store.Slots (Item.At_Slot).Stamp = Item.Stamp);
   function Position (Item : Slot_Identity) return Slot_Locator is (Item.At_Slot);
   function Occupied (Store : State; At_Slot : Slot) return Boolean is
     (Store.Slots (At_Slot).Present);
   function Kind (Store : State; At_Slot : Slot) return Identity_Kind is
     (Store.Slots (At_Slot).Tag);
   function Source (Store : State; At_Slot : Slot) return AML_References.Object_Handle is
     (Store.Slots (At_Slot).Owned);
   function Temporary_Roots (Store : State) return Natural is
     (Ledger.Roots (Store.Scalars));
   function Valid (Store : State) return Boolean is
      Count : Natural := 0;
      Seen : array (Positive range 1 .. Slot_Capacity + Staging_Root_Count) of Boolean := [others => False];
      Position : Natural;
   begin
      if not Ledger.Valid (Store.Scalars)
        or else Ledger.Objects (Store.Scalars) > Object_Capacity
      then return False; end if;
      for Item of Store.Slots loop
         if Item.Present then
            if Item.Stamp = No_Stamp or else Item.Stamp > Store.Last_Stamp then return False; end if;
         elsif Item.Stamp /= No_Stamp then return False;
         end if;
         if not Item.Present then
            if Item.Tag /= Non_Scalar or else Item.Lease /= Ledger.No_Handle
              or else Item.Owned /= AML_References.No_Object_Handle
            then return False; end if;
         elsif Item.Tag = Temporary_Scalar then
            if Item.Payload.Value_Kind /= Integer_Datum
              or else not Ledger.Live (Store.Scalars, Item.Lease)
              or else Item.Owned /= AML_References.No_Object_Handle
            then return False; end if;
            Position := Ledger.Lease_Position (Store.Scalars, Item.Lease);
            if Position not in Seen'Range or else Seen (Position) then return False; end if;
            Seen (Position) := True;
            Count := Count + 1;
         elsif Item.Tag = Owned_Scalar then
            if Item.Payload.Value_Kind /= Integer_Datum
              or else Item.Lease /= Ledger.No_Handle
              or else Item.Owned = AML_References.No_Object_Handle
            then return False; end if;
         elsif Item.Payload.Value_Kind = Integer_Datum
           or else Item.Lease /= Ledger.No_Handle
           or else Item.Owned /= AML_References.No_Object_Handle
         then return False;
         end if;
      end loop;
      return Count = Ledger.Roots (Store.Scalars);
   end Valid;
   function Snapshot (Store : State; At_Slot : Slot) return Datum is
      Item : constant Envelope := Store.Slots (At_Slot);
   begin
      if Item.Tag = Temporary_Scalar then
         declare Value : constant Ledger.Reading := Ledger.Read (Store.Scalars, Item.Lease); begin
            return (Integer_Datum, Value.Value,
              (if Value.Origin = Ledger.AML_Constant then AML_Decode.AML_Constant
               else AML_Decode.Ordinary_Integer));
         end;
      end if;
      return Item.Payload;
   end Snapshot;
   function Same_Identity (Store : State; Left, Right : Slot) return Boolean is
     (Store.Slots (Left).Present and then Store.Slots (Right).Present
      and then Store.Slots (Left).Tag = Store.Slots (Right).Tag
      and then (case Store.Slots (Left).Tag is
        when Non_Scalar => False,
        when Temporary_Scalar => Ledger.Same_Object
          (Store.Scalars, Store.Slots (Left).Lease, Store.Slots (Right).Lease),
        when Owned_Scalar => Store.Slots (Left).Owned = Store.Slots (Right).Owned));
   function Temporary_Owners (Store : State; At_Slot : Slot) return Natural is
     (if Store.Slots (At_Slot).Tag = Temporary_Scalar then
        Ledger.Owners (Store.Scalars, Store.Slots (At_Slot).Lease) else 0);
   procedure Other_Owned_Lease
     (Store : State; Item : Slot_Identity; Shared : out Boolean;
      Status : out Outcome)
   is
   begin
      Shared := False;
      Status := Invalid_Source;
      if not Matches_Slot (Store, Item)
        or else Kind (Store, Position (Item)) /= Owned_Scalar
      then return; end if;
      Status := Ready;
      for Index in Slot loop
         if Index /= Position (Item) and then Occupied (Store, Index)
           and then Kind (Store, Index) = Owned_Scalar
           and then Same_Identity (Store, Position (Item), Index)
         then Shared := True; return; end if;
      end loop;
   end Other_Owned_Lease;
   function Compound_Identity_Matches
     (Store : State; At_Slot : Slot; Value : Object_Value) return Boolean
   is
      String_Type_Code : constant Natural := 2;
      Package_Type_Code : constant Natural := 4;
   begin
      return Store.Slots (At_Slot).Present
        and then Store.Slots (At_Slot).Tag = Non_Scalar
        and then Store.Slots (At_Slot).Payload.Value_Kind = Object_Datum
        and then Value.Source /= AML_References.No_Object_Handle
        and then Value.ID = AML_References.Source (Value.Source)
        and then Value.Type_Code in String_Type_Code .. Package_Type_Code
        and then Store.Slots (At_Slot).Payload.Object.Source = Value.Source
        and then Store.Slots (At_Slot).Payload.Object.ID = Value.ID
        and then Store.Slots (At_Slot).Payload.Object.Type_Code = Value.Type_Code;
   end Compound_Identity_Matches;
   function Compound_Cache_Updated
     (Store, Prior : State; At_Slot : Slot; Value : Object_Value) return Boolean
   is
      Expected : State := Prior;
   begin
      Expected.Slots (At_Slot).Payload := (Object_Datum, Value);
      return Store = Expected;
   end Compound_Cache_Updated;
   procedure Refresh_Compound_Cache
     (Store : in out State; At_Slot : Slot; Value : Object_Value;
      Status : out Outcome)
   is
   begin
      Status := Invalid_Source;
      if not Compound_Identity_Matches (Store, At_Slot, Value) then return; end if;
      Store.Slots (At_Slot).Payload := (Object_Datum, Value);
      Status := Ready;
   end Refresh_Compound_Cache;
   procedure Clear_Slot (Store : in out State; At_Slot : Slot) is
      Status : Ledger.Status;
   begin
      if Store.Slots (At_Slot).Tag = Temporary_Scalar then
         Ledger.Drop (Store.Scalars, Store.Slots (At_Slot).Lease, Status);
         pragma Assert (Status = Ledger.Ready);
      end if;
      Store.Slots (At_Slot) := (others => <>);
   end Clear_Slot;
   procedure Clear (Store : in out State; At_Slot : Slot) is
   begin
      Clear_Slot (Store, At_Slot);
   end Clear;
   function Range_Cleared
     (Store, Prior : State; First, Last : Slot) return Boolean
   is
   begin
      if Store.Last_Stamp /= Prior.Last_Stamp then return False; end if;
      for Index in Slot loop
         if Index in First .. Last then
            if Store.Slots (Index) /= Envelope'(others => <>) then return False; end if;
         elsif Store.Slots (Index) /= Prior.Slots (Index) then return False;
         end if;
      end loop;
      declare
         Count : Natural := 0;
      begin
         for Index in First .. Last loop
            if Prior.Slots (Index).Tag = Temporary_Scalar then Count := Count + 1; end if;
         end loop;
         declare
            Items : Ledger.Handle_Array (1 .. Count);
            Used : Natural := 0;
         begin
            for Index in First .. Last loop
               if Prior.Slots (Index).Tag = Temporary_Scalar then
                  Used := Used + 1; Items (Used) := Prior.Slots (Index).Lease;
               end if;
            end loop;
            return Ledger.Dropped_Exactly (Store.Scalars, Prior.Scalars, Items);
         end;
      end;
   end Range_Cleared;
   procedure Clear_Range (Store : in out State; First, Last : Slot) is
      Count : Natural := 0;
   begin
      if First > Last then return; end if;
      for Index in First .. Last loop
         if Store.Slots (Index).Tag = Temporary_Scalar then Count := Count + 1; end if;
      end loop;
      declare
         Items : Ledger.Handle_Array (1 .. Count);
         Used : Natural := 0;
         Result : Ledger.Status;
      begin
         for Index in First .. Last loop
            if Store.Slots (Index).Tag = Temporary_Scalar then
               Used := Used + 1;
               Items (Used) := Store.Slots (Index).Lease;
            end if;
         end loop;
         Ledger.Drop_Many (Store.Scalars, Items, Result);
         pragma Assert (Result = Ledger.Ready);
         if Result /= Ledger.Ready then return; end if;
      end;
      for Index in First .. Last loop Store.Slots (Index) := (others => <>); end loop;
   end Clear_Range;
   procedure Clear_All (Store : in out State) is
   begin
      Ledger.Clear_All (Store.Scalars);
      Store.Slots := [others => <>];
   end Clear_All;
   function Converted (Status : Ledger.Status) return Outcome is
     (case Status is
        when Ledger.Ready => Ready,
        when Ledger.Invalid_Domain | Ledger.Invalid_Handle => Invalid_Source,
        when Ledger.Object_Limit | Ledger.Root_Limit => Capacity_Limit,
        when Ledger.Generation_Exhausted => Generation_Exhausted);
   procedure Normalize_Temporary_Result
     (Store : in out State; At_Slot : Slot; Width : AML_Decode.Integer_Width;
      Status : out Outcome)
   is
      Changed : Ledger.Status;
   begin
      if not Store.Slots (At_Slot).Present
        or else Store.Slots (At_Slot).Tag /= Temporary_Scalar
      then Status := Invalid_Source; return; end if;
      Ledger.Set_Result_Width (Store.Scalars, Store.Slots (At_Slot).Lease,
        (if Width = AML_Decode.Bits_32 then Ledger.Bits_32 else Ledger.Bits_64), Changed);
      Status := Converted (Changed);
   end Normalize_Temporary_Result;
   procedure Put
     (Store : in out State; At_Slot : Slot; Value : Datum; Status : out Outcome)
   is
      Lease : Ledger.Handle := Ledger.No_Handle;
      Added : Ledger.Status;
   begin
      if Store.Last_Stamp = Slot_Stamp'Last then Status := Generation_Exhausted; return; end if;
      if Value.Value_Kind = Integer_Datum then
         -- One staging object permits replacement at the usable limit; a
         -- shared destination does not release its old object on replacement.
         if Ledger.Objects (Store.Scalars) = Object_Capacity
           and then (Store.Slots (At_Slot).Tag /= Temporary_Scalar
             or else Ledger.Owners (Store.Scalars, Store.Slots (At_Slot).Lease) /= 1)
         then Status := Capacity_Limit; return; end if;
         Ledger.New_Value (Store.Scalars, Value.Number,
           (if Value.Origin = AML_Decode.AML_Constant then Ledger.AML_Constant else Ledger.Ordinary), Lease, Added);
         Status := Converted (Added);
         if Status /= Ready then return; end if;
      end if;
      Clear_Slot (Store, At_Slot);
      Store.Last_Stamp := Store.Last_Stamp + 1;
      Store.Slots (At_Slot) :=
        (Present => True, Stamp => Store.Last_Stamp, Payload => Value,
         Tag => (if Value.Value_Kind = Integer_Datum then Temporary_Scalar else Non_Scalar),
         Lease => Lease, Owned => AML_References.No_Object_Handle);
      Status := Ready;
   end Put;
   procedure Put_Owned
     (Store : in out State; At_Slot : Slot; Value : AML_Decode.Integer_Value;
      Origin : AML_Decode.Integer_Origin; Identity : AML_References.Object_Handle;
      Status : out Outcome) is
   begin
      if Identity = AML_References.No_Object_Handle then Status := Invalid_Source; return; end if;
      if Store.Last_Stamp = Slot_Stamp'Last then Status := Generation_Exhausted; return; end if;
      Clear_Slot (Store, At_Slot);
      Store.Last_Stamp := Store.Last_Stamp + 1;
      Store.Slots (At_Slot) :=
        (Present => True, Stamp => Store.Last_Stamp, Payload => (Integer_Datum, Value, Origin),
         Tag => Owned_Scalar, Lease => Ledger.No_Handle, Owned => Identity);
      Status := Ready;
   end Put_Owned;
   procedure Refresh_Owned_Cache
     (Store : in out State; At_Slot : Slot; Value : AML_Decode.Integer_Value;
      Origin : AML_Decode.Integer_Origin) is
   begin
      Store.Slots (At_Slot).Payload := (Integer_Datum, Value, Origin);
   end Refresh_Owned_Cache;
   procedure Share
     (Store : in out State; From, Into : Slot; Status : out Outcome)
   is
      Item : Envelope := Store.Slots (From);
      Added : Ledger.Status;
      New_Lease : Ledger.Handle;
   begin
      if not Item.Present then Status := Invalid_Source; return; end if;
      if From = Into then Status := Ready; return; end if;
      if Store.Last_Stamp = Slot_Stamp'Last then Status := Generation_Exhausted; return; end if;
      if Item.Tag = Temporary_Scalar then
         Ledger.Share (Store.Scalars, Item.Lease, New_Lease, Added);
         Status := Converted (Added);
         if Status /= Ready then return; end if;
         Item.Lease := New_Lease;
      end if;
      Clear_Slot (Store, Into);
      Store.Last_Stamp := Store.Last_Stamp + 1;
      Item.Stamp := Store.Last_Stamp;
      Store.Slots (Into) := Item; Status := Ready;
   end Share;
   procedure Move
     (Store : in out State; From, Into : Slot; Status : out Outcome)
   is
      Item : Envelope := Store.Slots (From);
      New_Lease : Ledger.Handle;
      Moved : Ledger.Status;
   begin
      if not Item.Present then Status := Invalid_Source; return; end if;
      if From = Into then Status := Ready; return; end if;
      if Store.Last_Stamp = Slot_Stamp'Last then Status := Generation_Exhausted; return; end if;
      if Item.Tag = Temporary_Scalar then
         Ledger.Move (Store.Scalars, Item.Lease, New_Lease, Moved);
         Status := Converted (Moved);
         if Status /= Ready then return; end if;
         Item.Lease := New_Lease;
      end if;
      -- The source lease was moved, so clearing this envelope must not Drop it.
      Store.Slots (From) := (others => <>);
      Clear_Slot (Store, Into);
      Store.Last_Stamp := Store.Last_Stamp + 1;
      Item.Stamp := Store.Last_Stamp;
      Store.Slots (Into) := Item; Status := Ready;
   end Move;
   function Root_Projection (Store : State; At_Slot : Slot) return Datum is
      Integer_Type_Code : constant Natural := 1;
      Item : constant Envelope := Store.Slots (At_Slot);
   begin
      if Item.Tag = Owned_Scalar then
         return (Object_Datum,
           (Source => Item.Owned, ID => AML_References.Source (Item.Owned),
            Type_Code => Integer_Type_Code, Size => 0, others => <>));
      end if;
      return Snapshot (Store, At_Slot);
   end Root_Projection;
   procedure Visit_Compound_Owners
     (Store : State; Excluded : Slot_Identity; Status : out Outcome)
   is
      Accepted : Boolean;
   begin
      Status := Invalid_Source;
      if not Matches_Slot (Store, Excluded)
        or else Kind (Store, Position (Excluded)) /= Owned_Scalar
      then return; end if;
      for Index in Slot loop
         if Index /= Position (Excluded) and then Occupied (Store, Index)
           and then Kind (Store, Index) = Non_Scalar
         then
            Visit (Snapshot (Store, Index), Accepted);
            if not Accepted then return; end if;
         end if;
      end loop;
      Status := Ready;
   end Visit_Compound_Owners;
end AML_Execute.Scalar_Transport;
