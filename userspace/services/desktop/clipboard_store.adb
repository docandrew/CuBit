------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Desktop clipboard history (see the spec and docs/clipboard.md).
------------------------------------------------------------------------------
package body Clipboard_Store with SPARK_Mode is

   Zero_Bytes : constant Payload_Bytes := [others => 0];

   --  Replace one slot; every other slot keeps its contents.
   procedure Replace_Slot
     (S : in out Store; Slot : Slot_Index; E : Slot_Entry)
     with Pre => Valid (S) and then Valid_Slot (E, S.Next) and then
       (if E.Meta.Phase /= Vacant then
          (for all K in Slot_Index =>
             (if K /= Slot and then S.Slots (K).Meta.Phase /= Vacant then
                S.Slots (K).Meta.Name /= E.Meta.Name))),
       Post => Valid (S) and then S.Next = S.Next'Old and then
         S.Slots (Slot) = E and then
         (for all K in Slot_Index =>
            (if K /= Slot then S.Slots (K) = S.Slots'Old (K)))
   is
   begin
      S.Slots (Slot) := E;
   end Replace_Slot;

   --  The slot's content is gone; only a sensitive tombstone or nothing
   --  remains. Wiping writes zeros over every byte, used or not.
   procedure Wipe
     (S : in out Store; Slot : Slot_Index; Phase : Entry_Phase)
     with Pre => Valid (S) and then Phase /= Available and then
       (if Phase in Tombstone then
          S.Slots (Slot).Meta.Phase /= Vacant and then
          S.Slots (Slot).Meta.Kind = Sensitive),
       Post => Valid (S) and then S.Next = S.Next'Old and then
         S.Slots (Slot).Meta.Phase = Phase and then
         All_Zero (S.Slots (Slot).Data) and then
         (if Phase /= Vacant then
            S.Slots (Slot).Meta.Name = S.Slots'Old (Slot).Meta.Name and then
            S.Slots (Slot).Meta.Kind = S.Slots'Old (Slot).Meta.Kind and then
            S.Slots (Slot).Meta.Life = S.Slots'Old (Slot).Meta.Life and then
            S.Slots (Slot).Meta.Pinned = S.Slots'Old (Slot).Meta.Pinned) and then
         (for all K in Slot_Index =>
            (if K /= Slot then S.Slots (K) = S.Slots'Old (K)))
   is
      E : Slot_Entry := S.Slots (Slot);
   begin
      E.Data := Zero_Bytes;
      if Phase = Vacant then
         E.Meta := (others => <>);
      else
         E.Meta.Phase := Phase;
      end if;
      Replace_Slot (S, Slot, E);
   end Wipe;

   function Current (S : Store) return Slot_Count is
      Best : Slot_Count := 0;
   begin
      for K in Slot_Index loop
         pragma Loop_Invariant
           (if Best /= 0 then
              Best < K and then S.Slots (Best).Meta.Phase = Available and then
              (for all J in Slot_Index =>
                 (if J < K and then S.Slots (J).Meta.Phase = Available then
                    S.Slots (J).Meta.Name <= S.Slots (Best).Meta.Name)));
         pragma Loop_Invariant
           (if Best = 0 then
              (for all J in Slot_Index =>
                 (if J < K then S.Slots (J).Meta.Phase /= Available)));
         if S.Slots (K).Meta.Phase = Available and then
           (Best = 0 or else S.Slots (K).Meta.Name > S.Slots (Best).Meta.Name)
         then
            Best := K;
         end if;
      end loop;
      return Best;
   end Current;

   function Older (S : Store; Below : Entry_Name) return Slot_Count is
      Best : Slot_Count := 0;
   begin
      for K in Slot_Index loop
         pragma Loop_Invariant
           (if Best /= 0 then
              S.Slots (Best).Meta.Phase /= Vacant and then
              (Below = No_Entry or else S.Slots (Best).Meta.Name < Below));
         if S.Slots (K).Meta.Phase /= Vacant and then
           (Below = No_Entry or else S.Slots (K).Meta.Name < Below) and then
           (Best = 0 or else S.Slots (K).Meta.Name > S.Slots (Best).Meta.Name)
         then
            Best := K;
         end if;
      end loop;
      return Best;
   end Older;

   function Find (S : Store; Name : Entry_Name) return Slot_Count is
   begin
      if Name = No_Entry then
         return 0;
      end if;
      for K in Slot_Index loop
         pragma Loop_Invariant
           (for all J in Slot_Index =>
              (if J < K then
                 S.Slots (J).Meta.Phase = Vacant or else
                 S.Slots (J).Meta.Name /= Name));
         if S.Slots (K).Meta.Phase /= Vacant and then
           S.Slots (K).Meta.Name = Name
         then
            return K;
         end if;
      end loop;
      return 0;
   end Find;

   --  Victim for a new entry: a vacant slot, else the oldest tombstone, else
   --  the oldest unpinned entry. 0 when every slot is pinned.
   function Victim (S : Store) return Slot_Count
     with Pre => Valid (S),
       Post => (if Victim'Result /= 0 then
                  not S.Slots (Victim'Result).Meta.Pinned)
   is
      Best : Slot_Count := 0;
      function Rank (E : Slot_Entry) return Natural is
        (case E.Meta.Phase is
           when Vacant => 0, when Consumed | Expired => 1,
           when Available => 2);
   begin
      for K in Slot_Index loop
         pragma Loop_Invariant
           (if Best /= 0 then not S.Slots (Best).Meta.Pinned);
         if not S.Slots (K).Meta.Pinned and then
           (Best = 0 or else
            Rank (S.Slots (K)) < Rank (S.Slots (Best)) or else
            (Rank (S.Slots (K)) = Rank (S.Slots (Best)) and then
             S.Slots (K).Meta.Name < S.Slots (Best).Meta.Name))
         then
            Best := K;
         end if;
      end loop;
      return Best;
   end Victim;

   procedure Offer
     (S       : in out Store;
      Source  : Source_Identity;
      Now     : Millisecond;
      Kind    : Sensitivity;
      Shape   : Layout;
      Data    : Payload_Bytes;
      Name    : out Entry_Name;
      Outcome : out Offer_Outcome)
   is
      Slot : Slot_Count;
      E    : Slot_Entry;
   begin
      Name := No_Entry;
      if S.Next = Entry_Name'Last then
         Outcome := Names_Exhausted;
         return;
      end if;
      Slot := Victim (S);
      if Slot = 0 then
         Outcome := History_Full;
         return;
      end if;
      Wipe (S, Slot, Vacant);
      Name := S.Next;
      S.Next := S.Next + 1;
      pragma Assert
        (for all K in Slot_Index =>
           (if S.Slots (K).Meta.Phase /= Vacant then
              S.Slots (K).Meta.Name < Name));
      E.Meta :=
        (Name      => Name,
         Phase     => Available,
         Source    => Source,
         Copied_At => Now,
         Kind      => Kind,
         Life      => Lifetime_For (Kind, Now),
         Pinned    => False,
         Shape     => Shape);
      --  Only the declared regions are kept; the rest of the slot stays zero
      --  so no byte from a previous entry or the staging copy survives.
      E.Data := Zero_Bytes;
      for I in Payload_Index range
        1 .. Shape.Type_Length + Shape.Text_Length + Shape.CCL_Length
      loop
         E.Data (I) := Data (I);
      end loop;
      Replace_Slot (S, Slot, E);
      Outcome := Stored;
      pragma Assert (S.Slots (Slot).Meta.Name = Name);
      pragma Assert
        (for all K in Slot_Index =>
           (if S.Slots (K).Meta.Phase = Available then
              S.Slots (K).Meta.Name <= Name));
   end Offer;

   procedure Expire (S : in out Store; Now : Millisecond) is
   begin
      for K in Slot_Index loop
         pragma Loop_Invariant (Valid (S) and then S.Next = S.Next'Loop_Entry);
         pragma Loop_Invariant
           (for all J in Slot_Index =>
              (if J < K and then S.Slots (J).Meta.Phase = Available then
                 not Expired_At (S.Slots (J).Meta.Life, Now)));
         if S.Slots (K).Meta.Phase = Available and then
           Expired_At (S.Slots (K).Meta.Life, Now)
         then
            --  Only sensitive entries carry a deadline.
            Wipe (S, K, Expired);
         end if;
      end loop;
   end Expire;

   function Next_Deadline (S : Store; Otherwise : Millisecond)
     return Millisecond
   is
      Result : Millisecond := Otherwise;
   begin
      for K in Slot_Index loop
         if S.Slots (K).Meta.Phase = Available and then
           S.Slots (K).Meta.Life.Kind = Until_Deadline and then
           S.Slots (K).Meta.Life.Deadline < Result
         then
            Result := S.Slots (K).Meta.Life.Deadline;
         end if;
      end loop;
      return Result;
   end Next_Deadline;

   procedure Paste
     (S        : in out Store;
      Now      : Millisecond;
      Target   : Sensitivity;
      Accepted : Representation_Set;
      Meta     : out Metadata;
      Data     : out Payload_Bytes;
      Outcome  : out Paste_Outcome)
   is
      Slot : Slot_Count;
      Keep_CCL : Boolean;
      Delivered_Length : Payload_Length;
   begin
      Meta := (others => <>);
      Data := Zero_Bytes;
      Expire (S, Now);
      if not Accepted (Text_Form) then
         Outcome := Format_Refused;
         return;
      end if;
      Slot := Current (S);
      if Slot = 0 then
         Outcome := Nothing_Available;
         return;
      end if;
      Meta := S.Slots (Slot).Meta;
      Keep_CCL := Accepted (CCL_Form) and then Meta.Shape.CCL_Length > 0;
      if not Keep_CCL then
         --  Text only: the type name goes with the CCL form.
         Meta.Shape := (Type_Length => 0,
                        Text_Length => Meta.Shape.Text_Length,
                        CCL_Length  => 0);
      end if;
      Delivered_Length := Meta.Shape.Type_Length + Meta.Shape.Text_Length +
        Meta.Shape.CCL_Length;
      --  Regions keep their order: the type and CCL forms are dropped
      --  together, so the text form moves to the front when dropped.
      declare
         Offset : constant Payload_Length :=
           (if Keep_CCL then 0 else S.Slots (Slot).Meta.Shape.Type_Length);
      begin
         for I in Payload_Index range 1 .. Delivered_Length loop
            pragma Loop_Invariant (Valid (S));
            Data (I) := S.Slots (Slot).Data (I + Offset);
         end loop;
      end;
      if Meta.Kind = Sensitive or else Target = Sensitive then
         --  Paste once. An ordinary entry pasted into a secret field becomes
         --  a tombstone too; record it as sensitive so it shows the mask.
         declare
            E : Slot_Entry := S.Slots (Slot);
         begin
            E.Meta.Kind := Sensitive;
            E.Meta.Pinned := False;
            E.Meta.Life := Lifetime_For (Sensitive, E.Meta.Copied_At);
            E.Meta.Phase := Consumed;
            E.Data := Zero_Bytes;
            Replace_Slot (S, Slot, E);
         end;
      end if;
      Outcome := Delivered;
   end Paste;

   procedure Promote (S : in out Store; Slot : Slot_Index; Done : out Boolean)
   is
      E : Slot_Entry;
   begin
      Done := False;
      if S.Slots (Slot).Meta.Phase /= Available or else
        S.Next = Entry_Name'Last
      then
         return;
      end if;
      E := S.Slots (Slot);
      E.Meta.Name := S.Next;
      S.Next := S.Next + 1;
      pragma Assert
        (for all K in Slot_Index =>
           (if S.Slots (K).Meta.Phase /= Vacant then
              S.Slots (K).Meta.Name < E.Meta.Name));
      Replace_Slot (S, Slot, E);
      Done := True;
   end Promote;

   procedure Set_Pinned
     (S : in out Store; Slot : Slot_Index; Pinned : Boolean; Done : out Boolean)
   is
      E : Slot_Entry;
   begin
      Done := False;
      if S.Slots (Slot).Meta.Phase /= Available or else
        S.Slots (Slot).Meta.Kind /= Ordinary
      then
         return;
      end if;
      E := S.Slots (Slot);
      E.Meta.Pinned := Pinned;
      Replace_Slot (S, Slot, E);
      Done := True;
   end Set_Pinned;

   procedure Delete (S : in out Store; Slot : Slot_Index) is
   begin
      Wipe (S, Slot, Vacant);
   end Delete;

   procedure Clear (S : in out Store) is
   begin
      for K in Slot_Index loop
         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant
           (for all J in Slot_Index =>
              (if J < K and then not S.Slots (J).Meta.Pinned then
                 S.Slots (J).Meta.Phase = Vacant and then
                 All_Zero (S.Slots (J).Data)));
         if not S.Slots (K).Meta.Pinned then
            Wipe (S, K, Vacant);
         end if;
      end loop;
   end Clear;

   function Preview_Length
     (S : Store; Slot : Slot_Index; Limit : Payload_Length)
     return Payload_Length
   is
     (if S.Slots (Slot).Meta.Kind = Sensitive or else
         S.Slots (Slot).Meta.Phase /= Available
      then 0
      else Payload_Length'Min (Limit, S.Slots (Slot).Meta.Shape.Text_Length));

   function Preview_Byte
     (S : Store; Slot : Slot_Index; Index : Payload_Index)
     return Interfaces.Unsigned_8
   is
     (S.Slots (Slot).Data (Index + S.Slots (Slot).Meta.Shape.Type_Length));
end Clipboard_Store;
