package body Block_Cache_Index with SPARK_Mode is
   pragma Assertion_Policy (Pre => Ignore, Post => Ignore, Ghost => Ignore,
                            Loop_Invariant => Ignore, Assert => Ignore);

   --  Under Placed, a key can only be held within its own set.
   procedure Lemma_Outside_Set (Table : Index; Key : Block_Key)
     with Ghost,
          Pre => Placed (Table) and then
                 (for all Way in Way_Index =>
                    not Holds (Table, Slot_Of (Set_Of (Key), Way), Key)),
          Post => not Contains (Table, Key)
   is
   begin
      for Slot in Slot_Index loop
         pragma Assert
           (if Holds (Table, Slot, Key) then
              Slot / Ways = Set_Of (Key) and then
              Slot = Slot_Of (Set_Of (Key), Slot mod Ways));
         pragma Loop_Invariant
           (for all Earlier in Slot_Index'First .. Slot =>
              not Holds (Table, Earlier, Key));
      end loop;
   end Lemma_Outside_Set;

   procedure Find
     (Table : in out Index; Key : Block_Key;
      Found : out Boolean; Slot : out Slot_Index)
   is
      Set : constant Set_Index := Set_Of (Key);
   begin
      Found := False;
      Slot := Slot_Of (Set, 0);
      for Way in Way_Index loop
         if Holds (Table, Slot_Of (Set, Way), Key) then
            Found := True;
            Slot := Slot_Of (Set, Way);
            Table.Referenced (Slot) := True;
            return;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Way_Index'First .. Way =>
              not Holds (Table, Slot_Of (Set, Earlier), Key));
         pragma Loop_Invariant (Table = Table'Loop_Entry);
      end loop;
      Lemma_Outside_Set (Table, Key);
   end Find;

   procedure Lemma_Way (Set : Set_Index; Way : Way_Index)
     with Ghost, Post => Slot_Of (Set, Way) mod Ways = Way
   is
   begin
      null;
   end Lemma_Way;

   procedure Clear (Table : out Index; Active_Ways : Way_Count) is
   begin
      Table :=
        (Keys => [others => (Volume => 0, Block => 0)],
         Used | Dirty | Referenced => [others => False],
         Classes => [others => File_Data], Hands => [others => 0],
         Active_Ways => Active_Ways);
   end Clear;

   procedure Claim
     (Table : in out Index; Key : Block_Key;
      Found : out Boolean; Slot : out Slot_Index)
   is
      Set : constant Set_Index := Set_Of (Key);
      --  Two sweeps of the set find a clean victim if one exists: the first
      --  clears every reference bit it passes.
      Sweep_Limit : constant := 2 * Ways;
      Way : Way_Index;
      Chosen : Way_Index := 0;
   begin
      Found := False;
      Slot := Slot_Of (Set, 0);
      for Candidate in 0 .. Table.Active_Ways - 1 loop
         if not Table.Used (Slot_Of (Set, Candidate)) then
            Slot := Slot_Of (Set, Candidate);
            Chosen := Candidate;
            Found := True;
            exit;
         end if;
      end loop;
      if not Found then
         for Step in 1 .. Sweep_Limit loop
            Way := Table.Hands (Set);
            Table.Hands (Set) := (if Way >= Table.Active_Ways - 1 then 0 else Way + 1);
            if not Table.Dirty (Slot_Of (Set, Way)) then
               if Table.Referenced (Slot_Of (Set, Way)) then
                  Table.Referenced (Slot_Of (Set, Way)) := False;
               else
                  Slot := Slot_Of (Set, Way);
                  Chosen := Way;
                  Found := True;
                  exit;
               end if;
            end if;
            pragma Loop_Invariant
              (Table.Keys = Table'Loop_Entry.Keys and then
               Table.Used = Table'Loop_Entry.Used and then
               Table.Dirty = Table'Loop_Entry.Dirty and then
               Table.Classes = Table'Loop_Entry.Classes and then
               Table.Active_Ways = Table'Loop_Entry.Active_Ways);
            pragma Loop_Invariant (Way < Table.Active_Ways);
            pragma Loop_Invariant
              (for all S in Set_Index => Table.Hands (S) < Table.Active_Ways);
            pragma Loop_Invariant (not Found);
         end loop;
         --  Only reference bits and the hand moved; restore the observable
         --  state exactly when every way was dirty.
         if not Found then
            for Candidate in 0 .. Table.Active_Ways - 1 loop
               if not Table.Dirty (Slot_Of (Set, Candidate)) then
                  Slot := Slot_Of (Set, Candidate);
                  Chosen := Candidate;
                  Found := True;
                  exit;
               end if;
            end loop;
         end if;
      end if;
      if Found then
         pragma Assert (Slot = Slot_Of (Set, Chosen) and then Chosen < Table.Active_Ways);
         Lemma_Way (Set, Chosen);
         pragma Assert (Slot mod Ways < Table.Active_Ways);
         pragma Assert (not Table.Used (Slot) or else not Table.Dirty (Slot));
         Table.Keys (Slot) := Key;
         Table.Used (Slot) := True;
         Table.Dirty (Slot) := False;
         Table.Referenced (Slot) := True;
      end if;
   end Claim;

   procedure Mark_Dirty
     (Table : in out Index; Slot : Slot_Index; Class : Block_Class)
   is
   begin
      Table.Dirty (Slot) := True;
      Table.Classes (Slot) := Class;
   end Mark_Dirty;

   procedure Mark_Clean (Table : in out Index; Slot : Slot_Index) is
   begin
      Table.Dirty (Slot) := False;
   end Mark_Clean;

   procedure Forget (Table : in out Index; Key : Block_Key) is
   begin
      for Slot in Slot_Index loop
         if Holds (Table, Slot, Key) then
            Table.Used (Slot) := False;
            Table.Referenced (Slot) := False;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Slot_Index'First .. Slot =>
              not Holds (Table, Earlier, Key));
         pragma Loop_Invariant
           (Table.Keys = Table'Loop_Entry.Keys and then
            Table.Dirty = Table'Loop_Entry.Dirty and then
            Table.Classes = Table'Loop_Entry.Classes);
         pragma Loop_Invariant
           (for all Other in Slot_Index =>
              (if Table.Used (Other) then Table'Loop_Entry.Used (Other)));
         pragma Loop_Invariant
           (for all Other in Slot_Index =>
              (if Table'Loop_Entry.Used (Other) and then
                  Table'Loop_Entry.Keys (Other) /= Key
               then Table.Used (Other)));
      end loop;
   end Forget;

   procedure Discard_Volume (Table : in out Index; Volume : Unsigned_64) is
   begin
      for Slot in Slot_Index loop
         if Table.Used (Slot) and then Table.Keys (Slot).Volume = Volume then
            Table.Used (Slot) := False;
            Table.Dirty (Slot) := False;
            Table.Referenced (Slot) := False;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Slot_Index'First .. Slot =>
              (if Table.Used (Earlier) then Table.Keys (Earlier).Volume /= Volume));
         pragma Loop_Invariant
           (Table.Keys = Table'Loop_Entry.Keys and then
            Table.Classes = Table'Loop_Entry.Classes);
         pragma Loop_Invariant
           (for all Other in Slot_Index =>
              (if Table.Used (Other) then Table'Loop_Entry.Used (Other)));
         pragma Loop_Invariant
           (for all Other in Slot_Index =>
              (if Table.Dirty (Other) then Table.Used (Other)));
         pragma Loop_Invariant
           (for all Other in Slot_Index =>
              (if Table'Loop_Entry.Used (Other) and then
                  Table'Loop_Entry.Keys (Other).Volume /= Volume
               then Table.Used (Other) and then
                    Table.Dirty (Other) = Table'Loop_Entry.Dirty (Other)));
      end loop;
   end Discard_Volume;

   procedure Lowest_Dirty_Class
     (Table : Index; Volume : Unsigned_64;
      Found : out Boolean; Class : out Block_Class)
   is
   begin
      Found := False;
      Class := Block_Class'Last;
      for Slot in Slot_Index loop
         if Table.Used (Slot) and then Table.Dirty (Slot) and then
           Table.Keys (Slot).Volume = Volume and then
           (not Found or else Table.Classes (Slot) < Class)
         then
            Found := True;
            Class := Table.Classes (Slot);
         end if;
         pragma Loop_Invariant
           (Found = (for some Earlier in Slot_Index'First .. Slot =>
              Table.Used (Earlier) and then Table.Dirty (Earlier) and then
              Table.Keys (Earlier).Volume = Volume));
         pragma Loop_Invariant
           (if Found then
              (for some Earlier in Slot_Index'First .. Slot =>
                 Table.Used (Earlier) and then Table.Dirty (Earlier) and then
                 Table.Keys (Earlier).Volume = Volume and then
                 Table.Classes (Earlier) = Class) and then
              (for all Earlier in Slot_Index'First .. Slot =>
                 (if Table.Used (Earlier) and then Table.Dirty (Earlier) and then
                     Table.Keys (Earlier).Volume = Volume
                  then Table.Classes (Earlier) >= Class)));
      end loop;
   end Lowest_Dirty_Class;
end Block_Cache_Index;
