package body AML_Scalar_Ledger.Dropped_Equivalence is
   function Frozen_Dropped
     (Store, Prior : State; Items : Handle_Array) return Boolean
   is
      Selected : array (Root_Slot) of Boolean := [others => False];
      Removed : array (Object_Slot) of Owner_Count := [others => 0];
      Retired : Object_Index := 0;
   begin
      if Store.Domain /= Prior.Domain or else Store.Last /= Prior.Last then return False; end if;
      for Item of Items loop
         if not Live (Prior, Item) or else Selected (Item.Slot) then return False; end if;
         Selected (Item.Slot) := True;
         declare Object : constant Object_Slot := Prior.Leases (Item.Slot).Object; begin
            if Removed (Object) = Owner_Count'Last then return False; end if;
            Removed (Object) := Removed (Object) + 1;
         end;
      end loop;
      for Index in Root_Slot loop
         if Selected (Index) then
            if Store.Leases (Index) /= Root_Entry'(others => <>) then return False; end if;
         elsif Store.Leases (Index) /= Prior.Leases (Index) then return False;
         end if;
      end loop;
      for Index in Object_Slot loop
         if Removed (Index) = 0 then
            if Store.Values (Index) /= Prior.Values (Index) then return False; end if;
         elsif Removed (Index) > Prior.Values (Index).Count then return False;
         elsif Removed (Index) = Prior.Values (Index).Count then
            if Store.Values (Index) /= Scalar'(others => <>) then return False; end if;
            Retired := Retired + 1;
         elsif Store.Values (Index) /= Scalar'
           (Prior.Values (Index).Value, Prior.Values (Index).Origin,
            Prior.Values (Index).Count - Removed (Index)) then return False;
         end if;
      end loop;
      return Retired <= Prior.Live_Objects
        and then Store.Live_Objects = Prior.Live_Objects - Retired;
   end Frozen_Dropped;

      function Candidate_Selected (Items : Handle_Array; Through : Natural; Root : Root_Slot)
        return Boolean is
        (Through > 0 and then (for some I in Items'First .. Items'First + (Through - 1) =>
          Items (I).Slot = Root));
      function Candidate_Removed_Count
        (Prior : State; Items : Handle_Array; Through : Natural; Object : Object_Slot)
        return Natural is
        (if Through = 0 then 0 else Candidate_Removed_Count (Prior, Items, Through - 1, Object)
          + Boolean'Pos (Live (Prior, Items (Items'First + (Through - 1))) and then
              Prior.Leases (Items (Items'First + (Through - 1)).Slot).Object = Object));
      function Candidate_Retired_Count (Before, After : State; Through : Object_Index)
        return Natural is
        (if Through = 0 then 0 else Candidate_Retired_Count (Before, After, Through - 1)
          + Boolean'Pos (Before.Values (Through).Count /= 0 and then After.Values (Through).Count = 0));
      function Candidate_Valid_Selection
        (Prior : State; Items : Handle_Array; Through : Natural) return Boolean is
        (Through = 0 or else
          ((for all I in Items'First .. Items'First + (Through - 1) => Live (Prior, Items (I)))
           and then (for all I in Items'First .. Items'First + (Through - 1) =>
             (for all J in Items'First .. I - 1 => Items (I).Slot /= Items (J).Slot))));
      function Candidate_Model (Store, Prior : State; Items : Handle_Array)
        return Boolean is
        (Store.Domain = Prior.Domain and then Store.Last = Prior.Last
         and then Candidate_Valid_Selection (Prior, Items, Items'Length)
         and then (for all R in Root_Slot =>
           (if Candidate_Selected (Items, Items'Length, R) then Store.Leases (R) = Root_Entry'(others => <>)
            else Store.Leases (R) = Prior.Leases (R)))
         and then (for all O in Object_Slot =>
           Candidate_Removed_Count (Prior, Items, Items'Length, O) <= Prior.Values (O).Count and then
           (if Candidate_Removed_Count (Prior, Items, Items'Length, O) = 0 then Store.Values (O) = Prior.Values (O)
            elsif Candidate_Removed_Count (Prior, Items, Items'Length, O) = Prior.Values (O).Count then
              Store.Values (O) = Scalar'(others => <>)
            else Store.Values (O) = Scalar'(Prior.Values (O).Value, Prior.Values (O).Origin,
              Prior.Values (O).Count - Candidate_Removed_Count (Prior, Items, Items'Length, O))))
         and then Candidate_Retired_Count (Prior, Store, Object_Capacity) <= Prior.Live_Objects
         and then Store.Live_Objects = Prior.Live_Objects - Candidate_Retired_Count (Prior, Store, Object_Capacity));
   procedure Run (Domain, Foreign : AML_Frame_Handles.Invocation_Domain; Checks : out Natural) is
      Prior, After : State;
      A, B : Handle;
      procedure Compare (Items : Handle_Array) is
         Expected : constant Boolean := Frozen_Dropped (After, Prior, Items);
      begin
         if Candidate_Model (After, Prior, Items) /= Expected then
            raise Program_Error with "dropped model differs from frozen checker";
         end if;
         if Validation.Check_Dropped (After, Prior, Items) /= Expected then
            raise Program_Error with "extracted checker differs from frozen checker";
         end if;
         pragma Assert (Dropped_Exactly (After, Prior, Items) = Expected);
         Checks := Checks + 1;
      end Compare;
      procedure Case_Expected (Items : Handle_Array; Expected : Boolean) is
      begin
         if Frozen_Dropped (After, Prior, Items) /= Expected then
            raise Program_Error with "independent expected result";
         end if;
         Compare (Items);
      end Case_Expected;
      procedure One_Root_After is
         O : constant Object_Index := Prior.Leases (1).Object;
      begin
         After := Prior;
         After.Leases (1) := (others => <>);
         if O /= 0 and then After.Values (O).Count > 0 then
            After.Values (O).Count := After.Values (O).Count - 1;
            if After.Values (O).Count = 0 then
               After.Values (O) := (others => <>);
               if After.Live_Objects > 0 then After.Live_Objects := After.Live_Objects - 1; end if;
            end if;
         end if;
      end One_Root_After;
   begin
      if Object_Capacity /= 2 or else Root_Capacity /= 2 or else Generation_Limit /= 2 then
         raise Program_Error with "fixture requires 2/2/2";
      end if;
      Checks := 0;
      A := (Domain, 1, 1); B := (Domain, 2, 1);
      -- Genuine positive empty/single/batch, high lowerbound, payload/default witnesses.
      Prior := (Domain => Domain, Last => 1, Live_Objects => 1,
        Values => [1 => (Number'Last, AML_Constant, 2), 2 => (others => <>)],
        Leases => [1 => (1, 1), 2 => (1, 1)]);
      After := Prior; Case_Expected ([7 .. 6 => No_Handle], True);
      One_Root_After; Case_Expected ([17 => A], True);
      After := Prior; After.Leases := [others => <>]; After.Values := [others => <>];
      After.Live_Objects := 0; Case_Expected ([19 => A, 20 => B], True);
      Case_Expected ([19 => B, 20 => A], True);
      Case_Expected ([19 => A, 20 => A], False);
      Case_Expected ([19 => A, 20 => B, 21 => A], False); -- saturation then duplicate: overflow rejected earlier
      Case_Expected ([19 => (Foreign, 1, 1)], False);
      Case_Expected ([19 => (Domain, 1, 2)], False);
      Case_Expected ([19 => No_Handle], False);
      After.Domain := Foreign; Case_Expected ([19 => A, 20 => B], False);
      After := Prior; After.Last := 2; Case_Expected ([7 .. 6 => No_Handle], False);
      -- Full scalar record must be retained for an unselected object, even malformed state.
      After := Prior; After.Values (2).Value := Number'Last; Case_Expected ([7 .. 6 => No_Handle], False);
      After := Prior; After.Values (2).Origin := AML_Constant; Case_Expected ([7 .. 6 => No_Handle], False);
      -- Exhaustive structural states; no Valid assumption and arbitrary live-object cache.
      for Authority in Boolean loop
       for Last in Generation loop
        for Cached in Object_Index loop
         for C1 in Owner_Count loop
          for C2 in Owner_Count loop
           for O1 in Object_Index loop
            for O2 in Object_Index loop
             for G1 in Generation loop
              for G2 in Generation loop
               Prior := (Domain => (if Authority then Domain else AML_Frame_Handles.No_Domain),
                 Last => Last, Live_Objects => Cached,
                 Values => [1 => (Number'Last, AML_Constant, C1), 2 => (7, Ordinary, C2)],
                 Leases => [1 => (O1, G1), 2 => (O2, G2)]);
               After := Prior; Compare ([11 .. 10 => No_Handle]);
               One_Root_After; Compare ([23 => A]);
               Compare ([23 => A, 24 => A]);
               Compare ([23 => (Foreign, 1, 1)]);
              end loop;
             end loop;
            end loop;
           end loop;
          end loop;
         end loop;
        end loop;
       end loop;
      end loop;
   end Run;
end AML_Scalar_Ledger.Dropped_Equivalence;
