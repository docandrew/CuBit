package body AML_Scalar_Ledger with SPARK_Mode is
   function Empty (Domain : AML_Frame_Handles.Invocation_Domain) return State is
     (Domain => Domain, others => <>);
   function Invocation_Domain (Store : State) return AML_Frame_Handles.Invocation_Domain is
     (Store.Domain);
   function Initialized
     (Store : State; Domain : AML_Frame_Handles.Invocation_Domain) return Boolean is
     (Store.Domain = Domain and then Store.Last = 0 and then Store.Live_Objects = 0
      and then (for all O in Object_Slot => Store.Values (O) = Scalar'(others => <>))
      and then (for all R in Root_Slot => Store.Leases (R) = Root_Entry'(others => <>)));
   procedure Initialize
     (Store : out State; Domain : AML_Frame_Handles.Invocation_Domain)
   is
   begin
      declare
         pragma Assertion_Policy (Ghost => Ignore);
      begin
         -- Exact State defaults: Last/Live_Objects zero; both tables empty.
         Store := (Domain => Domain, others => <>);
         Validation.Establish_Zero (Store);
         Validation.Establish_Valid (Store);
      end;
   end Initialize;
   function Objects (Store : State) return Natural is (Store.Live_Objects);
   package body Validation with SPARK_Mode is
      pragma Assertion_Policy (Ghost => Ignore, Assert => Ignore,
                               Loop_Invariant => Ignore);
      function Live_Value_Count (Store : State; Through : Object_Index)
        return Natural is
        (if Through = 0 then 0 else Live_Value_Count (Store, Through - 1)
           + Boolean'Pos (Store.Values (Through).Count /= 0));
      function Lease_Count (Store : State; Object : Object_Slot;
                            Through : Root_Index) return Natural is
        (if Through = 0 then 0 else Lease_Count (Store, Object, Through - 1)
           + Boolean'Pos (Store.Leases (Through).Object = Object));
      function Root_Count (Store : State; Through : Root_Index) return Natural is
        (if Through = 0 then 0 else Root_Count (Store, Through - 1)
           + Boolean'Pos (Store.Leases (Through).Object /= 0));
      function Lease_Valid (Store : State; Index : Root_Slot) return Boolean is
        (if Store.Leases (Index).Object /= 0 then
           AML_Frame_Handles.Has_Authority (Store.Domain)
           and then Store.Leases (Index).Serial /= 0
           and then Store.Leases (Index).Serial <= Store.Last
         else Store.Leases (Index).Serial = 0);
      function Model (Store : State) return Boolean is
        ((for all I in Root_Slot => Lease_Valid (Store, I))
         and then (for all O in Object_Slot =>
           Store.Values (O).Count = Lease_Count (Store, O, Root_Capacity))
         and then Store.Live_Objects = Live_Value_Count (Store, Object_Capacity));
      function Count_Roots (Store : State) return Natural is
         Count : Natural := 0;
      begin
         for Index in Root_Slot loop
            if Store.Leases (Index).Object /= 0 then Count := Count + 1; end if;
            pragma Loop_Invariant (Count = Root_Count (Store, Index));
         end loop;
         return Count;
      end Count_Roots;
      function Check (Store : State) return Boolean is
         Counts : array (Object_Slot) of Owner_Count := [others => 0];
         Live_Count : Object_Index := 0;
      begin
         for Index in Root_Slot loop
            -- Direct fields avoid a nonscalar local preceding loop invariants.
            if Store.Leases (Index).Object /= 0 then
               pragma Assert
                 (Counts (Store.Leases (Index).Object) =
                    Lease_Count (Store, Store.Leases (Index).Object, Index - 1));
               pragma Assert
                 (Lease_Count (Store, Store.Leases (Index).Object, Index - 1) <= Index - 1);
               pragma Assert (Counts (Store.Leases (Index).Object) < Owner_Count'Last);
               if not AML_Frame_Handles.Has_Authority (Store.Domain)
                 or else Store.Leases (Index).Serial = 0
                 or else Store.Leases (Index).Serial > Store.Last
                 or else Counts (Store.Leases (Index).Object) = Owner_Count'Last
               then
                  pragma Assert (not Lease_Valid (Store, Index));
                  pragma Assert (not Model (Store));
                  return False;
               end if;
               Counts (Store.Leases (Index).Object) :=
                 Counts (Store.Leases (Index).Object) + 1;
            elsif Store.Leases (Index).Serial /= 0 then
               pragma Assert (not Model (Store));
               return False;
            end if;
            pragma Assert (Lease_Valid (Store, Index));
            pragma Loop_Invariant
              (for all O in Object_Slot => Counts (O) = Lease_Count (Store, O, Index));
            pragma Loop_Invariant
              (for all I in 1 .. Index => Lease_Valid (Store, I));
         end loop;
         for Index in Object_Slot loop
            if Store.Values (Index).Count /= Counts (Index) then
               pragma Assert (not Model (Store));
               return False;
            end if;
            if Store.Values (Index).Count /= 0 then Live_Count := Live_Count + 1; end if;
            pragma Loop_Invariant (Live_Count = Live_Value_Count (Store, Index));
            pragma Loop_Invariant
              (for all O in 1 .. Index =>
                 Store.Values (O).Count = Lease_Count (Store, O, Root_Capacity));
         end loop;
         return Store.Live_Objects = Live_Count;
      end Check;
      function Find_Root (Store : State) return Root_Index is
      begin
         for Index in Root_Slot loop
            if Store.Leases (Index).Object = 0 then return Index; end if;
            pragma Loop_Invariant
              (for all I in 1 .. Index => Store.Leases (I).Object /= 0);
         end loop;
         return 0;
      end Find_Root;
      function Find_Object (Store : State) return Object_Index is
      begin
         for Index in Object_Slot loop
            if Store.Values (Index).Count = 0 then return Index; end if;
            pragma Loop_Invariant
              (for all I in 1 .. Index => Store.Values (I).Count /= 0);
         end loop;
         return 0;
      end Find_Object;
      procedure Value_Witness
        (Store : State; Object : Object_Slot; Through : Object_Index) is
      begin
         if Object < Through then Value_Witness (Store, Object, Through - 1); end if;
      end Value_Witness;
      procedure Lease_Witness
        (Store : State; Root : Root_Slot; Object : Object_Slot;
         Through : Root_Index) is
      begin
         if Root < Through then Lease_Witness (Store, Root, Object, Through - 1); end if;
      end Lease_Witness;
      procedure Value_Update
        (Before, After : State; Object : Object_Slot; Through : Object_Index) is
      begin
         if Through > 0 then Value_Update (Before, After, Object, Through - 1); end if;
      end Value_Update;
      procedure Lease_Update
        (Before, After : State; Root : Root_Slot; Object : Object_Slot;
         Through : Root_Index) is
      begin
         if Through > 0 then Lease_Update (Before, After, Root, Object, Through - 1); end if;
      end Lease_Update;
      procedure Unchanged_Model (Before, After : State) is
      begin
         Value_Update (Before, After, Object_Slot'First, Object_Capacity);
         for O in Object_Slot loop
            Lease_Update (Before, After, Root_Slot'First, O, Root_Capacity);
            pragma Assert (After.Values (O).Count = Lease_Count (After, O, Root_Capacity));
            pragma Loop_Invariant (for all I in 1 .. O =>
              After.Values (I).Count = Lease_Count (After, I, Root_Capacity));
         end loop;
      end Unchanged_Model;
      procedure Establish_Update
        (Before, After : State; Root : Root_Slot; Object : Object_Slot) is
      begin
         Value_Update (Before, After, Object, Object_Capacity);
         for O in Object_Slot loop
            Lease_Update (Before, After, Root, O, Root_Capacity);
            pragma Assert (After.Values (O).Count = Lease_Count (After, O, Root_Capacity));
            pragma Loop_Invariant (for all I in 1 .. O =>
              After.Values (I).Count = Lease_Count (After, I, Root_Capacity));
         end loop;
         pragma Assert (After.Live_Objects = Live_Value_Count (After, Object_Capacity));
      end Establish_Update;
      function Selected (Items : Handle_Array; Through : Natural; Root : Root_Slot)
        return Boolean is
        (Through > 0 and then (for some I in Items'First .. Items'First + (Through - 1) =>
          Items (I).Slot = Root));
      function Removed_Count
        (Prior : State; Items : Handle_Array; Through : Natural; Object : Object_Slot)
        return Natural is
        (if Through = 0 then 0 else Removed_Count (Prior, Items, Through - 1, Object)
          + Boolean'Pos (Live (Prior, Items (Items'First + (Through - 1))) and then
              Prior.Leases (Items (Items'First + (Through - 1)).Slot).Object = Object));
      procedure Removed_Count_Extends
        (Prior : State; Items : Handle_Array; Through, Total : Natural;
         Object : Object_Slot) is
      begin
         if Total > Through + 1 then
            Removed_Count_Extends (Prior, Items, Through, Total - 1, Object);
            pragma Assert (Removed_Count (Prior, Items, Total - 1, Object)
              <= Removed_Count (Prior, Items, Total, Object));
         else
            pragma Assert (Removed_Count (Prior, Items, Total, Object)
              = Removed_Count (Prior, Items, Through, Object) + 1);
         end if;
      end Removed_Count_Extends;
      function Dropped_Prefix
        (Store, Prior : State; Items : Handle_Array; Through : Natural) return Boolean is
        (Store.Domain = Prior.Domain and then Store.Last = Prior.Last
         and then (for all R in Root_Slot =>
           (if Selected (Items, Through, R) then Store.Leases (R) = Root_Entry'(others => <>)
            else Store.Leases (R) = Prior.Leases (R)))
         and then (for all O in Object_Slot =>
           Removed_Count (Prior, Items, Through, O) <= Prior.Values (O).Count and then
           (if Removed_Count (Prior, Items, Through, O) = 0 then Store.Values (O) = Prior.Values (O)
            elsif Removed_Count (Prior, Items, Through, O) = Prior.Values (O).Count then
              Store.Values (O) = Scalar'(others => <>)
            else Store.Values (O) = Scalar'(Prior.Values (O).Value, Prior.Values (O).Origin,
              Prior.Values (O).Count - Removed_Count (Prior, Items, Through, O))))
         and then Store.Live_Objects = Live_Value_Count (Store, Object_Capacity));
      procedure Zero_Values (Store : State; Through : Object_Index) is
      begin
         if Through > 0 then Zero_Values (Store, Through - 1); end if;
      end Zero_Values;
      procedure Zero_Leases (Store : State; Object : Object_Slot; Through : Root_Index) is
      begin
         if Through > 0 then Zero_Leases (Store, Object, Through - 1); end if;
      end Zero_Leases;
      procedure Lease_From_Model (Store : State; Root : Root_Slot) is
      begin
         pragma Assert (Lease_Valid (Store, Root));
      end Lease_From_Model;
      procedure Observe_State (Store : State) is
         Checked : constant Boolean := Check (Store) with Ghost;
      begin
         pragma Assert (Checked = Valid (Store));
         pragma Assert (Checked = Model (Store));
         pragma Assert (Store.Live_Objects = Live_Value_Count (Store, Object_Capacity));
         for O in Object_Slot loop
            pragma Assert (Store.Values (O).Count = Lease_Count (Store, O, Root_Capacity));
            pragma Loop_Invariant (for all I in 1 .. O =>
              Store.Values (I).Count = Lease_Count (Store, I, Root_Capacity));
         end loop;
         for R in Root_Slot loop
            Lease_From_Model (Store, R);
            pragma Assert (Lease_Valid (Store, R));
            Metadata_Prefix_Step (Store, R);
            pragma Loop_Invariant (Metadata_Prefix (Store, R));
            pragma Loop_Invariant (for all I in 1 .. R => Lease_Valid (Store, I));
         end loop;
         Metadata_Complete (Store, Root_Capacity);
      end Observe_State;
      procedure Observe_Roots (Store : State) is
         Observed : constant Natural := Roots (Store) with Ghost;
      begin
         pragma Assert (Observed = Root_Count (Store, Root_Capacity));
      end Observe_Roots;
      procedure Root_Update
        (Before, After : State; Root : Root_Slot; Through : Root_Index) is
      begin
         if Through > 0 then Root_Update (Before, After, Root, Through - 1); end if;
      end Root_Update;
      procedure Metadata_Frame
        (Before, After : State; Changed : Root_Index) is
      begin
         for R in Root_Slot loop
            if R /= Changed then
               Lease_From_Model (Before, R);
               pragma Assert (Lease_Valid (Before, R));
               pragma Assert (Before.Leases (R) = After.Leases (R));
            end if;
            pragma Assert (Lease_Valid (After, R));
            Metadata_Prefix_Step (After, R);
            Metadata_Complete (After, R);
            pragma Loop_Invariant (Metadata_Prefix (After, R));
            pragma Loop_Invariant (for all I in 1 .. R => Lease_Valid (After, I));
         end loop;
         Metadata_Complete (After, Root_Capacity);
      end Metadata_Frame;
      function Allocation_Frame (Store, Prior : State) return Boolean is
        (Store.Domain = Prior.Domain and then
         (for all R in Root_Slot =>
           (if Prior.Leases (R).Object /= 0 then
             Store.Leases (R) = Prior.Leases (R)
             and then Store.Values (Prior.Leases (R).Object) =
               Prior.Values (Prior.Leases (R).Object))));
      procedure Selected_Step
        (Items : Handle_Array; Through : Natural; Root : Root_Slot) is
      begin
         if Selected (Items, Through, Root) then
            pragma Assert (Selected (Items, Through + 1, Root));
         elsif Items (Items'First + Through).Slot = Root then
            pragma Assert (Selected (Items, Through + 1, Root));
         else
            pragma Assert (not Selected (Items, Through + 1, Root));
         end if;
      end Selected_Step;
      procedure Selected_All (Items : Handle_Array; Through : Natural) is
      begin
         for R in Root_Slot loop
            Selected_Step (Items, Through, R);
            pragma Loop_Invariant (for all I in 1 .. R => Selected (Items, Through + 1, I) =
              (Selected (Items, Through, I) or else Items (Items'First + Through).Slot = I));
         end loop;
      end Selected_All;
      procedure Prefix_Base (Store : State; Items : Handle_Array) is
      begin
         for R in Root_Slot loop
            pragma Assert (not Selected (Items, 0, R));
            pragma Loop_Invariant (for all I in 1 .. R => not Selected (Items, 0, I));
         end loop;
         for O in Object_Slot loop
            pragma Assert (Removed_Count (Store, Items, 0, O) = 0);
            pragma Loop_Invariant (for all I in 1 .. O => Removed_Count (Store, Items, 0, I) = 0);
         end loop;
      end Prefix_Base;
      procedure Single_Drop
        (Before, After : State; Items : Handle_Array) is
         Object : constant Object_Slot := Before.Leases (Items (Items'First).Slot).Object with Ghost;
      begin
         for O in Object_Slot loop
            pragma Assert (Removed_Count (Before, Items, 1, O) = Boolean'Pos (O = Object));
            pragma Loop_Invariant (for all I in 1 .. O =>
              Removed_Count (Before, Items, 1, I) = Boolean'Pos (I = Object));
         end loop;
         pragma Assert (Dropped_Prefix (After, Before, Items, 1));
      end Single_Drop;
      function Retired_Count (Before, After : State; Through : Object_Index)
        return Natural is
        (if Through = 0 then 0 else Retired_Count (Before, After, Through - 1)
          + Boolean'Pos (Before.Values (Through).Count /= 0 and then After.Values (Through).Count = 0));
      procedure Retired_Balance (Before, After : State; Through : Object_Index) is
      begin
         if Through > 0 then Retired_Balance (Before, After, Through - 1); end if;
      end Retired_Balance;
      procedure Finish_Drops
        (Store, Prior : State; Items : Handle_Array) is
         Seen : array (Root_Slot) of Boolean := [others => False];
         Removed : array (Object_Slot) of Natural := [others => 0];
         Retired : Natural := 0;
      begin
         for Index in Items'Range loop
            Selected_All (Items, Index - Items'First);
            Seen (Items (Index).Slot) := True;
            Removed (Prior.Leases (Items (Index).Slot).Object) :=
              Removed (Prior.Leases (Items (Index).Slot).Object) + 1;
            pragma Loop_Invariant (for all R in Root_Slot =>
              Seen (R) = Selected (Items, Index - Items'First + 1, R));
            pragma Loop_Invariant (for all O in Object_Slot =>
              Removed (O) = Removed_Count (Prior, Items, Index - Items'First + 1, O));
         end loop;
         for O in Object_Slot loop
            pragma Assert (Removed (O) <= Prior.Values (O).Count);
            pragma Assert (if Store.Values (O).Count /= 0 then Prior.Values (O).Count /= 0);
            if Removed (O) /= 0 and then Removed (O) = Prior.Values (O).Count then
               Retired := Retired + 1;
            end if;
            pragma Loop_Invariant (Retired = Retired_Count (Prior, Store, O));
            pragma Loop_Invariant (for all I in 1 .. O =>
              (if Store.Values (I).Count /= 0 then Prior.Values (I).Count /= 0));
         end loop;
         Retired_Balance (Prior, Store, Object_Capacity);
         pragma Assert (Retired <= Prior.Live_Objects);
         pragma Assert (Store.Live_Objects = Prior.Live_Objects - Retired);
         -- Prove the exact checker result, not an assumed opaque predicate.
         pragma Assert (Dropped_Model (Store, Prior, Items));
         declare Checked : constant Boolean := Check_Dropped (Store, Prior, Items) with Ghost; begin
            pragma Assert (Checked);
            pragma Assert (Dropped_Exactly (Store, Prior, Items));
         end;
      end Finish_Drops;
      procedure Prefix_Step
        (Prior, Before, After : State; Items : Handle_Array; Through : Natural) is
         Item : constant Handle := Items (Items'First + Through) with Ghost;
         Object : constant Object_Slot := Prior.Leases (Item.Slot).Object with Ghost;
      begin
         -- This next lease was not selected by the preceding prefix.
         pragma Assert (Before.Leases (Item.Slot) = Prior.Leases (Item.Slot));
         for R in Root_Slot loop
            Selected_Step (Items, Through, R);
            pragma Assert
              (if Selected (Items, Through + 1, R) then
                 After.Leases (R) = Root_Entry'(others => <>)
               else After.Leases (R) = Prior.Leases (R));
            pragma Loop_Invariant (for all I in 1 .. R =>
              (if Selected (Items, Through + 1, I) then
                 After.Leases (I) = Root_Entry'(others => <>)
               else After.Leases (I) = Prior.Leases (I)));
         end loop;
         for O in Object_Slot loop
            -- Prefix accounting followed by the authenticated single drop.
            pragma Assert (Before.Values (O).Count = Prior.Values (O).Count
              - Removed_Count (Prior, Items, Through, O));
            pragma Assert (After.Values (O).Count = Before.Values (O).Count
              - Boolean'Pos (O = Object));
            pragma Assert (Removed_Count (Prior, Items, Through + 1, O) =
              Removed_Count (Prior, Items, Through, O) + Boolean'Pos (O = Object));
            pragma Assert (Removed_Count (Prior, Items, Through + 1, O) <= Prior.Values (O).Count);
            pragma Loop_Invariant (for all I in 1 .. O =>
              Removed_Count (Prior, Items, Through + 1, I) <= Prior.Values (I).Count);
         end loop;
      end Prefix_Step;
      procedure Establish_Valid (Store : State) is
         Checked : constant Boolean := Check (Store) with Ghost;
      begin
         -- This real call instantiates Check's proved equivalence contract.
         -- Each step remains a proof obligation; none is an assumption.
         pragma Assert (Checked = Model (Store));
         pragma Assert (Checked);
         pragma Assert (Valid (Store));
      end Establish_Valid;
      procedure Establish_Zero (Store : State) is
      begin
         Zero_Values (Store, Object_Capacity);
         for O in Object_Slot loop
            Zero_Leases (Store, O, Root_Capacity);
            pragma Loop_Invariant (for all I in 1 .. O => Lease_Count (Store, I, Root_Capacity) = 0);
         end loop;
      end Establish_Zero;
      function Metadata_Prefix (Store : State; Through : Root_Index)
        return Boolean is
        (Through = 0 or else
          (Metadata_Prefix (Store, Through - 1) and then Lease_Valid (Store, Through)));
      procedure Metadata_Prefix_Step (Store : State; Through : Root_Slot) is
      begin
         pragma Assert (Metadata_Prefix (Store, Through));
      end Metadata_Prefix_Step;
      procedure Metadata_Complete (Store : State; Through : Root_Index) is
      begin
         if Through > 0 then
            Metadata_Complete (Store, Through - 1);
            pragma Assert (Lease_Valid (Store, Through));
         end if;
      end Metadata_Complete;
      procedure Root_Metadata (Store : State; Item : Handle) is
      begin
         Establish_Valid (Store);
         Observe_State (Store);
         pragma Assert (Lease_Valid (Store, Item.Slot));
         pragma Assert (Item.Serial = Store.Leases (Item.Slot).Serial);
         Lease_Witness (Store, Item.Slot, Store.Leases (Item.Slot).Object, Root_Capacity);
         pragma Assert (Store.Values (Store.Leases (Item.Slot).Object).Count =
           Lease_Count (Store, Store.Leases (Item.Slot).Object, Root_Capacity));
      end Root_Metadata;
      procedure Allocation_Preserved
        (Before, After : State; Root : Root_Slot; Object : Object_Slot) is
      begin
         for R in Root_Slot loop
            if Before.Leases (R).Object /= 0 then
               Lease_Witness (Before, R, Before.Leases (R).Object, Root_Capacity);
               pragma Assert (Before.Values (Before.Leases (R).Object).Count > 0);
               pragma Assert (Before.Leases (R).Object /= Object and then R /= Root);
               pragma Assert (After.Leases (R) = Before.Leases (R));
               pragma Assert (After.Values (Before.Leases (R).Object) =
                 Before.Values (Before.Leases (R).Object));
            end if;
            pragma Loop_Invariant (for all I in 1 .. R =>
              (if Before.Leases (I).Object /= 0 then
                After.Leases (I) = Before.Leases (I) and then
                After.Values (Before.Leases (I).Object) = Before.Values (Before.Leases (I).Object)));
         end loop;
      end Allocation_Preserved;
      function Valid_Selection
        (Prior : State; Items : Handle_Array; Through : Natural) return Boolean is
        (Through = 0 or else
          ((for all I in Items'First .. Items'First + (Through - 1) => Live (Prior, Items (I)))
           and then (for all I in Items'First .. Items'First + (Through - 1) =>
             (for all J in Items'First .. I - 1 => Items (I).Slot /= Items (J).Slot))));
      procedure Selection_Reject
        (Prior : State; Items : Handle_Array; Index : Positive) is
      begin
         if not Live (Prior, Items (Index)) then
            pragma Assert (not Valid_Selection (Prior, Items, Items'Length));
            return;
         end if;
         for I in Items'First .. Index - 1 loop
            if Items (I).Slot = Items (Index).Slot then
               pragma Assert (not Valid_Selection (Prior, Items, Items'Length));
               return;
            end if;
            pragma Loop_Invariant (for all J in Items'First .. I =>
              Items (J).Slot /= Items (Index).Slot);
         end loop;
         pragma Assert (not Selected (Items, Index - Items'First, Items (Index).Slot));
         pragma Assert (False);
      end Selection_Reject;
      function Fresh_Allocation
        (Store, Prior : State; Item : Handle) return Boolean is
        (Live (Store, Item)
         and then Prior.Leases (Item.Slot).Object = 0
         and then Prior.Values (Store.Leases (Item.Slot).Object).Count = 0);
      procedure Allocation_Keep_Live
        (Before, After : State; Source, New_Item : Handle) is
      begin
         Observe_State (Before);
         Root_Metadata (Before, Source);
         pragma Assert (After.Leases (Source.Slot) = Before.Leases (Source.Slot));
         pragma Assert (After.Domain = Before.Domain);
         pragma Assert (Before.Leases (Source.Slot).Object /=
           After.Leases (New_Item.Slot).Object);
         pragma Assert (After.Values (Before.Leases (Source.Slot).Object) =
           Before.Values (Before.Leases (Source.Slot).Object));
         pragma Assert (Read (After, Source) = Read (Before, Source));
      end Allocation_Keep_Live;
      procedure Selection_Witness
        (Items : Handle_Array; Through : Natural; Index : Positive) is
      begin
         pragma Assert (for some I in Items'First .. Items'First + (Through - 1) =>
           I = Index and then Items (I).Slot = Items (Index).Slot);
      end Selection_Witness;
      procedure Selection_Extend
        (Prior : State; Items : Handle_Array; Through : Natural) is
      begin
         if Through > 0 then
            for I in Items'First .. Items'First + (Through - 1) loop
               Selection_Witness (Items, Through, I);
               pragma Assert (Items (I).Slot /= Items (Items'First + Through).Slot);
               pragma Loop_Invariant (for all J in Items'First .. I =>
                 Items (J).Slot /= Items (Items'First + Through).Slot);
            end loop;
         end if;
         pragma Assert (Valid_Selection (Prior, Items, Through + 1));
      end Selection_Extend;
      procedure Selection_Future
        (Prior : State; Items : Handle_Array; Through : Natural) is
      begin
         pragma Assert (Live (Prior, Items (Items'First + Through)));
         if Through > 0 then
            for I in Items'First .. Items'First + (Through - 1) loop
               pragma Assert (Items (I).Slot /= Items (Items'First + Through).Slot);
               pragma Loop_Invariant (for all J in Items'First .. I =>
                 Items (J).Slot /= Items (Items'First + Through).Slot);
            end loop;
         end if;
      end Selection_Future;
      function Dropped_Model (Store, Prior : State; Items : Handle_Array)
        return Boolean is
        (Store.Domain = Prior.Domain and then Store.Last = Prior.Last
         and then Valid_Selection (Prior, Items, Items'Length)
         and then (for all R in Root_Slot =>
           (if Selected (Items, Items'Length, R) then Store.Leases (R) = Root_Entry'(others => <>)
            else Store.Leases (R) = Prior.Leases (R)))
         and then (for all O in Object_Slot =>
           Removed_Count (Prior, Items, Items'Length, O) <= Prior.Values (O).Count and then
           (if Removed_Count (Prior, Items, Items'Length, O) = 0 then Store.Values (O) = Prior.Values (O)
            elsif Removed_Count (Prior, Items, Items'Length, O) = Prior.Values (O).Count then
              Store.Values (O) = Scalar'(others => <>)
            else Store.Values (O) = Scalar'(Prior.Values (O).Value, Prior.Values (O).Origin,
              Prior.Values (O).Count - Removed_Count (Prior, Items, Items'Length, O))))
         and then Retired_Count (Prior, Store, Object_Capacity) <= Prior.Live_Objects
         and then Store.Live_Objects = Prior.Live_Objects - Retired_Count (Prior, Store, Object_Capacity));
      function Check_Dropped
        (Store, Prior : State; Items : Handle_Array) return Boolean
      is
         Selected : array (Root_Slot) of Boolean := [others => False];
         Removed : array (Object_Slot) of Owner_Count := [others => 0];
         Retired : Object_Index := 0;
      begin
         if Store.Domain /= Prior.Domain or else Store.Last /= Prior.Last then pragma Assert (not Dropped_Model (Store, Prior, Items)); return False; end if;
         for Position in Items'Range loop
            if not Live (Prior, Items (Position)) or else Selected (Items (Position).Slot) then
               Selection_Reject (Prior, Items, Position);
               pragma Assert (not Dropped_Model (Store, Prior, Items));
               return False;
            end if;
            Selected (Items (Position).Slot) := True;
            declare Object : constant Object_Slot := Prior.Leases (Items (Position).Slot).Object; begin
               if Removed (Object) = Owner_Count'Last then
                  Removed_Count_Extends
                    (Prior, Items, Position - Items'First, Items'Length, Object);
                  pragma Assert (not Dropped_Model (Store, Prior, Items));
                  return False;
               end if;
               Removed (Object) := Removed (Object) + 1;
            end;
            pragma Loop_Invariant (Valid_Selection (Prior, Items, Position - Items'First + 1));
            pragma Loop_Invariant (for all R in Root_Slot =>
              Selected (R) = Validation.Selected (Items, Position - Items'First + 1, R));
            pragma Loop_Invariant (for all O in Object_Slot =>
              Removed (O) = Removed_Count (Prior, Items, Position - Items'First + 1, O));
         end loop;
         for Index in Root_Slot loop
            if Selected (Index) then
               if Store.Leases (Index) /= Root_Entry'(others => <>) then pragma Assert (not Dropped_Model (Store, Prior, Items)); return False; end if;
            elsif Store.Leases (Index) /= Prior.Leases (Index) then pragma Assert (not Dropped_Model (Store, Prior, Items)); return False;
            end if;
            pragma Loop_Invariant (for all R in 1 .. Index =>
              (if Validation.Selected (Items, Items'Length, R) then
                Store.Leases (R) = Root_Entry'(others => <>) else Store.Leases (R) = Prior.Leases (R)));
         end loop;
         for Index in Object_Slot loop
            if Removed (Index) = 0 then
               if Store.Values (Index) /= Prior.Values (Index) then pragma Assert (not Dropped_Model (Store, Prior, Items)); return False; end if;
            elsif Removed (Index) > Prior.Values (Index).Count then pragma Assert (not Dropped_Model (Store, Prior, Items)); return False;
            elsif Removed (Index) = Prior.Values (Index).Count then
               if Store.Values (Index) /= Scalar'(others => <>) then pragma Assert (not Dropped_Model (Store, Prior, Items)); return False; end if;
               Retired := Retired + 1;
            elsif Store.Values (Index) /= Scalar'
              (Prior.Values (Index).Value, Prior.Values (Index).Origin,
               Prior.Values (Index).Count - Removed (Index)) then pragma Assert (not Dropped_Model (Store, Prior, Items)); return False;
            end if;
            pragma Loop_Invariant (Retired = Retired_Count (Prior, Store, Index));
            pragma Loop_Invariant (for all O in 1 .. Index =>
              Removed (O) <= Prior.Values (O).Count and then
              (if Removed (O) = 0 then Store.Values (O) = Prior.Values (O)
               elsif Removed (O) = Prior.Values (O).Count then Store.Values (O) = Scalar'(others => <>)
               else Store.Values (O) = Scalar'(Prior.Values (O).Value, Prior.Values (O).Origin,
                 Prior.Values (O).Count - Removed (O))));
         end loop;
         pragma Assert (Retired = Retired_Count (Prior, Store, Object_Capacity));
         return Retired <= Prior.Live_Objects
           and then Store.Live_Objects = Prior.Live_Objects - Retired;
      end Check_Dropped;
   end Validation;
   function Valid (Store : State) return Boolean is (Validation.Check (Store));
   function Live (Store : State; Item : Handle) return Boolean is
     (AML_Frame_Handles.Has_Authority (Store.Domain)
      and then Item.Domain = Store.Domain and then Item.Slot /= 0
      and then Item.Serial /= 0
      and then Store.Leases (Item.Slot).Serial = Item.Serial
      and then Store.Leases (Item.Slot).Object /= 0);
   function Same_Object (Store : State; Left, Right : Handle) return Boolean is
     (Live (Store, Left) and then Live (Store, Right)
      and then Store.Leases (Left.Slot).Object = Store.Leases (Right.Slot).Object);
   function Lease_Position (Store : State; Item : Handle) return Natural is
     (if Live (Store, Item) then Item.Slot else 0);
   function Owners (Store : State; Item : Handle) return Natural is
     (if Live (Store, Item) then
        Store.Values (Store.Leases (Item.Slot).Object).Count else 0);
   function Reading_Model (Store : State; Item : Handle) return Reading is
     (if Live (Store, Item) then
        Reading'(Outcome => Ready,
          Value => Store.Values (Store.Leases (Item.Slot).Object).Value,
          Origin => Store.Values (Store.Leases (Item.Slot).Object).Origin)
      else Reading'(others => <>));
   function Read (Store : State; Item : Handle) return Reading
     with Refined_Post => Read'Result = Reading_Model (Store, Item)
   is
   begin
      if not Live (Store, Item) then return (others => <>); end if;
      declare Value : constant Scalar := Store.Values (Store.Leases (Item.Slot).Object); begin
         return (Ready, Value.Value, Value.Origin);
      end;
   end Read;
   function Free_Root (Store : State) return Root_Index is
     (Validation.Find_Root (Store))
     with Annotate => (GNATprove, Inline_For_Proof);
   function Free_Object (Store : State) return Object_Index is
     (Validation.Find_Object (Store))
     with Annotate => (GNATprove, Inline_For_Proof);
   procedure Share
     (Store : in out State; Source : Handle;
      Item : out Handle; Outcome : out Status)
   is
      Root : constant Root_Index := Free_Root (Store);
      Object : Object_Index;
   begin
      declare
         pragma Assertion_Policy (Ghost => Ignore, Assert => Ignore, Loop_Invariant => Ignore);
         Before : constant State := Store with Ghost;
      begin
      Validation.Observe_State (Store);
      Validation.Observe_Roots (Store);
      Item := No_Handle;
      if not Live (Store, Source) then Outcome := Invalid_Handle;
      elsif Store.Last = Generation'Last then Outcome := Generation_Exhausted;
      elsif Root = 0 then Outcome := Root_Limit;
      else
         Object := Store.Leases (Source.Slot).Object;
         Validation.Lease_Witness (Store, Root, Object, Root_Capacity);
         Validation.Lease_Witness (Store, Source.Slot, Object, Root_Capacity);
         pragma Assert (Store.Values (Object).Count in 1 .. Root_Capacity - 1);
         Store.Last := Store.Last + 1;
         Store.Values (Object).Count := Store.Values (Object).Count + 1;
         Store.Leases (Root) := (Object, Store.Last);
         Item := (Store.Domain, Root, Store.Last); Outcome := Ready;
         Validation.Metadata_Frame (Before, Store, Root);
         Validation.Root_Update (Before, Store, Root, Root_Capacity);
         Validation.Observe_Roots (Store);
         Validation.Establish_Update (Before, Store, Root, Object);
      end if;
      end;
   end Share;
   procedure Move
     (Store : in out State; Source : in out Handle;
      Item : out Handle; Outcome : out Status)
   is
   begin
      declare
         pragma Assertion_Policy (Ghost => Ignore, Assert => Ignore, Loop_Invariant => Ignore);
         Before : constant State := Store with Ghost;
      begin
      Validation.Observe_State (Store);
      Validation.Observe_Roots (Store);
      Item := No_Handle;
      if not Live (Store, Source) then Outcome := Invalid_Handle;
      elsif Store.Last = Generation'Last then Outcome := Generation_Exhausted;
      else
         Validation.Root_Metadata (Store, Source);
         pragma Assert (Source.Serial <= Store.Last);
         Store.Last := Store.Last + 1;
         Store.Leases (Source.Slot).Serial := Store.Last;
         Item := (Store.Domain, Source.Slot, Store.Last);
         Source := No_Handle; Outcome := Ready;
      end if;
      if Outcome = Ready then
         Validation.Metadata_Frame (Before, Store, Item.Slot);
         Validation.Root_Update (Before, Store, Item.Slot, Root_Capacity);
         Validation.Observe_Roots (Store);
         Validation.Unchanged_Model (Before, Store);
      end if;
      end;
   end Move;
   procedure Copy
     (Store : in out State; Source : Handle;
      Item : out Handle; Outcome : out Status)
   is
      Value : constant Reading := Read (Store, Source);
   begin
      declare
         pragma Assertion_Policy (Ghost => Ignore, Assert => Ignore);
         Before : constant State := Store with Ghost;
      begin
         Item := No_Handle;
         if Value.Outcome /= Ready then Outcome := Invalid_Handle;
         else
            New_Value (Store, Value.Value, Value.Origin, Item, Outcome);
            if Outcome = Ready then
               Validation.Allocation_Keep_Live (Before, Store, Source, Item);
            end if;
         end if;
      end;
   end Copy;
   procedure Drop
     (Store : in out State; Item : in out Handle; Outcome : out Status)
   is
      Object : Object_Index;
   begin
      declare
         pragma Assertion_Policy (Ghost => Ignore, Assert => Ignore, Loop_Invariant => Ignore);
         Before : constant State := Store with Ghost;
         Prior_Item : constant Handle := Item with Ghost;
      begin
      Validation.Observe_State (Store);
      Validation.Observe_Roots (Store);
      if not Live (Store, Item) then Outcome := Invalid_Handle; return; end if;
      Object := Store.Leases (Item.Slot).Object;
      Validation.Lease_Witness (Store, Item.Slot, Object, Root_Capacity);
      Validation.Value_Witness (Store, Object, Object_Capacity);
      pragma Assert (Store.Values (Object).Count > 0 and then Store.Live_Objects > 0);
      Store.Leases (Item.Slot) := (others => <>);
      Store.Values (Object).Count := Store.Values (Object).Count - 1;
      if Store.Values (Object).Count = 0 then
         Store.Values (Object) := (others => <>);
         Store.Live_Objects := Store.Live_Objects - 1;
      end if;
      Item := No_Handle; Outcome := Ready;
      Validation.Metadata_Frame (Before, Store, Prior_Item.Slot);
      Validation.Root_Update (Before, Store, Prior_Item.Slot, Root_Capacity);
      Validation.Observe_Roots (Store);
      Validation.Establish_Update (Before, Store, Prior_Item.Slot, Object);
      end;
   end Drop;
   function Dropped_Exactly
     (Store, Prior : State; Items : Handle_Array) return Boolean is
     (Validation.Check_Dropped (Store, Prior, Items));
   procedure Drop_Many
     (Store : in out State; Items : Handle_Array; Outcome : out Status)
   is
      Seen : array (Root_Slot) of Boolean := [others => False];
   begin
      declare
         pragma Assertion_Policy (Ghost => Ignore, Assert => Ignore, Loop_Invariant => Ignore);
         Prior : constant State := Store with Ghost;
      begin
         Validation.Observe_State (Store);
         Validation.Prefix_Base (Store, Items);
         Outcome := Invalid_Handle;
         pragma Assert (Validation.Valid_Selection (Prior, Items, 0));
         pragma Assert (for all R in Root_Slot => not Validation.Selected (Items, 0, R));
         if Items'Length > Root_Capacity then return; end if;
         for Index in Items'Range loop
            if not Live (Store, Items (Index)) or else Seen (Items (Index).Slot) then return; end if;
            Validation.Selection_Extend (Prior, Items, Index - Items'First);
            Validation.Selected_All (Items, Index - Items'First);
            Seen (Items (Index).Slot) := True;
            pragma Loop_Invariant (Validation.Valid_Selection (Prior, Items, Index - Items'First + 1));
            pragma Loop_Invariant (Store = Prior);
            pragma Loop_Invariant (for all I in Items'First .. Index => Live (Prior, Items (I)));
            pragma Loop_Invariant (for all I in Items'First .. Index =>
              (for all J in Items'First .. I - 1 => Items (I).Slot /= Items (J).Slot));
            pragma Loop_Invariant (for all R in Root_Slot =>
              Seen (R) = Validation.Selected (Items, Index - Items'First + 1, R));
         end loop;
         for Index in Items'Range loop
            pragma Loop_Invariant (Validation.Valid_Selection (Prior, Items, Items'Length));
            pragma Loop_Invariant (for all I in Items'Range => Live (Prior, Items (I)));
            pragma Loop_Invariant (for all I in Items'Range =>
              (for all J in Items'First .. I - 1 => Items (I).Slot /= Items (J).Slot));
            pragma Loop_Invariant (Validation.Model (Store));
            pragma Loop_Invariant
              (Validation.Dropped_Prefix (Store, Prior, Items, Index - Items'First));
            pragma Loop_Invariant (for all I in Index .. Items'Last =>
              Live (Store, Items (I)) and then
              Store.Leases (Items (I).Slot) = Prior.Leases (Items (I).Slot));
            declare
               Before : constant State := Store with Ghost;
               Object : constant Object_Slot := Store.Leases (Items (Index).Slot).Object;
            begin
               Validation.Observe_State (Store);
               Validation.Lease_Witness (Store, Items (Index).Slot, Object, Root_Capacity);
               Validation.Value_Witness (Store, Object, Object_Capacity);
               pragma Assert (Store.Values (Object).Count > 0 and then Store.Live_Objects > 0);
               Store.Leases (Items (Index).Slot) := (others => <>);
               Store.Values (Object).Count := Store.Values (Object).Count - 1;
               if Store.Values (Object).Count = 0 then
                  Store.Values (Object) := (others => <>);
                  Store.Live_Objects := Store.Live_Objects - 1;
               end if;
               Validation.Metadata_Frame (Before, Store, Items (Index).Slot);
               Validation.Establish_Update (Before, Store, Items (Index).Slot, Object);
               Validation.Single_Drop (Before, Store, Items (Index .. Index));
               Validation.Selection_Future (Prior, Items, Index - Items'First);
               Validation.Prefix_Step (Prior, Before, Store, Items, Index - Items'First);
            end;
            pragma Assert
              (Validation.Dropped_Prefix (Store, Prior, Items, Index - Items'First + 1));
         end loop;
         Validation.Finish_Drops (Store, Prior, Items);
         pragma Assert (Dropped_Exactly (Store, Prior, Items));
         Outcome := Ready;
      end;
   end Drop_Many;
   function Cleared (Store, Prior : State) return Boolean is
     (Store.Domain = Prior.Domain and then Store.Last = Prior.Last
      and then Roots (Store) = 0 and then Objects (Store) = 0);
   procedure Clear_All (Store : in out State) is
   begin
      declare
         pragma Assertion_Policy (Ghost => Ignore, Assert => Ignore);
      begin
      -- Last and Domain deliberately survive; no old lease can revive.
      Store.Leases := [others => <>];
      Store.Values := [others => <>];
      Store.Live_Objects := 0;
      Validation.Establish_Zero (Store);
      Validation.Establish_Valid (Store);
      Validation.Observe_Roots (Store);
      end;
   end Clear_All;
   procedure Set_Result_Width
     (Store : in out State; Item : Handle; Width : Result_Width;
      Outcome : out Status)
   is
      Bits_32_Mask : constant Number := 16#FFFF_FFFF#;
      Object : Object_Index;
   begin
      declare
         pragma Assertion_Policy (Ghost => Ignore, Assert => Ignore, Loop_Invariant => Ignore);
         Before : constant State := Store with Ghost;
      begin
      Validation.Observe_State (Store);
      Validation.Observe_Roots (Store);
      if not Live (Store, Item) then Outcome := Invalid_Handle; return; end if;
      Object := Store.Leases (Item.Slot).Object;
      if Width = Bits_32 then
         Store.Values (Object).Value := Store.Values (Object).Value and Bits_32_Mask;
      end if;
      Outcome := Ready;
      Validation.Metadata_Frame (Before, Store, 0);
      Validation.Root_Update (Before, Store, Root_Slot'First, Root_Capacity);
      Validation.Observe_Roots (Store);
      Validation.Unchanged_Model (Before, Store);
      end;
   end Set_Result_Width;
   pragma Assertion_Policy (Ghost => Ignore, Refined_Post => Ignore);
   function Roots (Store : State) return Natural is
      pragma Assertion_Policy (Ghost => Ignore, Refined_Post => Ignore);
      pragma Refined_Post
        (Roots'Result = Validation.Root_Count (Store, Root_Capacity));
   begin
      return Validation.Count_Roots (Store);
   end Roots;
   procedure New_Value
     (Store : in out State; Value : Number; Origin : Integer_Origin;
      Item : out Handle; Outcome : out Status)
   is
      pragma Assertion_Policy (Ghost => Ignore, Refined_Post => Ignore);
      pragma Refined_Post
        ((Valid (Store) and then
       Invocation_Domain (Store) = Invocation_Domain (Store'Old) and then
       (if Outcome = Ready then Live (Store, Item)
          and then Read (Store, Item).Value = Value
          and then Read (Store, Item).Origin = Origin
          and then Owners (Store, Item) = 1
          and then Objects (Store) = Objects (Store'Old) + 1
          and then Roots (Store) = Roots (Store'Old) + 1
        else Store = Store'Old and then Item = No_Handle))
         and then (if Outcome = Ready then
           Validation.Allocation_Frame (Store, Store'Old)
           and then Validation.Fresh_Allocation (Store, Store'Old, Item)));
      Root : constant Root_Index := Free_Root (Store);
      Object : constant Object_Index := Free_Object (Store);
   begin
      declare
         pragma Assertion_Policy (Ghost => Ignore, Assert => Ignore, Loop_Invariant => Ignore);
         Before : constant State := Store with Ghost;
      begin
      Validation.Observe_State (Store);
      Validation.Observe_Roots (Store);
      Item := No_Handle;
      if not AML_Frame_Handles.Has_Authority (Store.Domain) then Outcome := Invalid_Domain;
      elsif Store.Last = Generation'Last then Outcome := Generation_Exhausted;
      elsif Root = 0 then Outcome := Root_Limit;
      elsif Object = 0 then Outcome := Object_Limit;
      else
         pragma Assert (Before.Leases (Root).Object = 0);
         pragma Assert (Before.Values (Object).Count = 0);
         Validation.Value_Witness (Store, Object, Object_Capacity);
         Validation.Lease_Witness (Store, Root, Object, Root_Capacity);
         pragma Assert (Store.Live_Objects < Object_Capacity);
         Store.Last := Store.Last + 1;
         Store.Values (Object) := (Value, Origin, 1);
         Store.Live_Objects := Store.Live_Objects + 1;
         Store.Leases (Root) := (Object, Store.Last);
         Item := (Store.Domain, Root, Store.Last); Outcome := Ready;
         pragma Assert (Item.Slot = Root);
         pragma Assert (Store.Leases (Item.Slot).Object = Object);
      end if;
      if Outcome = Ready then
         Validation.Allocation_Preserved (Before, Store, Root, Object);
         Validation.Metadata_Frame (Before, Store, Root);
         Validation.Root_Update (Before, Store, Root, Root_Capacity);
         Validation.Observe_Roots (Store);
         Validation.Establish_Update (Before, Store, Root, Object);
         pragma Assert (Validation.Fresh_Allocation (Store, Before, Item));
      end if;
      end;
   end New_Value;
end AML_Scalar_Ledger;
