with Interfaces;
with AML_Frame_Handles;
generic
   Object_Capacity : Positive;
   Root_Capacity : Positive;
   Generation_Limit : Positive := Natural'Last;
package AML_Scalar_Ledger with SPARK_Mode, Pure is
   subtype Number is Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_64;
   use type AML_Frame_Handles.Invocation_Domain;
   type Integer_Origin is (Ordinary, AML_Constant);
   type Result_Width is (Bits_32, Bits_64);
   type Status is (Ready, Invalid_Domain, Invalid_Handle,
                   Object_Limit, Root_Limit, Generation_Exhausted);
   type Handle is private;
   No_Handle : constant Handle;
   type State is private;
   -- The caller owns a fresh invocation domain and must not restore old State
   -- snapshots while handles from a newer State are still externally live.
   function Empty (Domain : AML_Frame_Handles.Invocation_Domain) return State;
   function Valid (Store : State) return Boolean with
     Annotate => (GNATprove, Inline_For_Proof);
   function Invocation_Domain (Store : State) return AML_Frame_Handles.Invocation_Domain;
   function Initialized
     (Store : State; Domain : AML_Frame_Handles.Invocation_Domain) return Boolean with Ghost;
   -- Fresh construction only, with the same fresh-domain obligation as Empty.
   -- This is not teardown/reset: reusing a domain could revive copied tokens.
   procedure Initialize
     (Store : out State; Domain : AML_Frame_Handles.Invocation_Domain)
     with Post => Valid (Store) and then Initialized (Store, Domain);
   function Live (Store : State; Item : Handle) return Boolean;
   function Same_Object (Store : State; Left, Right : Handle) return Boolean;
   -- Cached cardinality; Valid independently checks the exact live-value count.
   function Objects (Store : State) return Natural;
   function Roots (Store : State) return Natural with
     Post => Roots'Result <= Root_Capacity;
   function Owners (Store : State; Item : Handle) return Natural;
   -- Read-only census position, not a constructor or transferable authority.
   function Lease_Position (Store : State; Item : Handle) return Natural with
     Post => Lease_Position'Result <= Root_Capacity
       and then (if Live (Store, Item) then Lease_Position'Result > 0
                 else Lease_Position'Result = 0);
   type Reading is record
      Outcome : Status := Invalid_Handle;
      Value : Number := 0;
      Origin : Integer_Origin := Ordinary;
   end record;
   function Read (Store : State; Item : Handle) return Reading with
     Post => (Read'Result.Outcome = Ready) = Live (Store, Item);
   procedure New_Value
     (Store : in out State; Value : Number; Origin : Integer_Origin;
      Item : out Handle; Outcome : out Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       Invocation_Domain (Store) = Invocation_Domain (Store'Old) and then
       (if Outcome = Ready then Live (Store, Item)
          and then Read (Store, Item).Value = Value
          and then Read (Store, Item).Origin = Origin
          and then Owners (Store, Item) = 1
          and then Objects (Store) = Objects (Store'Old) + 1
          and then Roots (Store) = Roots (Store'Old) + 1
        else Store = Store'Old and then Item = No_Handle);
   procedure Share
     (Store : in out State; Source : Handle;
      Item : out Handle; Outcome : out Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Outcome = Ready then Same_Object (Store, Source, Item)
          and then Owners (Store, Source) = Owners (Store'Old, Source) + 1
          and then Objects (Store) = Objects (Store'Old)
          and then Roots (Store) = Roots (Store'Old) + 1
        else Store = Store'Old and then Item = No_Handle);
   -- Move consumes the old lease, issuing a new generation in its same slot.
   -- Even a stale copy of the input token can no longer read/drop the root.
   procedure Move
     (Store : in out State; Source : in out Handle;
      Item : out Handle; Outcome : out Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Outcome = Ready then Source = No_Handle and then Live (Store, Item)
          and then not Live (Store, Source'Old)
          and then Read (Store, Item) = Read (Store'Old, Source'Old)
          and then Owners (Store, Item) = Owners (Store'Old, Source'Old)
          and then Objects (Store) = Objects (Store'Old)
          and then Roots (Store) = Roots (Store'Old)
        else Store = Store'Old and then Source = Source'Old and then Item = No_Handle);
   procedure Copy
     (Store : in out State; Source : Handle;
      Item : out Handle; Outcome : out Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Outcome = Ready then Live (Store, Source) and then Live (Store, Item)
          and then not Same_Object (Store, Source, Item)
          and then Read (Store, Item) = Read (Store'Old, Source)
          and then Read (Store, Source) = Read (Store'Old, Source)
          and then Owners (Store, Item) = 1
          and then Objects (Store) = Objects (Store'Old) + 1
          and then Roots (Store) = Roots (Store'Old) + 1
        else Store = Store'Old and then Item = No_Handle);
   procedure Drop
     (Store : in out State; Item : in out Handle; Outcome : out Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Outcome = Ready then Item = No_Handle and then not Live (Store, Item'Old)
          and then Roots (Store) = Roots (Store'Old) - 1
          and then Objects (Store) = Objects (Store'Old) -
            (if Owners (Store'Old, Item'Old) = 1 then 1 else 0)
        else Store = Store'Old and then Item = Item'Old);
   type Handle_Array is array (Positive range <>) of Handle;
   function Dropped_Exactly
     (Store, Prior : State; Items : Handle_Array) return Boolean with Ghost,
     Annotate => (GNATprove, Inline_For_Proof);

   -- Every handle must be live and distinct. Reject the entire selection before
   -- mutation on a stale, foreign, repeated, or null handle; [] is a no-op.
   procedure Drop_Many
     (Store : in out State; Items : Handle_Array; Outcome : out Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Outcome = Ready then Dropped_Exactly (Store, Store'Old, Items)
        else Store = Store'Old);
   function Cleared (Store, Prior : State) return Boolean with Ghost;
   -- Whole-invocation teardown preserves the issuance generation/domain.
   procedure Clear_All (Store : in out State) with
     Pre => Valid (Store), Post => Valid (Store)
       and then Roots (Store) = 0 and then Objects (Store) = 0
       and then Cleared (Store, Store'Old);
   -- Mutates the object seen through every shared lease. Copy is independent.
   -- This operation is explicit; reads/copies do not silently truncate.
   procedure Set_Result_Width
     (Store : in out State; Item : Handle; Width : Result_Width;
      Outcome : out Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Outcome = Ready then Live (Store, Item)
          and then Read (Store, Item).Origin = Read (Store'Old, Item).Origin
          and then Read (Store, Item).Value =
            (if Width = Bits_64 then Read (Store'Old, Item).Value
             else Read (Store'Old, Item).Value mod 2 ** 32)
          and then Objects (Store) = Objects (Store'Old)
          and then Roots (Store) = Roots (Store'Old)
        else Store = Store'Old);
private
   subtype Object_Index is Natural range 0 .. Object_Capacity;
   subtype Object_Slot is Object_Index range 1 .. Object_Capacity;
   subtype Root_Index is Natural range 0 .. Root_Capacity;
   subtype Root_Slot is Root_Index range 1 .. Root_Capacity;
   subtype Generation is Natural range 0 .. Generation_Limit;
   subtype Owner_Count is Natural range 0 .. Root_Capacity;
   type Handle is record
      Domain : AML_Frame_Handles.Invocation_Domain := AML_Frame_Handles.No_Domain;
      Slot : Root_Index := 0;
      Serial : Generation := 0;
   end record;
   No_Handle : constant Handle := (others => <>);
   type Scalar is record
      Value : Number := 0;
      Origin : Integer_Origin := Ordinary;
      Count : Owner_Count := 0;
   end record;
   type Root_Entry is record
      Object : Object_Index := 0;
      Serial : Generation := 0;
   end record;
   type Object_Table is array (Object_Slot) of Scalar;
   type Root_Table is array (Root_Slot) of Root_Entry;
   type State is record
      Domain : AML_Frame_Handles.Invocation_Domain := AML_Frame_Handles.No_Domain;
      Last : Generation := 0;
      Live_Objects : Object_Index := 0;
      Values : Object_Table := [others => <>];
      Leases : Root_Table := [others => <>];
   end record;
   -- Inherit the profile Ghost policy: erased release, checked O(1) model.
   function Reading_Model (Store : State; Item : Handle) return Reading with Ghost;
   package Validation with SPARK_Mode is
      -- Only new proof models and their equivalence posts are erased here.
      -- All existing public operation contracts remain outside this scope.
      pragma Assertion_Policy (Ghost => Ignore, Post => Ignore);
      function Live_Value_Count (Store : State; Through : Object_Index)
        return Natural with Ghost, Subprogram_Variant => (Decreases => Through),
        Post => Live_Value_Count'Result <= Through;
      function Lease_Count (Store : State; Object : Object_Slot;
                            Through : Root_Index) return Natural with Ghost,
        Subprogram_Variant => (Decreases => Through),
        Post => Lease_Count'Result <= Through;
      function Root_Count (Store : State; Through : Root_Index)
        return Natural with Ghost, Subprogram_Variant => (Decreases => Through),
        Post => Root_Count'Result <= Through;
      function Lease_Valid (Store : State; Index : Root_Slot) return Boolean with Ghost;
      function Model (Store : State) return Boolean with Ghost;
      function Check (Store : State) return Boolean with
        Post => Check'Result = Model (Store);
      function Count_Roots (Store : State) return Natural with
        Post => Count_Roots'Result = Root_Count (Store, Root_Capacity)
          and then Count_Roots'Result <= Root_Capacity;
      function Find_Root (Store : State) return Root_Index with
        Post => (if Find_Root'Result /= 0 then
                   Store.Leases (Find_Root'Result).Object = 0
                 else (for all I in Root_Slot => Store.Leases (I).Object /= 0));
      function Find_Object (Store : State) return Object_Index with
        Post => (if Find_Object'Result /= 0 then
                   Store.Values (Find_Object'Result).Count = 0
                 else (for all I in Object_Slot => Store.Values (I).Count /= 0));
      procedure Value_Witness
        (Store : State; Object : Object_Slot; Through : Object_Index) with Ghost,
        Pre => Object <= Through, Subprogram_Variant => (Decreases => Through),
        Post => (if Store.Values (Object).Count = 0 then
                   Live_Value_Count (Store, Through) < Through
                 else Live_Value_Count (Store, Through) > 0);
      procedure Lease_Witness
        (Store : State; Root : Root_Slot; Object : Object_Slot;
         Through : Root_Index) with Ghost, Pre => Root <= Through,
        Subprogram_Variant => (Decreases => Through),
        Post => (if Store.Leases (Root).Object = 0 then
                   Lease_Count (Store, Object, Through) < Through
                   and then Root_Count (Store, Through) < Through
                 elsif Store.Leases (Root).Object = Object then
                   Lease_Count (Store, Object, Through) > 0);
      procedure Value_Update
        (Before, After : State; Object : Object_Slot; Through : Object_Index)
        with Ghost, Subprogram_Variant => (Decreases => Through),
        Pre => (for all O in Object_Slot =>
                  (if O /= Object then Before.Values (O).Count = After.Values (O).Count)),
        Post => (if Object > Through or else
                      (Before.Values (Object).Count = 0) = (After.Values (Object).Count = 0)
                 then Live_Value_Count (After, Through) = Live_Value_Count (Before, Through)
                 elsif Before.Values (Object).Count = 0 then
                   Live_Value_Count (Before, Through) < Through and then
                   Live_Value_Count (After, Through) = Live_Value_Count (Before, Through) + 1
                 else Live_Value_Count (After, Through) < Through and then
                   Live_Value_Count (Before, Through) = Live_Value_Count (After, Through) + 1);
      procedure Lease_Update
        (Before, After : State; Root : Root_Slot; Object : Object_Slot;
         Through : Root_Index) with Ghost,
        Subprogram_Variant => (Decreases => Through),
        Pre => (for all R in Root_Slot =>
                  (if R /= Root then Before.Leases (R).Object = After.Leases (R).Object)),
        Post => (if Root > Through or else
                      (Before.Leases (Root).Object = Object) = (After.Leases (Root).Object = Object)
                 then Lease_Count (After, Object, Through) = Lease_Count (Before, Object, Through)
                 elsif Before.Leases (Root).Object /= Object then
                   Lease_Count (Before, Object, Through) < Through and then
                   Lease_Count (After, Object, Through) = Lease_Count (Before, Object, Through) + 1
                 else Lease_Count (After, Object, Through) < Through and then
                   Lease_Count (Before, Object, Through) = Lease_Count (After, Object, Through) + 1);
      procedure Unchanged_Model (Before, After : State) with Ghost,
        Pre => Model (Before) and then Before.Domain = After.Domain
          and then Before.Live_Objects = After.Live_Objects
          and then (for all O in Object_Slot => Before.Values (O).Count = After.Values (O).Count)
          and then (for all R in Root_Slot => Before.Leases (R).Object = After.Leases (R).Object)
          and then (for all R in Root_Slot => Lease_Valid (After, R)),
        Post => Model (After);
      procedure Establish_Update
        (Before, After : State; Root : Root_Slot; Object : Object_Slot) with Ghost,
        Pre => Model (Before) and then Before.Domain = After.Domain
          and then (for all O in Object_Slot =>
            (if O /= Object then Before.Values (O).Count = After.Values (O).Count))
          and then (for all R in Root_Slot =>
            (if R /= Root then Before.Leases (R).Object = After.Leases (R).Object))
          and then (for all R in Root_Slot => Lease_Valid (After, R))
          and then (if Before.Leases (Root).Object = 0 then
             After.Leases (Root).Object = Object
             and then Before.Values (Object).Count < Root_Capacity
             and then After.Values (Object).Count = Before.Values (Object).Count + 1
           else Before.Leases (Root).Object = Object
             and then After.Leases (Root).Object = 0
             and then Before.Values (Object).Count > 0
             and then After.Values (Object).Count = Before.Values (Object).Count - 1)
          and then (if Before.Values (Object).Count = 0 then
             Before.Live_Objects < Object_Capacity
             and then After.Live_Objects = Before.Live_Objects + 1
           elsif After.Values (Object).Count = 0 then
             Before.Live_Objects > 0
             and then After.Live_Objects = Before.Live_Objects - 1
           else After.Live_Objects = Before.Live_Objects),
        Post => Model (After);
      function Selected (Items : Handle_Array; Through : Natural; Root : Root_Slot)
        return Boolean with Ghost, Pre => Through <= Items'Length,
        Post => (if Through = 0 then not Selected'Result);
      function Removed_Count
        (Prior : State; Items : Handle_Array; Through : Natural; Object : Object_Slot)
        return Natural with Ghost, Pre => Through <= Items'Length,
        Subprogram_Variant => (Decreases => Through),
        Post => Removed_Count'Result <= Through;
      procedure Removed_Count_Extends
        (Prior : State; Items : Handle_Array; Through, Total : Natural;
         Object : Object_Slot) with Ghost,
        Pre => Through < Total and then Total <= Items'Length
          and then Live (Prior, Items (Items'First + Through))
          and then Prior.Leases (Items (Items'First + Through).Slot).Object = Object,
        Post => Removed_Count (Prior, Items, Through, Object)
          < Removed_Count (Prior, Items, Total, Object),
        Subprogram_Variant => (Decreases => Total - Through);
      function Dropped_Prefix
        (Store, Prior : State; Items : Handle_Array; Through : Natural)
        return Boolean with Ghost, Pre => Through <= Items'Length;
      procedure Zero_Values (Store : State; Through : Object_Index) with Ghost,
        Pre => (for all O in Object_Slot => Store.Values (O).Count = 0),
        Subprogram_Variant => (Decreases => Through),
        Post => Live_Value_Count (Store, Through) = 0;
      procedure Zero_Leases (Store : State; Object : Object_Slot; Through : Root_Index)
        with Ghost, Pre => (for all R in Root_Slot => Store.Leases (R).Object = 0),
        Subprogram_Variant => (Decreases => Through),
        Post => Lease_Count (Store, Object, Through) = 0 and then Root_Count (Store, Through) = 0;
      procedure Lease_From_Model (Store : State; Root : Root_Slot) with Ghost,
        Pre => Model (Store), Post => Lease_Valid (Store, Root);
      procedure Observe_State (Store : State) with Ghost,
        Pre => Valid (Store),
        Post => Model (Store)
          and then Store.Live_Objects = Live_Value_Count (Store, Object_Capacity)
          and then (for all O in Object_Slot =>
            Store.Values (O).Count = Lease_Count (Store, O, Root_Capacity))
          and then (for all R in Root_Slot => Lease_Valid (Store, R));
      procedure Observe_Roots (Store : State) with Ghost,
        Post => Roots (Store) = Root_Count (Store, Root_Capacity);
      procedure Root_Update
        (Before, After : State; Root : Root_Slot; Through : Root_Index)
        with Ghost, Subprogram_Variant => (Decreases => Through),
        Pre => (for all R in Root_Slot =>
          (if R /= Root then Before.Leases (R).Object = After.Leases (R).Object)),
        Post => (if Root > Through or else
                    (Before.Leases (Root).Object = 0) = (After.Leases (Root).Object = 0)
          then Root_Count (After, Through) = Root_Count (Before, Through)
          elsif Before.Leases (Root).Object = 0 then
            Root_Count (Before, Through) < Through and then
            Root_Count (After, Through) = Root_Count (Before, Through) + 1
          else Root_Count (After, Through) < Through and then
            Root_Count (Before, Through) = Root_Count (After, Through) + 1);
      procedure Metadata_Frame
        (Before, After : State; Changed : Root_Index) with Ghost,
        Pre => Model (Before) and then Before.Domain = After.Domain
          and then Before.Last <= After.Last
          and then (for all R in Root_Slot =>
            (if R /= Changed then Before.Leases (R) = After.Leases (R)))
          and then (if Changed /= 0 then
            (if After.Leases (Changed).Object = 0 then After.Leases (Changed).Serial = 0
             else AML_Frame_Handles.Has_Authority (After.Domain)
               and then After.Leases (Changed).Serial /= 0
               and then After.Leases (Changed).Serial <= After.Last)),
        Post => (for all R in Root_Slot => Lease_Valid (After, R));
      function Allocation_Frame (Store, Prior : State) return Boolean with Ghost;
      procedure Selected_Step
        (Items : Handle_Array; Through : Natural; Root : Root_Slot) with Ghost,
        Pre => Through < Items'Length,
        Post => Selected (Items, Through + 1, Root) =
          (Selected (Items, Through, Root)
           or else Items (Items'First + Through).Slot = Root);
      procedure Selected_All (Items : Handle_Array; Through : Natural) with Ghost,
        Pre => Through < Items'Length,
        Post => (for all R in Root_Slot => Selected (Items, Through + 1, R) =
          (Selected (Items, Through, R) or else Items (Items'First + Through).Slot = R));
      procedure Prefix_Base (Store : State; Items : Handle_Array) with Ghost,
        Pre => Model (Store), Post => Dropped_Prefix (Store, Store, Items, 0);
      procedure Single_Drop
        (Before, After : State; Items : Handle_Array) with Ghost,
        Pre => Items'Length = 1 and then Model (Before) and then Model (After)
          and then Live (Before, Items (Items'First))
          and then Before.Domain = After.Domain and then Before.Last = After.Last
          and then (for all R in Root_Slot =>
            (if R = Items (Items'First).Slot then After.Leases (R) = Root_Entry'(others => <>)
             else After.Leases (R) = Before.Leases (R)))
          and then (for all O in Object_Slot =>
            (if O /= Before.Leases (Items (Items'First).Slot).Object then
               After.Values (O) = Before.Values (O)
             elsif Before.Values (O).Count = 1 then After.Values (O) = Scalar'(others => <>)
             else Before.Values (O).Count > 1 and then After.Values (O) = Scalar'
               (Before.Values (O).Value, Before.Values (O).Origin, Before.Values (O).Count - 1))),
        Post => Dropped_Prefix (After, Before, Items, 1);
      function Retired_Count (Before, After : State; Through : Object_Index)
        return Natural with Ghost, Subprogram_Variant => (Decreases => Through),
        Post => Retired_Count'Result <= Through;
      procedure Retired_Balance (Before, After : State; Through : Object_Index)
        with Ghost, Subprogram_Variant => (Decreases => Through),
        Pre => (for all O in Object_Slot =>
          (if After.Values (O).Count /= 0 then Before.Values (O).Count /= 0)),
        Post => Live_Value_Count (After, Through) <= Live_Value_Count (Before, Through)
          and then Retired_Count (Before, After, Through) =
            Live_Value_Count (Before, Through) - Live_Value_Count (After, Through);
      procedure Finish_Drops
        (Store, Prior : State; Items : Handle_Array) with Ghost,
        Pre => Model (Prior) and then Dropped_Prefix (Store, Prior, Items, Items'Length)
          and then Items'Length <= Root_Capacity
          and then (for all I in Items'Range => Live (Prior, Items (I)))
          and then (for all I in Items'Range =>
            (for all J in Items'First .. I - 1 => Items (I).Slot /= Items (J).Slot)),
        Post => Dropped_Exactly (Store, Prior, Items);
      procedure Prefix_Step
        (Prior, Before, After : State; Items : Handle_Array; Through : Natural)
        with Ghost,
        Pre => Through < Items'Length
          and then Dropped_Prefix (Before, Prior, Items, Through)
          and then Model (Before) and then Model (After)
          and then Live (Prior, Items (Items'First + Through))
          and then Live (Before, Items (Items'First + Through))
          and then not Selected (Items, Through, Items (Items'First + Through).Slot)
          and then Dropped_Prefix
            (After, Before, Items (Items'First + Through .. Items'First + Through), 1),
        Post => Dropped_Prefix (After, Prior, Items, Through + 1);
      procedure Establish_Valid (Store : State) with Ghost,
        Pre => Model (Store), Post => Valid (Store);
      procedure Establish_Zero (Store : State) with Ghost,
        Pre => Store.Live_Objects = 0
          and then (for all O in Object_Slot => Store.Values (O).Count = 0)
          and then (for all R in Root_Slot => Store.Leases (R) = Root_Entry'(others => <>)),
        Post => Model (Store) and then Root_Count (Store, Root_Capacity) = 0;
      function Metadata_Prefix (Store : State; Through : Root_Index)
        return Boolean with Ghost, Subprogram_Variant => (Decreases => Through);
      procedure Metadata_Prefix_Step (Store : State; Through : Root_Slot) with Ghost,
        Pre => Metadata_Prefix (Store, Through - 1) and then Lease_Valid (Store, Through),
        Post => Metadata_Prefix (Store, Through);
      procedure Metadata_Complete (Store : State; Through : Root_Index) with Ghost,
        Pre => Metadata_Prefix (Store, Through),
        Subprogram_Variant => (Decreases => Through),
        Post => (for all R in 1 .. Through => Lease_Valid (Store, R))
          and then (for all R in 1 .. Through =>
            (if Store.Leases (R).Object /= 0 then
               AML_Frame_Handles.Has_Authority (Store.Domain)
               and then Store.Leases (R).Serial /= 0
               and then Store.Leases (R).Serial <= Store.Last
             else Store.Leases (R).Serial = 0));
      procedure Root_Metadata (Store : State; Item : Handle) with Ghost,
        Pre => Model (Store) and then Live (Store, Item),
        Post => Item.Serial <= Store.Last and then
          Store.Values (Store.Leases (Item.Slot).Object).Count > 0;
      procedure Allocation_Preserved
        (Before, After : State; Root : Root_Slot; Object : Object_Slot) with Ghost,
        Pre => Model (Before) and then Before.Domain = After.Domain
          and then Before.Leases (Root).Object = 0
          and then Before.Values (Object).Count = 0
          and then (for all R in Root_Slot =>
            (if R /= Root then Before.Leases (R) = After.Leases (R)))
          and then (for all O in Object_Slot =>
            (if O /= Object then Before.Values (O) = After.Values (O))),
        Post => Allocation_Frame (After, Before);
      function Valid_Selection
        (Prior : State; Items : Handle_Array; Through : Natural) return Boolean
        with Ghost, Pre => Through <= Items'Length,
        Post => (if Through = 0 then Valid_Selection'Result
          elsif Valid_Selection'Result then
            (for all I in Items'First .. Items'First + (Through - 1) => Live (Prior, Items (I)))
            and then (for all I in Items'First .. Items'First + (Through - 1) =>
              (for all J in Items'First .. I - 1 => Items (I).Slot /= Items (J).Slot)));
      procedure Selection_Reject
        (Prior : State; Items : Handle_Array; Index : Positive) with Ghost,
        Pre => Index in Items'Range and then
          (not Live (Prior, Items (Index)) or else
            Selected (Items, Index - Items'First, Items (Index).Slot)),
        Post => not Valid_Selection (Prior, Items, Items'Length);
      function Fresh_Allocation
        (Store, Prior : State; Item : Handle) return Boolean with Ghost,
        Post => Fresh_Allocation'Result =
          (Live (Store, Item)
           and then Prior.Leases (Item.Slot).Object = 0
           and then Prior.Values (Store.Leases (Item.Slot).Object).Count = 0);
      procedure Allocation_Keep_Live
        (Before, After : State; Source, New_Item : Handle) with Ghost,
        Pre => Valid (Before) and then Live (Before, Source)
          and then Allocation_Frame (After, Before)
          and then Fresh_Allocation (After, Before, New_Item),
        Post => Live (After, Source)
          and then not Same_Object (After, Source, New_Item)
          and then Read (After, Source) = Read (Before, Source);
      procedure Selection_Witness
        (Items : Handle_Array; Through : Natural; Index : Positive) with Ghost,
        Pre => Through <= Items'Length and then Index in Items'Range
          and then Index - Items'First < Through
          and then Items (Index).Slot /= 0,
        Post => Selected (Items, Through, Items (Index).Slot);
      procedure Selection_Extend
        (Prior : State; Items : Handle_Array; Through : Natural) with Ghost,
        Pre => Through < Items'Length and then Valid_Selection (Prior, Items, Through)
          and then Live (Prior, Items (Items'First + Through))
          and then not Selected (Items, Through, Items (Items'First + Through).Slot),
        Post => Valid_Selection (Prior, Items, Through + 1);
      procedure Selection_Future
        (Prior : State; Items : Handle_Array; Through : Natural) with Ghost,
        Pre => Through < Items'Length and then Valid_Selection (Prior, Items, Items'Length),
        Post => not Selected (Items, Through, Items (Items'First + Through).Slot);
      function Dropped_Model (Store, Prior : State; Items : Handle_Array)
        return Boolean with Ghost;
      function Check_Dropped (Store, Prior : State; Items : Handle_Array)
        return Boolean with Post => Check_Dropped'Result = Dropped_Model (Store, Prior, Items);
   end Validation;
end AML_Scalar_Ledger;
