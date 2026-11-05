with AML_Identity;
package AML_Definition_Loads with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   -- Declaration provenance is metadata, never an address or hardware authority.
   type Definition_Identity is new AML_Identity.Identity;
   No_Definition : constant Definition_Identity := Definition_Identity (AML_Identity.No_Identity);
   function Has_Definition (Item : Definition_Identity) return Boolean is (Item /= No_Definition);
   type Load_Phase is (Retained, Loading, Loaded, Failed);
   type Entry_Model is record
      Phase : Load_Phase := Retained;
      Definition : Definition_Identity := No_Definition;
   end record;
   type Model_Array is array (Positive range <>) of Entry_Model;
   type State (Capacity : Positive) is limited private
     with Default_Initial_Condition => Valid (State) and then Unique_Definitions (State)
       and then not Active (State)
       and then (for all I in 1 .. State.Capacity =>
         Phase (State, I) = Retained and then Definition (State, I) = No_Definition);
   function Valid (Loads : State) return Boolean;
   function Phase (Loads : State; Index : Positive) return Load_Phase
     with Pre => Index <= Loads.Capacity;
   function Definition (Loads : State; Index : Positive) return Definition_Identity
     with Pre => Index <= Loads.Capacity;
   function Active (Loads : State) return Boolean
     with Post => Active'Result = (for some I in 1 .. Loads.Capacity => Phase (Loads, I) = Loading);
   function Model (Loads : State) return Model_Array with Ghost;
   function Unique_Definitions (Loads : State) return Boolean is
     (for all I in 1 .. Loads.Capacity =>
       (for all J in 1 .. Loads.Capacity =>
         (if I /= J and then Has_Definition (Definition (Loads, I)) then
            Definition (Loads, I) /= Definition (Loads, J)))) with Ghost;
   procedure Reset (Loads : in out State)
     with Post => Valid (Loads) and then not Active (Loads) and then Unique_Definitions (Loads)
       and then (for all I in 1 .. Loads.Capacity =>
         Phase (Loads, I) = Retained and then Definition (Loads, I) = No_Definition);
   -- Reserve before effects. Distinct slots may be pending for nested AML loads.
   procedure Reserve (Loads : in out State; Index : Positive; Accepted : out Boolean)
     with Pre => Valid (Loads) and then Index <= Loads.Capacity,
       Post => Valid (Loads)
         and then (if Unique_Definitions (Loads)'Old then Unique_Definitions (Loads))
         and then Accepted = (Model (Loads)'Old (Index).Phase = Retained)
         and then (if Accepted then Phase (Loads, Index) = Loading
           and then Definition (Loads, Index) = No_Definition
          else Model (Loads) = Model (Loads)'Old)
         and then (for all I in 1 .. Loads.Capacity =>
           (if I /= Index then Model (Loads) (I) = Model (Loads)'Old (I)));
   -- A consumed identity must be nonempty and absent from every prior slot.
   procedure Consume (Loads : in out State; Index : Positive;
      ID : Definition_Identity; Accepted : out Boolean)
     with Pre => Valid (Loads) and then Index <= Loads.Capacity,
       Post => Valid (Loads)
         and then (if Unique_Definitions (Loads)'Old then Unique_Definitions (Loads))
         and then Accepted = (Model (Loads)'Old (Index).Phase = Loading
           and then not Has_Definition (Model (Loads)'Old (Index).Definition)
           and then Has_Definition (ID)
           and then (for all I in 1 .. Loads.Capacity => Model (Loads)'Old (I).Definition /= ID))
         and then (if Accepted then Phase (Loads, Index) = Loading
           and then Definition (Loads, Index) = ID
          else Model (Loads) = Model (Loads)'Old)
         and then (for all I in 1 .. Loads.Capacity =>
           (if I /= Index then Model (Loads) (I) = Model (Loads)'Old (I)));
   -- Unconsumed reservations return to Retained; consumed failure is terminal.
   procedure Finish (Loads : in out State; Index : Positive; Succeeded : Boolean;
      Accepted : out Boolean)
     with Pre => Valid (Loads) and then Index <= Loads.Capacity,
       Post => Valid (Loads)
         and then (if Unique_Definitions (Loads)'Old then Unique_Definitions (Loads))
         and then Accepted = (Model (Loads)'Old (Index).Phase = Loading)
         and then Definition (Loads, Index) = Model (Loads)'Old (Index).Definition
         and then (if Accepted then Phase (Loads, Index) =
           (if not Has_Definition (Model (Loads)'Old (Index).Definition) then Retained
            elsif Succeeded then Loaded else Failed)
          else Model (Loads) = Model (Loads)'Old)
         and then (for all I in 1 .. Loads.Capacity =>
           (if I /= Index then Model (Loads) (I) = Model (Loads)'Old (I)));
   procedure Require_Recovery (Loads : in out State; Index : Positive; Accepted : out Boolean)
     with Pre => Valid (Loads) and then Index <= Loads.Capacity,
       Post => Valid (Loads)
         and then (if Unique_Definitions (Loads)'Old then Unique_Definitions (Loads))
         and then Accepted = (Model (Loads)'Old (Index).Phase = Loaded)
         and then Definition (Loads, Index) = Model (Loads)'Old (Index).Definition
         and then (if Accepted then Phase (Loads, Index) = Failed
           else Model (Loads) = Model (Loads)'Old)
         and then (for all I in 1 .. Loads.Capacity =>
           (if I /= Index then Model (Loads) (I) = Model (Loads)'Old (I)));
   type Slot_Selection is new Natural;
   No_Slot : constant Slot_Selection := 0;
   -- Root session policy only, not permission to mutate a registry or namespace.
   -- A no-slot session requires an idle registry. An enclosing-slot session
   -- requires exactly one consumed Loading entry with the expected identity.
   function Session_Admitted (Loads : State; Enclosing : Slot_Selection;
      Expected_Definition : Definition_Identity) return Boolean
     with Global => null,
       Post => Session_Admitted'Result =
         (if Enclosing = No_Slot then not Active (Loads)
          else Enclosing <= Slot_Selection (Loads.Capacity)
            and then Has_Definition (Expected_Definition)
            and then Phase (Loads, Positive (Enclosing)) = Loading
            and then Definition (Loads, Positive (Enclosing)) = Expected_Definition
            and then (for all I in 1 .. Loads.Capacity =>
              (if I /= Positive (Enclosing) then Phase (Loads, I) /= Loading)));

private
   function Valid_Entries (Entries : Model_Array) return Boolean is
     (for all I in Entries'Range =>
       (if Entries (I).Phase = Retained then not Has_Definition (Entries (I).Definition)
        elsif Entries (I).Phase in Loaded | Failed then Has_Definition (Entries (I).Definition)));
   type State (Capacity : Positive) is limited record
      Entries : Model_Array (1 .. Capacity);
   end record with Type_Invariant => Valid_Entries (State.Entries);
   function Phase (Loads : State; Index : Positive) return Load_Phase is (Loads.Entries (Index).Phase);
   function Definition (Loads : State; Index : Positive) return Definition_Identity is (Loads.Entries (Index).Definition);
   function Model (Loads : State) return Model_Array is (Loads.Entries);
   function Valid (Loads : State) return Boolean is (Valid_Entries (Loads.Entries));
end AML_Definition_Loads;
