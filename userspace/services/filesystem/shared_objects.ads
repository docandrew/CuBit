with Interfaces; use Interfaces;

--  Bounded, single-dispatcher object metadata. Links are internal service
--  indices, never client handles or authority. No heap.
--
--  Every operation is O(1) or bounded by a constant independent of
--  Capacity: objects live in an open-addressing table (a key's object is
--  within Probe_Window slots of its home, and every slot between is in
--  use or was), and each object lists its owners (at most Max_Holders),
--  each owner knowing its place in that list. The quantified contracts are
--  proved at level 1 and are not evaluated at run time.
generic
   Capacity : Positive;
   type Object_Key is private;
   Empty_Key : Object_Key;
   type Object_Value is private;
   Empty_Value : Object_Value;
   with function Hash (Key : Object_Key) return Unsigned_32;
   --  Owners one object may have at once (a further Attach is Full).
   Max_Holders : Positive := 32;
package Shared_Objects with SPARK_Mode is
   pragma Assertion_Policy (Pre => Ignore, Post => Ignore, Ghost => Ignore,
                            Loop_Invariant => Ignore, Assert => Ignore,
                            Default_Initial_Condition => Ignore);

   subtype Owner_Index is Natural range 0 .. Capacity - 1;
   --  Twice as many home slots as owners keeps probe runs short.
   Home_Slots : constant Positive := 2 * Positive'Min (Capacity, 2 ** 24);
   Probe_Window : constant := 64;
   Object_Slots : constant Positive := Home_Slots + Probe_Window;
   subtype Object_Index is Positive range 1 .. Object_Slots;
   subtype Link is Natural range 0 .. Object_Slots;
   subtype Holder_Count is Natural range 0 .. Max_Holders;
   subtype Holder_Index is Natural range 0 .. Max_Holders - 1;

   type State is private
     with Default_Initial_Condition => Valid (State);
   type Sharing_Mode is (Allow_Sharing, Deny_Sharing);
   type Attach_Result is (Created, Shared, Owner_Busy, Sharing_Conflict, Full);

   function Home (Key : Object_Key) return Object_Index is
     (Object_Index (Hash (Key) mod Unsigned_32 (Home_Slots) + 1));

   function Attached (S : State; Owner : Owner_Index) return Boolean;
   function Value (S : State; Owner : Owner_Index) return Object_Value
     with Pre => Attached (S, Owner);
   function Key (S : State; Owner : Owner_Index) return Object_Key
     with Pre => Attached (S, Owner);
   function Same_Object (S : State; Left, Right : Owner_Index) return Boolean;
   function Exclusive (S : State; Owner : Owner_Index) return Boolean;
   --  Quantified over all owners: for contracts and tests, not service paths.
   function Can_Attach (S : State; Identity : Object_Key; Mode : Sharing_Mode)
      return Boolean;
   function Isolated (S : State; Owner : Owner_Index) return Boolean;
   function Exclusive_Owners_Isolated (S : State) return Boolean;
   function Valid (S : State) return Boolean with Ghost;

   --  The object an owner is attached to (0: none).
   function Object_Of_Owner (S : State; Owner : Owner_Index) return Link;
   function Key_Of_Object (S : State; Object : Object_Index) return Object_Key;

   --  The object holding Identity, or 0: exactly when no owner has it.
   function Object_Of (S : State; Identity : Object_Key) return Link
     with Pre => Valid (S),
          Post =>
            (if Object_Of'Result /= 0 then
               Holding (S, Object_Of'Result) > 0 and then
               Key_Of_Object (S, Object_Of'Result) = Identity) and then
            (if Object_Of'Result = 0 then
               (for all Object in Object_Index =>
                  (if Holding (S, Object) > 0 then
                     Key_Of_Object (S, Object) /= Identity)) and then
               (for all Other in Owner_Index =>
                  (if Attached (S, Other) then Key (S, Other) /= Identity))
             else
               (for all Other in Owner_Index =>
                  (if Attached (S, Other) and then Key (S, Other) = Identity
                   then Object_Of_Owner (S, Other) = Object_Of'Result)));

   --  The owners of an object: Holder (S, Object, 0 .. Holding - 1), each
   --  attached to it; every owner attached to it is among them (the
   --  holder-list invariant in Valid).
   function Holding (S : State; Object : Object_Index) return Holder_Count;
   function Holder (S : State; Object : Object_Index; Index : Holder_Index)
      return Owner_Index
     with Pre => Valid (S) and then Index < Holding (S, Object),
          Post => Object_Of_Owner (S, Holder'Result) = Object;
   function Exclusively_Held (S : State; Identity : Object_Key) return Boolean
     with Pre => Valid (S),
          Post => Exclusively_Held'Result =
            (for some Other in Owner_Index =>
               Exclusive (S, Other) and then Key (S, Other) = Identity);

   --  Existing objects retain their current value, not the supplied snapshot.
   procedure Attach
     (S : in out State; Owner : Owner_Index; Identity : Object_Key;
      Initial : Object_Value; Result : out Attach_Result;
      Mode : Sharing_Mode := Allow_Sharing)
     with Pre => Valid (S),
          Post =>
       Valid (S) and then
       (if Result in Created | Shared then
          Attached (S, Owner) and then Key (S, Owner) = Identity and then
          Exclusive (S, Owner) = (Mode = Deny_Sharing) and then
          (if Mode = Deny_Sharing then Isolated (S, Owner))
        else
          (for all Other in Owner_Index =>
             Attached (S, Other) = Attached (S'Old, Other) and then
             (if Attached (S'Old, Other) then
                Key (S, Other) = Key (S'Old, Other) and then
                Value (S, Other) = Value (S'Old, Other)))) and then
       (if not Attached (S'Old, Owner) and then not Can_Attach (S'Old, Identity, Mode)
        then Result = Sharing_Conflict) and then
       Exclusive_Owners_Isolated (S) and then
       (for all Other in Owner_Index =>
          (if Other /= Owner then
             Attached (S, Other) = Attached (S'Old, Other) and then
             Exclusive (S, Other) = Exclusive (S'Old, Other) and then
             (if Attached (S'Old, Other) then
                Key (S, Other) = Key (S'Old, Other) and then
                Value (S, Other) = Value (S'Old, Other))));
   procedure Replace
     (S : in out State; Owner : Owner_Index; Item : Object_Value)
     with Pre => Valid (S) and then Attached (S, Owner),
          Post =>
            Valid (S) and then
            (for all Other in Owner_Index =>
               Attached (S, Other) = Attached (S'Old, Other) and then
               (if Attached (S'Old, Other) then
                  Key (S, Other) = Key (S'Old, Other) and then
                  Value (S, Other) =
                    (if Same_Object (S'Old, Owner, Other) then Item
                     else Value (S'Old, Other))));
   --  Closing one owner cannot retire metadata still referenced by another.
   procedure Detach (S : in out State; Owner : Owner_Index)
     with Pre => Valid (S),
          Post => Valid (S) and then not Attached (S, Owner) and then
       Exclusive_Owners_Isolated (S) and then
       (for all Other in Owner_Index =>
          (if Other /= Owner then
             Attached (S, Other) = Attached (S'Old, Other) and then
             Exclusive (S, Other) = Exclusive (S'Old, Other) and then
             (if Attached (S'Old, Other) then
                Key (S, Other) = Key (S'Old, Other) and then
                Value (S, Other) = Value (S'Old, Other))));
private
   type Links is array (Owner_Index) of Link;
   type Places is array (Owner_Index) of Holder_Index;
   type Modes is array (Owner_Index) of Sharing_Mode;
   type Keys is array (Object_Index) of Object_Key;
   type Values is array (Object_Index) of Object_Value;
   type Count_Array is array (Object_Index) of Holder_Count;
   type Holder_Row is array (Holder_Index) of Owner_Index;
   type Holder_Rows is array (Object_Index) of Holder_Row;
   type Flags is array (Object_Index) of Boolean;
   type State is record
      Owners : Links := [others => 0];
      Places_Of : Places := [others => 0];     --  an owner's holder index
      Sharing : Modes := [others => Allow_Sharing];
      Identities : Keys := [others => Empty_Key];
      Metadata : Values := [others => Empty_Value];
      Counts : Count_Array := [others => 0];
      Holders : Holder_Rows := [others => [others => 0]];
      Probed : Flags := [others => False];     --  in use now or before
   end record;

   function Attached (S : State; Owner : Owner_Index) return Boolean is
     (S.Owners (Owner) /= 0);
   function Value (S : State; Owner : Owner_Index) return Object_Value is
     (S.Metadata (S.Owners (Owner)));
   function Key (S : State; Owner : Owner_Index) return Object_Key is
     (S.Identities (S.Owners (Owner)));
   function Same_Object (S : State; Left, Right : Owner_Index) return Boolean is
     (Attached (S, Left) and then S.Owners (Left) = S.Owners (Right));
   function Exclusive (S : State; Owner : Owner_Index) return Boolean is
     (Attached (S, Owner) and then S.Sharing (Owner) = Deny_Sharing);
   function Can_Attach (S : State; Identity : Object_Key; Mode : Sharing_Mode)
      return Boolean is
     (for all Other in Owner_Index =>
        (if Attached (S, Other) and then Key (S, Other) = Identity then
           Mode = Allow_Sharing and then not Exclusive (S, Other)));
   function Isolated (S : State; Owner : Owner_Index) return Boolean is
     (for all Other in Owner_Index =>
        (if Other /= Owner and then Attached (S, Other) and then Attached (S, Owner)
         then Key (S, Other) /= Key (S, Owner)));
   function Exclusive_Owners_Isolated (S : State) return Boolean is
     (for all Owner in Owner_Index =>
        (if Exclusive (S, Owner) then Isolated (S, Owner)));
   function Object_Of_Owner (S : State; Owner : Owner_Index) return Link is
     (S.Owners (Owner));
   function Key_Of_Object (S : State; Object : Object_Index) return Object_Key is
     (S.Identities (Object));
   function Holding (S : State; Object : Object_Index) return Holder_Count is
     (S.Counts (Object));

   --  Owner links and holder lists agree.
   function Linked (S : State) return Boolean is
     ((for all Owner in Owner_Index =>
         (if S.Owners (Owner) /= 0 then
            S.Places_Of (Owner) < S.Counts (S.Owners (Owner)) and then
            S.Holders (S.Owners (Owner)) (S.Places_Of (Owner)) = Owner)) and then
      (for all Object in Object_Index =>
         (for all Index in Holder_Index =>
            (if Index < S.Counts (Object) then
               S.Owners (S.Holders (Object) (Index)) = Object and then
               S.Places_Of (S.Holders (Object) (Index)) = Index))))
     with Ghost;

   --  Live objects: probed, in their key's window, every slot before them
   --  in it probed, and keys unique.
   function Placed (S : State) return Boolean is
     ((for all Object in Object_Index =>
         (if S.Counts (Object) > 0 then
            S.Probed (Object) and then
            Object >= Home (S.Identities (Object)) and then
            Object - Home (S.Identities (Object)) < Probe_Window and then
            (for all Before in Home (S.Identities (Object)) .. Object - 1 =>
               S.Probed (Before)))) and then
      (for all A in Object_Index =>
         (for all B in Object_Index =>
            (if A /= B and then S.Counts (A) > 0 and then S.Counts (B) > 0
             then S.Identities (A) /= S.Identities (B)))))
     with Ghost;

   --  A denying owner is its object's only one.
   function Denials_Alone (S : State) return Boolean is
     (for all Owner in Owner_Index =>
        (if S.Owners (Owner) /= 0 and then S.Sharing (Owner) = Deny_Sharing
         then S.Counts (S.Owners (Owner)) = 1))
     with Ghost;

   function Valid (S : State) return Boolean is
     (Linked (S) and then Placed (S) and then Denials_Alone (S));
end Shared_Objects;
