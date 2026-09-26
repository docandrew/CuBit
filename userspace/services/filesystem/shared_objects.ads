--  Bounded, single-dispatcher object metadata. Links are internal service
--  indices, never client handles or authority. No heap or reference counters.
generic
   Capacity : Positive;
   type Object_Key is private;
   Empty_Key : Object_Key;
   type Object_Value is private;
   Empty_Value : Object_Value;
package Shared_Objects with SPARK_Mode is
   subtype Owner_Index is Natural range 0 .. Capacity - 1;
   type State is private
     with Default_Initial_Condition => Exclusive_Owners_Isolated (State);
   type Sharing_Mode is (Allow_Sharing, Deny_Sharing);
   type Attach_Result is (Created, Shared, Owner_Busy, Sharing_Conflict, Full);

   function Attached (S : State; Owner : Owner_Index) return Boolean;
   function Value (S : State; Owner : Owner_Index) return Object_Value
     with Pre => Attached (S, Owner);
   function Key (S : State; Owner : Owner_Index) return Object_Key
     with Pre => Attached (S, Owner);
   function Same_Object (S : State; Left, Right : Owner_Index) return Boolean;
   function Exclusive (S : State; Owner : Owner_Index) return Boolean;
   function Exclusively_Held (S : State; Identity : Object_Key) return Boolean;
   function Can_Attach (S : State; Identity : Object_Key; Mode : Sharing_Mode)
      return Boolean;
   function Isolated (S : State; Owner : Owner_Index) return Boolean with Ghost;
   function Exclusive_Owners_Isolated (S : State) return Boolean with Ghost;

   --  Existing objects retain their current value, not the supplied snapshot.
   procedure Attach
     (S : in out State; Owner : Owner_Index; Identity : Object_Key;
      Initial : Object_Value; Result : out Attach_Result;
      Mode : Sharing_Mode := Allow_Sharing)
     with Post =>
       (if Result in Created | Shared then
          Attached (S, Owner) and then Key (S, Owner) = Identity and then
          Exclusive (S, Owner) = (Mode = Deny_Sharing) and then
          (if Mode = Deny_Sharing then Isolated (S, Owner))
        else S = S'Old) and then
       (if not Attached (S'Old, Owner) and then not Can_Attach (S'Old, Identity, Mode)
        then Result = Sharing_Conflict) and then
       (if Exclusive_Owners_Isolated (S'Old) then Exclusive_Owners_Isolated (S)) and then
       (for all Other in Owner_Index =>
          (if Other /= Owner then
             Attached (S, Other) = Attached (S'Old, Other) and then
             Exclusive (S, Other) = Exclusive (S'Old, Other) and then
             (if Attached (S'Old, Other) then
                Key (S, Other) = Key (S'Old, Other) and then
                Value (S, Other) = Value (S'Old, Other))));
   procedure Replace
     (S : in out State; Owner : Owner_Index; Item : Object_Value)
     with Pre => Attached (S, Owner),
          Post =>
            Exclusive_Owners_Isolated (S) = Exclusive_Owners_Isolated (S'Old) and then
            (for all Other in Owner_Index =>
               Attached (S, Other) = Attached (S'Old, Other) and then
               (if Attached (S'Old, Other) then
                  Key (S, Other) = Key (S'Old, Other) and then
                  Value (S, Other) =
                    (if Same_Object (S'Old, Owner, Other) then Item
                     else Value (S'Old, Other))));
   --  Closing one owner cannot retire metadata still referenced by another.
   procedure Detach (S : in out State; Owner : Owner_Index)
     with Post => not Attached (S, Owner) and then
       (if Exclusive_Owners_Isolated (S'Old) then Exclusive_Owners_Isolated (S)) and then
       (for all Other in Owner_Index =>
          (if Other /= Owner then
             Attached (S, Other) = Attached (S'Old, Other) and then
             Exclusive (S, Other) = Exclusive (S'Old, Other) and then
             (if Attached (S'Old, Other) then
                Key (S, Other) = Key (S'Old, Other) and then
                Value (S, Other) = Value (S'Old, Other))));
private
   subtype Object_Index is Positive range 1 .. Capacity;
   subtype Link is Natural range 0 .. Capacity;
   type Links is array (Owner_Index) of Link;
   type Keys is array (Object_Index) of Object_Key;
   type Values is array (Object_Index) of Object_Value;
   type Modes is array (Owner_Index) of Sharing_Mode;
   type State is record
      Owners : Links := [others => 0];
      Identities : Keys := [others => Empty_Key];
      Metadata : Values := [others => Empty_Value];
      Sharing : Modes := [others => Allow_Sharing];
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
   function Exclusively_Held (S : State; Identity : Object_Key) return Boolean is
     (for some Other in Owner_Index =>
        Exclusive (S, Other) and then Key (S, Other) = Identity);
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
end Shared_Objects;
