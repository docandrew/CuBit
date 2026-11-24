pragma Ada_2022;
with AML_Names;
generic
   Capacity : Positive;
package AML_Child_Indices with SPARK_Mode, Pure is
   subtype Node_ID is Natural range 0 .. Capacity;
   subtype Position is Positive range 1 .. Capacity;
   type Bucket_Index is new Positive range 1 .. Capacity;
   Root : constant Node_ID := 0;
   type Links is array (Position) of Node_ID;
   type Bucket_Heads is array (Bucket_Index) of Node_ID;
   type State is record
      Heads : Bucket_Heads := [others => Root];
      Following, Previous : Links := [others => Root];
   end record;
   function Bucket (Scope : Node_ID; Part : AML_Names.Segment) return Bucket_Index;

   --  Local link algebra only. The owning namespace establishes membership,
   --  bucket/key coherence and the global canonical representation.
   procedure Prepend (Index : in out State; Node : Position; Slot : Bucket_Index)
     with Pre => Index.Heads (Slot) < Node
       and then Index.Following (Node) = Root
       and then Index.Previous (Node) = Root,
       Post => Index.Heads = (Index'Old.Heads with delta Slot => Node)
         and then Index.Following =
           (Index'Old.Following with delta Node => Index'Old.Heads (Slot))
         and then Index.Previous =
           (if Index'Old.Heads (Slot) = Root then Index'Old.Previous
            else (Index'Old.Previous with delta Index'Old.Heads (Slot) => Node))
         and then Index.Following (Node) < Node;

   procedure Unlink (Index : in out State; Node : Position; Slot : Bucket_Index)
     with Pre => Index.Following (Node) < Node
       and then (Index.Previous (Node) = Root or else Index.Previous (Node) > Node)
       and then (if Index.Previous (Node) = Root then Index.Heads (Slot) = Node
         else Index.Following (Index.Previous (Node)) = Node)
       and then (if Index.Following (Node) /= Root then
         Index.Previous (Index.Following (Node)) = Node),
       Post => Index.Heads =
           (if Index'Old.Previous (Node) = Root then
              (Index'Old.Heads with delta Slot => Index'Old.Following (Node))
            else Index'Old.Heads)
         and then Index.Following =
           (if Index'Old.Previous (Node) = Root then
              (Index'Old.Following with delta Node => Root)
            else (Index'Old.Following with delta
              Index'Old.Previous (Node) => Index'Old.Following (Node), Node => Root))
         and then Index.Previous =
           (if Index'Old.Following (Node) = Root then
              (Index'Old.Previous with delta Node => Root)
            else (Index'Old.Previous with delta
              Index'Old.Following (Node) => Index'Old.Previous (Node), Node => Root));

   -- A bounded owner receipt for one unpublished append. No index or
   -- namespace snapshot is retained by the ticket.
   generic
      type Incarnation is range <>;
      type Definition_Identity is private;
      No_Definition : Definition_Identity;
   package Name_Reservations with SPARK_Mode is
      subtype Node_ID is Natural range 0 .. Capacity;
      type Ticket is private;
      function Active (Item : Ticket) return Boolean;
      function Node (Item : Ticket) return Node_ID;
      function Stamp (Item : Ticket) return Incarnation;
      function Prior_Count (Item : Ticket) return Node_ID;
      function Prior_Stamp (Item : Ticket) return Incarnation;
      function Matches
        (Item : Ticket; Current_Node, Owner : Node_ID;
         Current_Stamp, Owner_Stamp : Incarnation;
         Definition : Definition_Identity) return Boolean;
      procedure Reserve
        (Item : out Ticket; Count, Owner : Node_ID;
         Last_Stamp, Owner_Stamp : Incarnation; Definition : Definition_Identity)
        with Pre => Count < Capacity and then Owner <= Count
          and then Last_Stamp >= 0 and then Last_Stamp < Incarnation'Last
          and then (if Owner = 0 then Owner_Stamp = 0
            else Owner_Stamp > 0 and then Owner_Stamp <= Last_Stamp),
          Post => Active (Item)
            and then Node (Item) = Count + 1
            and then Stamp (Item) = Last_Stamp + 1
            and then Owner < Node (Item)
            and then Prior_Count (Item) = Count
            and then Prior_Stamp (Item) = Last_Stamp
            and then Matches (Item, Node (Item), Owner, Stamp (Item), Owner_Stamp, Definition);
      -- Callers authenticate against the live entry before either committing or
      -- unlinking it. This receipt is not authority to alter another owner.
      procedure Release
        (Item : in out Ticket; Current_Node, Owner : Node_ID;
         Current_Stamp, Owner_Stamp : Incarnation; Definition : Definition_Identity)
        with Pre => Matches (Item, Current_Node, Owner, Current_Stamp, Owner_Stamp, Definition),
          Post => not Active (Item)
            and then not Matches (Item, Current_Node, Owner, Current_Stamp, Owner_Stamp, Definition);
   private
      type Ticket is record
         Reserved : Boolean := False;
         Next_Node, Lifetime_Owner, Before_Count : Node_ID := 0;
         Node_Stamp, Lifetime_Stamp, Before_Stamp : Incarnation := 0;
         Definition : Definition_Identity := No_Definition;
      end record;
      function Active (Item : Ticket) return Boolean is (Item.Reserved);
      function Node (Item : Ticket) return Node_ID is (Item.Next_Node);
      function Stamp (Item : Ticket) return Incarnation is (Item.Node_Stamp);
      function Prior_Count (Item : Ticket) return Node_ID is (Item.Before_Count);
      function Prior_Stamp (Item : Ticket) return Incarnation is (Item.Before_Stamp);
      function Matches
        (Item : Ticket; Current_Node, Owner : Node_ID;
         Current_Stamp, Owner_Stamp : Incarnation;
         Definition : Definition_Identity) return Boolean is
        (Item.Reserved and then Item.Next_Node = Current_Node
         and then Item.Lifetime_Owner = Owner and then Item.Node_Stamp = Current_Stamp
         and then Item.Lifetime_Stamp = Owner_Stamp and then Item.Definition = Definition);
   end Name_Reservations;
   -- Keyed membership coherence; callers authenticate lifetime identities.
   package Memberships with SPARK_Mode is
   subtype Node_ID is AML_Child_Indices.Node_ID;
   subtype Position is AML_Child_Indices.Position;
   Root : constant Node_ID := 0;
   type State is private;
   function Valid (Item : State) return Boolean;
   function Contains (Item : State; Node : Node_ID) return Boolean;
   function Key_Of (Item : State; Node : Node_ID) return Node_ID;
   function First (Item : State; Key : Position) return Node_ID;
   function Next (Item : State; Node : Position) return Node_ID;
   function Empty return State with Post => Valid (Empty'Result)
     and then (for all N in Position => not Contains (Empty'Result, N))
     and then (for all K in Position => First (Empty'Result, K) = Root);
   procedure Attach (Item : in out State; Node, Key : Position)
     with Pre => Valid (Item) and then not Contains (Item, Node)
       and then First (Item, Key) < Node,
       Post => Valid (Item) and then Contains (Item, Node)
         and then Key_Of (Item, Node) = Key and then First (Item, Key) = Node
         and then (for all I in Position =>
           (if I /= Node then Key_Of (Item, I) = Key_Of (Item'Old, I)
             and then Contains (Item, I) = Contains (Item'Old, I)))
         and then (for all K in Position =>
           (if K /= Key then First (Item, K) = First (Item'Old, K)));
   procedure Detach (Item : in out State; Node : Position)
     with Pre => Valid (Item) and then Contains (Item, Node),
       Post => Valid (Item) and then not Contains (Item, Node)
         and then (for all I in Position =>
           (if I /= Node then Key_Of (Item, I) = Key_Of (Item'Old, I)
             and then Contains (Item, I) = Contains (Item'Old, I)));
private
   type Keys is array (Position) of Node_ID;
   type State is record
      Index : AML_Child_Indices.State;
      Key : Keys := [others => Root];
   end record;
   function Contains (Item : State; Node : Node_ID) return Boolean is
     (Node /= Root and then Item.Key (Node) /= Root);
   function Key_Of (Item : State; Node : Node_ID) return Node_ID is
     (if Node = Root then Root else Item.Key (Node));
   function First (Item : State; Key : Position) return Node_ID is
     (Item.Index.Heads (AML_Child_Indices.Bucket_Index (Key)));
   function Next (Item : State; Node : Position) return Node_ID is
     (Item.Index.Following (Node));
   function Valid (Item : State) return Boolean is
     ((for all K in Position =>
       (if First (Item, K) /= Root then
          Key_Of (Item, First (Item, K)) = K
          and then Item.Index.Previous (First (Item, K)) = Root))
      and then (for all N in Position =>
        (if Item.Key (N) = Root then
           Item.Index.Previous (N) = Root and then Item.Index.Following (N) = Root
         else
           Item.Index.Following (N) < N
           and then (if Item.Index.Previous (N) = Root then
             First (Item, Position (Item.Key (N))) = N
           else Item.Index.Previous (N) > N
             and then Item.Key (Item.Index.Previous (N)) = Item.Key (N)
             and then Item.Index.Following (Item.Index.Previous (N)) = N)
           and then (if Item.Index.Following (N) /= Root then
             Item.Key (Item.Index.Following (N)) = Item.Key (N)
             and then Item.Index.Previous (Item.Index.Following (N)) = N))));
   end Memberships;
   generic
      Max_Code_Bytes : Positive;
   package Code_Bounds with SPARK_Mode is
      subtype Usage is Natural range 0 .. Max_Code_Bytes;
      type State is private;
      function Valid (Item : State) return Boolean;
      function Count (Item : State) return Node_ID;
      function Bound_At (Item : State; Node : Node_ID) return Usage;
      function Empty return State with Post => Valid (Empty'Result)
        and then Count (Empty'Result) = 0
        and then (for all I in Node_ID => Bound_At (Empty'Result, I) = 0);
      procedure Append (Item : in out State; Code_End : Usage)
        with Pre => Valid (Item) and then Count (Item) < Capacity,
          Post => Valid (Item) and then Count (Item) = Count (Item'Old) + 1
            and then Bound_At (Item, Count (Item)) =
              Usage'Max (Bound_At (Item'Old, Count (Item'Old)), Code_End)
            and then (for all I in Node_ID =>
              (if I /= Count (Item) then Bound_At (Item, I) = Bound_At (Item'Old, I)));
      procedure Update_Last (Item : in out State; Code_End : Usage)
        with Pre => Valid (Item) and then Count (Item) > 0,
          Post => Valid (Item) and then Count (Item) = Count (Item'Old)
            and then Bound_At (Item, Count (Item)) =
              Usage'Max (Bound_At (Item'Old, Count (Item'Old) - 1), Code_End)
            and then (for all I in Node_ID =>
              (if I /= Count (Item) then Bound_At (Item, I) = Bound_At (Item'Old, I)));
      procedure Pop (Item : in out State)
        with Pre => Valid (Item) and then Count (Item) > 0,
          Post => Valid (Item) and then Count (Item) = Count (Item'Old) - 1
            and then Bound_At (Item, Count (Item'Old)) = 0
            and then (for all I in Node_ID =>
              (if I /= Count (Item'Old) then Bound_At (Item, I) = Bound_At (Item'Old, I)));
   private
      type Bounds is array (Node_ID) of Usage;
      type State is record
         Used : Node_ID := 0;
         Prefix : Bounds := [others => 0];
      end record;
      function Count (Item : State) return Node_ID is (Item.Used);
      function Bound_At (Item : State; Node : Node_ID) return Usage is (Item.Prefix (Node));
      function Valid (Item : State) return Boolean is
        (Item.Prefix (0) = 0
         and then (for all I in Position =>
           (if I <= Item.Used then Item.Prefix (I) >= Item.Prefix (I - 1)
            else Item.Prefix (I) = 0)));
   end Code_Bounds;
end AML_Child_Indices;
