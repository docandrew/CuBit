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
end AML_Child_Indices;
