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
end AML_Child_Indices;
