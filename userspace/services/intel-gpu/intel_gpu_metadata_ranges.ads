with Interfaces; use Interfaces;
package Intel_GPU_Metadata_Ranges is
   -- Intrusive, insertion-only AVL index for trusted retained CPU metadata.
   -- Owner serializes access and retains the tree and every inserted node.
   -- Nodes are embedded in retained records: no heap, namespace-sized side
   -- table, removal, or implied GPU/CPU retirement authority.
   type Node is limited private;
   type Node_Access is access all Node;
   type Tree is limited private;
   function Count (Object : Tree) return Natural;
   function Height (Object : Tree) return Natural;
   procedure Conflict
     (Object : Tree; Base, Bytes : Unsigned_64;
      Overlaps : out Boolean; Visits : out Natural);
   -- Invalid/wrapping spans conservatively conflict. Half-open adjacency is
   -- allowed. At most64 node visits, independent of sparse allocation IDs.
   procedure Insert
     (Object : in out Tree; Item : Node_Access; Base, Bytes : Unsigned_64;
      Accepted : out Boolean; Visits : out Natural);
   -- Search then bounded ancestor rebalance (<=128 node visits total).
   -- Reject null/already-linked nodes and overlapping/invalid intervals before
   -- mutation. Nodes cannot be moved/reinitialized while retained in a tree.
private
   type Node is limited record
      Base, Bytes : Unsigned_64 := 0;
      Left, Right, Parent : Node_Access := null;
      Level : Natural range 0 .. 64 := 0;
      Linked : Boolean := False;
   end record;
   type Tree is limited record
      Root : Node_Access := null;
      Size : Natural := 0;
   end record;
end Intel_GPU_Metadata_Ranges;
