with Interfaces;
package Intel_GPU_Name_Index is
   subtype Name is Interfaces.Unsigned_32;
   type Node is record
      Key : Name := 0;
      Value, Left, Right : Natural := 0;
   end record;
   Empty : constant Node := (Key => 0, others => 0);
   type Index is limited private;
   -- Caller retains stable metadata slots. Reads/writes are trusted serialized
   -- operations; slots are not client-selected and storage never moves live
   -- registry records. Index nodes may move payloads, not the named records.
   generic
      with function Capacity return Natural;
      with function Read (Slot : Positive) return Node;
      with procedure Write (Slot : Positive; Item : Node);
   package Table is
      function Lookup (Object : Index; Key : Name) return Natural;
      -- Empty_Node must be an unlinked Empty slot owned by this index. Success
      -- consumes it; duplicate names leave the node and tree unchanged.
      procedure Insert (Object : in out Index; Key : Name; Value : Positive;
                        Empty_Node : Positive; Accepted : out Boolean);
      -- Unlinks a name, returns one cleared metadata slot. Does NOT release
      -- the named object/backing or authorize replacing its identity.
      procedure Remove (Object : in out Index; Key : Name;
                        Freed_Node : out Natural; Accepted : out Boolean);
      function Count (Object : Index) return Natural;
      function Quarantined (Object : Index) return Boolean;
   end Table;
private
   type Index is limited record
      Root, Used : Natural := 0;
      Failed : Boolean := False;
   end record;
end Intel_GPU_Name_Index;
