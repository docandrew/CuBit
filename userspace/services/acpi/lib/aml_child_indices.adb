pragma Ada_2022;
with AML_Child_Hash;
package body AML_Child_Indices with SPARK_Mode is
   function Bucket (Scope : Node_ID; Part : AML_Names.Segment) return Bucket_Index is
   begin
      return Bucket_Index (AML_Child_Hash.Bucket (Scope, Part, Capacity));
   end Bucket;
   procedure Prepend (Index : in out State; Node : Position; Slot : Bucket_Index) is
      Head : constant Node_ID := Index.Heads (Slot);
   begin
      Index.Following (Node) := Head;
      Index.Previous (Node) := Root;
      if Head /= Root then Index.Previous (Head) := Node; end if;
      Index.Heads (Slot) := Node;
   end Prepend;
   procedure Unlink (Index : in out State; Node : Position; Slot : Bucket_Index) is
      Before : constant Node_ID := Index.Previous (Node);
      After : constant Node_ID := Index.Following (Node);
   begin
      if Before = Root then Index.Heads (Slot) := After;
      else Index.Following (Before) := After; end if;
      if After /= Root then Index.Previous (After) := Before; end if;
      Index.Following (Node) := Root;
      Index.Previous (Node) := Root;
   end Unlink;

   package body Name_Reservations with SPARK_Mode is
      procedure Reserve
        (Item : out Ticket; Count, Owner : Node_ID;
         Last_Stamp, Owner_Stamp : Incarnation; Definition : Definition_Identity) is
      begin
         Item := (Reserved => True, Next_Node => Count + 1,
           Lifetime_Owner => Owner, Before_Count => Count,
           Node_Stamp => Last_Stamp + 1, Lifetime_Stamp => Owner_Stamp,
           Before_Stamp => Last_Stamp, Definition => Definition);
      end Reserve;
      procedure Release
        (Item : in out Ticket; Current_Node, Owner : Node_ID;
         Current_Stamp, Owner_Stamp : Incarnation; Definition : Definition_Identity) is
         pragma Unreferenced (Current_Node, Owner, Current_Stamp, Owner_Stamp, Definition);
      begin
         Item := (others => <>);
      end Release;
   end Name_Reservations;
   package body Memberships with SPARK_Mode is
   function Empty return State is (others => <>);
   procedure Attach (Item : in out State; Node, Key : Position) is
   begin
      AML_Child_Indices.Prepend (Item.Index, Node, AML_Child_Indices.Bucket_Index (Key));
      Item.Key (Node) := Key;
   end Attach;
   procedure Detach (Item : in out State; Node : Position) is
      Key : constant Position := Position (Item.Key (Node));
   begin
      AML_Child_Indices.Unlink (Item.Index, Node, AML_Child_Indices.Bucket_Index (Key));
      Item.Key (Node) := Root;
   end Detach;
   end Memberships;
   package body Code_Bounds with SPARK_Mode is
      function Empty return State is (others => <>);
      procedure Append (Item : in out State; Code_End : Usage) is
      begin
         Item.Prefix (Item.Used + 1) := Usage'Max (Item.Prefix (Item.Used), Code_End);
         Item.Used := Item.Used + 1;
      end Append;
      procedure Update_Last (Item : in out State; Code_End : Usage) is
      begin
         Item.Prefix (Item.Used) := Usage'Max (Item.Prefix (Item.Used - 1), Code_End);
      end Update_Last;
      procedure Pop (Item : in out State) is
      begin
         Item.Prefix (Item.Used) := 0;
         Item.Used := Item.Used - 1;
      end Pop;
   end Code_Bounds;
end AML_Child_Indices;
