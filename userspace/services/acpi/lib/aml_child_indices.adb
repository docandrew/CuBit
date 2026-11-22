pragma Ada_2022;
with Interfaces;
package body AML_Child_Indices with SPARK_Mode is
   function Bucket (Scope : Node_ID; Part : AML_Names.Segment) return Bucket_Index is
      use type Interfaces.Unsigned_64;
      Offset_Basis : constant Interfaces.Unsigned_64 := 14_695_981_039_346_656_037;
      Prime : constant Interfaces.Unsigned_64 := 1_099_511_628_211;
      Value : Interfaces.Unsigned_64 := Offset_Basis xor Interfaces.Unsigned_64 (Scope);
   begin
      for C of Part loop
         Value := (Value xor Interfaces.Unsigned_64 (Character'Pos (C))) * Prime;
      end loop;
      return Bucket_Index (Value mod Interfaces.Unsigned_64 (Capacity) + 1);
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
end AML_Child_Indices;
