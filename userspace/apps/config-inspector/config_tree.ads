--  Owned namespace tree, independent of IPC, rendering and application authority.
package Config_Tree is
   Maximum_Nodes : constant := 256;
   subtype Node_ID is Natural range 0 .. Maximum_Nodes;
   subtype Name_Length is Natural range 0 .. 128;
   type Node is record
      Path : String (1 .. 128) := [others => ' '];
      Length : Name_Length := 0;
      Label_First : Positive := 1;
      Parent, Child, Next : Node_ID := 0;
      Depth : Natural range 0 .. 128 := 0;
      Has_Value : Boolean := False;
      Expanded : Boolean := True;
   end record;
   type Node_Array is array (Positive range 1 .. Maximum_Nodes) of Node;
   type Model is record
      Nodes : Node_Array;
      Count : Node_ID := 1; -- Node 1 is the machine context, never a key.
   end record;
   type Row_Array is array (Positive range 1 .. Maximum_Nodes) of Node_ID;
   type Rows is record
      IDs : Row_Array := [others => 0];
      Count : Node_ID := 0;
   end record;
   --  Failed refresh leaves the previous complete snapshot unchanged.
   procedure Load (Item : in out Model; Names : String; Success : out Boolean);
   function Visible (Item : Model) return Rows;
   function Find (Item : Model; Path : String) return Node_ID;
   function Key (Item : Model; ID : Node_ID) return String;
   function Label (Item : Model; ID : Node_ID) return String;
end Config_Tree;
