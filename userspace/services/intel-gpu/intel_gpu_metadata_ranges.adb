package body Intel_GPU_Metadata_Ranges is
   function Count (Object : Tree) return Natural is (Object.Size);
   function Level (Item : Node_Access) return Natural is
     (if Item = null then 0 else Item.Level);
   function Height (Object : Tree) return Natural is (Level (Object.Root));
   function Valid (Base, Bytes : Unsigned_64) return Boolean is
     (Base /= 0 and then Bytes /= 0 and then Bytes <= Unsigned_64'Last - Base);
   function Before (Base, Bytes : Unsigned_64; Item : Node_Access) return Boolean is
     (Base < Item.Base and then Bytes <= Item.Base - Base);
   function After (Base : Unsigned_64; Item : Node_Access) return Boolean is
     (Base > Item.Base and then Item.Bytes <= Base - Item.Base);
   procedure Conflict
     (Object : Tree; Base, Bytes : Unsigned_64;
      Overlaps : out Boolean; Visits : out Natural) is
      Current : Node_Access := Object.Root;
   begin
      Overlaps := True; Visits := 0;
      if not Valid (Base, Bytes) then return; end if;
      for Step in 1 .. 64 loop
         if Current = null then Overlaps := False; return; end if;
         Visits := Visits + 1;
         if Before (Base, Bytes, Current) then Current := Current.Left;
         elsif After (Base, Current) then Current := Current.Right;
         else return; end if;
      end loop;
      Overlaps := Current /= null;
   end Conflict;
   procedure Refresh (Item : Node_Access) is
   begin
      Item.Level := 1 + Natural'Max (Level (Item.Left), Level (Item.Right));
   end Refresh;
   function Balance (Item : Node_Access) return Integer is
     (Integer (Level (Item.Left)) - Integer (Level (Item.Right)));
   procedure Replace_Parent (Object : in out Tree; Old, New_Node : Node_Access) is
   begin
      New_Node.Parent := Old.Parent;
      if Old.Parent = null then Object.Root := New_Node;
      elsif Old.Parent.Left = Old then Old.Parent.Left := New_Node;
      else Old.Parent.Right := New_Node; end if;
   end Replace_Parent;
   function Rotate_Left (Object : in out Tree; Item : Node_Access) return Node_Access is
      Top : constant Node_Access := Item.Right;
   begin
      Replace_Parent (Object, Item, Top);
      Item.Right := Top.Left;
      if Item.Right /= null then Item.Right.Parent := Item; end if;
      Top.Left := Item; Item.Parent := Top;
      Refresh (Item); Refresh (Top); return Top;
   end Rotate_Left;
   function Rotate_Right (Object : in out Tree; Item : Node_Access) return Node_Access is
      Top : constant Node_Access := Item.Left;
   begin
      Replace_Parent (Object, Item, Top);
      Item.Left := Top.Right;
      if Item.Left /= null then Item.Left.Parent := Item; end if;
      Top.Right := Item; Item.Parent := Top;
      Refresh (Item); Refresh (Top); return Top;
   end Rotate_Right;
   procedure Insert
     (Object : in out Tree; Item : Node_Access; Base, Bytes : Unsigned_64;
      Accepted : out Boolean; Visits : out Natural) is
      Current : Node_Access := Object.Root;
      Parent, Top, Ignored : Node_Access := null;
      Is_Left : Boolean := False;
   begin
      Accepted := False; Visits := 0;
      -- With at most2**31-1 nodes, an AVL tree's height is below64. Retain
      -- this explicit bound even on targets with a wider Natural type.
      if Item = null or else Item.Linked or else not Valid (Base, Bytes) or else
        Unsigned_64 (Object.Size) >= 2 ** 31 - 1 then return; end if;
      for Step in 1 .. 64 loop
         exit when Current = null;
         Visits := Visits + 1; Parent := Current;
         if Before (Base, Bytes, Current) then
            Is_Left := True; Current := Current.Left;
         elsif After (Base, Current) then
            Is_Left := False; Current := Current.Right;
         else return; end if;
      end loop;
      if Current /= null then return; end if;
      Item.Base := Base; Item.Bytes := Bytes;
      Item.Left := null; Item.Right := null; Item.Parent := Parent;
      Item.Level := 1; Item.Linked := True;
      if Parent = null then Object.Root := Item;
      elsif Is_Left then Parent.Left := Item;
      else Parent.Right := Item; end if;
      Object.Size := Object.Size + 1;
      Current := Parent;
      for Step in 1 .. 64 loop
         exit when Current = null;
         Visits := Visits + 1; Refresh (Current); Top := Current;
         if Balance (Current) > 1 then
            if Balance (Current.Left) < 0 then
               Ignored := Rotate_Left (Object, Current.Left);
            end if;
            Top := Rotate_Right (Object, Current);
         elsif Balance (Current) < -1 then
            if Balance (Current.Right) > 0 then
               Ignored := Rotate_Right (Object, Current.Right);
            end if;
            Top := Rotate_Left (Object, Current);
         end if;
         Current := Top.Parent;
      end loop;
      Accepted := True;
   end Insert;
end Intel_GPU_Metadata_Ranges;
