with Interfaces; use Interfaces;
package body Intel_GPU_Name_Index is
   function Goes_Right (Key : Name; Depth : Natural) return Boolean is
     ((Key and Shift_Left (Unsigned_32'(1), 31 - Depth)) /= 0);
   package body Table is
      function Count (Object : Index) return Natural is (Object.Used);
      function Quarantined (Object : Index) return Boolean is (Object.Failed);
      function Lookup (Object : Index; Key : Name) return Natural is
         Cursor : Natural := Object.Root;
         Item : Node;
      begin
         if Object.Failed or Key = 0 then return 0; end if;
         for Depth in 0 .. 32 loop
            if Cursor = 0 or else Cursor > Capacity then return 0; end if;
            Item := Read (Cursor);
            if Item.Key = 0 or Item.Value = 0 then return 0; end if;
            if Item.Key = Key then return Item.Value; end if;
            if Depth = 32 then return 0; end if;
            Cursor := (if Goes_Right (Key, Depth) then Item.Right else Item.Left);
         end loop;
         return 0;
      end Lookup;
      procedure Insert (Object : in out Index; Key : Name; Value : Positive;
                        Empty_Node : Positive; Accepted : out Boolean) is
         Cursor : Natural := Object.Root;
         Parent : Natural := 0;
         Right : Boolean := False;
         Item : Node;
      begin
         Accepted := False;
         if Object.Failed or Key = 0 or Empty_Node > Capacity or
           Object.Used = Natural'Last then return; end if;
         if Read (Empty_Node) /= Empty then return; end if;
         for Depth in 0 .. 32 loop
            if Cursor = 0 then
               Write (Empty_Node, (Key, Value, 0, 0));
               if Parent = 0 then Object.Root := Empty_Node;
               else
                  Item := Read (Parent);
                  if Right then Item.Right := Empty_Node; else Item.Left := Empty_Node; end if;
                  Write (Parent, Item);
               end if;
               Object.Used := Object.Used + 1;
               Accepted := True;
               return;
            end if;
            if Cursor > Capacity then Object.Failed := True; return; end if;
            Item := Read (Cursor);
            if Item.Key = 0 or Item.Value = 0 then Object.Failed := True; return; end if;
            if Item.Key = Key then return; end if;
            if Depth = 32 then Object.Failed := True; return; end if;
            Parent := Cursor; Right := Goes_Right (Key, Depth);
            Cursor := (if Right then Item.Right else Item.Left);
         end loop;
      end Insert;
      procedure Remove (Object : in out Index; Key : Name;
                        Freed_Node : out Natural; Accepted : out Boolean) is
         Cursor : Natural := Object.Root;
         Parent : Natural := 0;
         Target : Natural := 0;
         Item, Replacement : Node;
         Found : Boolean := False;
      begin
         Freed_Node := 0; Accepted := False;
         if Object.Failed or Key = 0 then return; end if;
         for Depth in 0 .. 32 loop
            if Cursor = 0 then return; end if;
            if Cursor > Capacity then Object.Failed := True; return; end if;
            Item := Read (Cursor);
            if Item.Key = 0 or Item.Value = 0 then Object.Failed := True; return; end if;
            if Item.Key = Key then Found := True; exit; end if;
            if Depth = 32 then Object.Failed := True; return; end if;
            Parent := Cursor;
            Cursor := (if Goes_Right (Key, Depth) then Item.Right else Item.Left);
         end loop;
         if not Found then return; end if;
         Target := Cursor;
         -- A descendant leaf shares the target's ancestor prefix. Move only
         -- its index payload into Target, preserving all named record slots.
         for Step in 0 .. 32 loop
            if Cursor = 0 or else Cursor > Capacity then Object.Failed := True; return; end if;
            Item := Read (Cursor);
            if Item.Key = 0 or Item.Value = 0 then Object.Failed := True; return; end if;
            if Item.Left = 0 and Item.Right = 0 then
               if Object.Used = 0 then Object.Failed := True; return; end if;
               if Parent = 0 then Object.Root := 0;
               else
                  Replacement := Read (Parent);
                  if Replacement.Left = Cursor then Replacement.Left := 0;
                  elsif Replacement.Right = Cursor then Replacement.Right := 0;
                  else Object.Failed := True; return;
                  end if;
                  Write (Parent, Replacement);
               end if;
               if Cursor /= Target then
                  Replacement := Read (Target);
                  Replacement.Key := Item.Key; Replacement.Value := Item.Value;
                  Write (Target, Replacement);
               end if;
               Write (Cursor, Empty);
               Object.Used := Object.Used - 1;
               Freed_Node := Cursor; Accepted := True;
               return;
            end if;
            if Step = 32 then Object.Failed := True; return; end if;
            Parent := Cursor;
            Cursor := (if Item.Left /= 0 then Item.Left else Item.Right);
         end loop;
      end Remove;
   end Table;
end Intel_GPU_Name_Index;
