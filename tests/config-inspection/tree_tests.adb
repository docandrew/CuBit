with Ada.Text_IO;
with Config_Tree; use Config_Tree;
procedure Tree_Tests is
   Item, Before : Model;
   Good : Boolean;
   ID : Node_ID;
   View : Rows;
   Exhaust : String (1 .. 2304);
   Used : Natural := 0;
   procedure Rejected (Names : String) is
   begin
      Before := Item;
      Load (Item, Names, Good);
      pragma Assert (not Good and Item = Before);
   end Rejected;
begin
   Load (Item, "a.first" & ASCII.LF & "b.value" & ASCII.LF & "a.second" & ASCII.LF & "a" & ASCII.LF, Good);
   pragma Assert (Good and Item.Count = 6);
   ID := Find (Item, "a");
   pragma Assert (Item.Nodes (ID).Has_Value and Item.Nodes (ID).Child /= 0);
   View := Visible (Item);
   pragma Assert (View.Count = 6 and Key (Item, View.IDs (4)) = "a.second");
   Item.Nodes (ID).Expanded := False;
   View := Visible (Item);
   pragma Assert (View.Count = 4);
   Load (Item, "b.value" & ASCII.LF & "a.second" & ASCII.LF, Good);
   pragma Assert (Good and not Item.Nodes (Find (Item, "a")).Expanded);
   Rejected (".bad"); Rejected ("bad."); Rejected ("bad..name");
   Rejected (ASCII.LF & "bad"); Rejected ("bad" & ASCII.NUL);
   Rejected (String'(1 .. 129 => 'x'));
   for I in 0 .. 255 loop
      declare
         Name : constant String := "node." & Character'Val (97 + I / 26) &
           Character'Val (97 + I mod 26) & ASCII.LF;
      begin
         Exhaust (Used + 1 .. Used + Name'Length) := Name;
         Used := Used + Name'Length;
      end;
   end loop;
   Rejected (Exhaust (1 .. Used));
   Load (Item, "", Good);
   pragma Assert (Good and Item.Count = 1 and Visible (Item).Count = 1);
   Ada.Text_IO.Put_Line ("PASS Config tree grouping, mixed value/namespace, collapse, refresh and atomic rejection");
end Tree_Tests;
