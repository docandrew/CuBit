pragma Ada_2022;
with Ada.Text_IO;
with AML_Child_Indices;
procedure Membership_Tests is
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   generic Capacity : Positive;
   procedure Exercise;
   procedure Exercise is
      package Index is new AML_Child_Indices (Capacity);
      package M renames Index.Memberships;
      use type M.State;
      Item : M.State := M.Empty;
      Before : constant M.State := Item;
   begin
      for Repeat in 1 .. 32 loop
         Check (M.Valid (Item));
         M.Attach (Item, 2, 1); M.Attach (Item, 4, 1);
         M.Attach (Item, 3, 2); M.Attach (Item, 6, 1); M.Attach (Item, 5, 2);
         Check (M.First (Item, 1) = 6 and M.Next (Item, 6) = 4 and M.Next (Item, 4) = 2);
         Check (M.First (Item, 2) = 5 and M.Next (Item, 5) = 3);
         M.Detach (Item, 4);
         Check (not M.Contains (Item, 4) and M.Next (Item, 6) = 2);
         Check (M.First (Item, 2) = 5 and M.Key_Of (Item, 3) = 2);
         M.Detach (Item, 2);
         Check (M.Next (Item, 6) = 0 and not M.Contains (Item, 2));
         M.Detach (Item, 6);
         Check (M.First (Item, 1) = 0);
         M.Attach (Item, 2, 1);
         Check (M.First (Item, 1) = 2 and M.Next (Item, 2) = 0);
         M.Detach (Item, 5);
         Check (M.First (Item, 2) = 3);
         M.Detach (Item, 3); M.Detach (Item, 2);
         Check (M.Valid (Item) and Item = Before);
         M.Attach (Item, 1, 8);
         Check (M.First (Item, 8) = 1 and M.Key_Of (Item, 1) = 8);
         M.Detach (Item, 1);
         Check (Item = Before);
      end loop;
   end Exercise;
   procedure Small is new Exercise (512);
   procedure Large is new Exercise (16_384);
begin
   Small; Large;
   Ada.Text_IO.Put_Line ("Membership checks" & Checks'Image);
end Membership_Tests;
