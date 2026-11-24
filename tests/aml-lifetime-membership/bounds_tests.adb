pragma Ada_2022;
with Ada.Text_IO;
with AML_Child_Indices;
procedure Bounds_Tests is
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   generic Capacity : Positive;
   procedure Exercise;
   procedure Exercise is
      package L is new AML_Child_Indices (Capacity);
      package B is new L.Code_Bounds (65_536);
      use type B.State;
      Item : B.State := B.Empty;
      Before : constant B.State := Item;
   begin
      for Repeat in 1 .. 32 loop
         Check (B.Valid (Item) and B.Count (Item) = 0 and B.Bound_At (Item, 0) = 0);
         B.Append (Item, 10);
         Check (B.Count (Item) = 1 and B.Bound_At (Item, 1) = 10);
         B.Append (Item, 0);
         Check (B.Count (Item) = 2 and B.Bound_At (Item, 2) = 10);
         B.Update_Last (Item, 40);
         Check (B.Bound_At (Item, 2) = 40);
         B.Update_Last (Item, 5);
         Check (B.Bound_At (Item, 2) = 10);
         B.Append (Item, 30);
         Check (B.Count (Item) = 3 and B.Bound_At (Item, 3) = 30);
         B.Pop (Item);
         Check (B.Count (Item) = 2 and B.Bound_At (Item, 2) = 10 and B.Bound_At (Item, 3) = 0);
         B.Update_Last (Item, 20);
         Check (B.Bound_At (Item, 1) = 10 and B.Bound_At (Item, 2) = 20);
         B.Pop (Item);
         Check (B.Count (Item) = 1 and B.Bound_At (Item, 1) = 10);
         B.Pop (Item);
         Check (Item = Before);
         B.Append (Item, 65_536); B.Append (Item, 0);
         Check (B.Bound_At (Item, 2) = 65_536);
         B.Pop (Item); B.Pop (Item);
         Check (Item = Before);
      end loop;
   end Exercise;
   procedure Small is new Exercise (512);
   procedure Large is new Exercise (16_384);
begin
   Small; Large;
   Ada.Text_IO.Put_Line ("Code bound checks" & Checks'Image);
end Bounds_Tests;
