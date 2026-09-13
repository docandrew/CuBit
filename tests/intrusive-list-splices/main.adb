with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Test_Splices;
with Buddy_Blocks; use Buddy_Blocks;
with Refinement_Checks;
procedure Main is
   subtype Slot is Natural range 0 .. 64;
   type Links is record
      Previous, Following : Natural := 0;
   end record;
   Nodes : array (Slot) of Links;
   Members : array (Slot) of Boolean := [others => False];
   Ledger : array (Slot) of Descriptor;
   Count : Natural := 0;
   package Random_Slots is new Ada.Numerics.Discrete_Random (Slot);
   Generator : Random_Slots.Generator;
   procedure Validate is
      Seen : array (Slot) of Boolean := [others => False];
      Current, Previous : Slot := 0;
      Total : Natural := 0;
   begin
      Current := Nodes (0).Following;
      while Current /= 0 loop
         pragma Assert (not Seen (Current));
         Seen (Current) := True;
         pragma Assert (Members (Current));
         pragma Assert (Nodes (Current).Previous = Previous);
         Previous := Current;
         Current := Nodes (Current).Following;
         Total := Total + 1;
      end loop;
      pragma Assert (Nodes (0).Previous = Previous and Total = Count);
      for I in Slot range 1 .. Slot'Last loop
         pragma Assert (Seen (I) = Members (I));
         pragma Assert (Seen (I) = Matches (Ledger (I), Listed, 0));
      end loop;
   end Validate;
   procedure Toggle (Item : Slot) is
      First, Previous, Following : Slot;
      Accepted : Boolean;
   begin
      if Item = 0 then
         return;
      end if;
      if Members (Item) then
         Move (Ledger (Item), 0, Remove, Accepted);
         pragma Assert (Accepted);
         Previous := Nodes (Item).Previous;
         Following := Nodes (Item).Following;
         Test_Splices.Remove
           (Nodes (Previous).Following, Nodes (Following).Previous,
            Previous, Following);
         Count := Test_Splices.Removed (Count);
         Move (Ledger (Item), 0, Commit, Accepted);
         pragma Assert (Accepted);
      else
         if Kind (Ledger (Item)) = Allocated then
            Move (Ledger (Item), 0, Release_Block, Accepted);
            pragma Assert (Accepted);
         end if;
         Move (Ledger (Item), 0, Publish, Accepted);
         pragma Assert (Accepted);
         First := Nodes (0).Following;
         Test_Splices.Insert_Front
           (Nodes (0).Following, Nodes (First).Previous,
            Nodes (Item).Previous, Nodes (Item).Following, 0, Item);
         Count := Test_Splices.Added (Count);
      end if;
      Members (Item) := not Members (Item);
      Validate;
   end Toggle;
begin
   Refinement_Checks.Run;
   Ada.Text_IO.Put_Line
     ("PASS Ghost refinement: all removal positions, all orders, corruption rejection");
   for I in Slot range 1 .. Slot'Last loop
      declare
         Accepted : Boolean;
      begin
         Admit (Ledger (I), 0, True, Accepted);
         pragma Assert (Accepted);
      end;
   end loop;
   -- Empty/singleton, full list, arbitrary interior removal and reinsertion.
   Toggle (1);
   Toggle (1);
   for I in Slot range 1 .. Slot'Last loop
      Toggle (I);
   end loop;
   for I in Slot range 1 .. Slot'Last loop
      Toggle (I);
   end loop;
   Random_Slots.Reset (Generator, 20260913);
   for Trial in 1 .. 200_000 loop
      Toggle (Random_Slots.Random (Generator));
   end loop;
   Ada.Text_IO.Put_Line
     ("PASS intrusive splices: 200000 operations with full list/ledger oracle");
end Main;
