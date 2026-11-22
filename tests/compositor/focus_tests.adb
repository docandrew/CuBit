with Ada.Text_IO;
with Compositor_Focus;
procedure Focus_Tests is
   package F renames Compositor_Focus;
   Items : F.Candidates (0 .. 7);
   Choice : F.Selection;
   Expected : Integer;
   Grown : F.Candidates (0 .. 15) := [others => False];
   Empty : constant F.Candidates (0 .. -1) := [others => False];
begin
   for Mask in 0 .. 255 loop
      Expected := -1;
      for I in Items'Range loop
         Items (I) := (Mask / 2 ** I) mod 2 = 1;
         if Items (I) then Expected := I; end if;
      end loop;
      Choice := F.Topmost (Items);
      pragma Assert (Choice.Found = (Expected /= -1));
      if Choice.Found then pragma Assert (Choice.Slot = Expected); end if;
   end loop;
   -- A grown table: the topmost eligible slot may be past the first eight.
   Grown (3) := True;
   Grown (12) := True;
   Choice := F.Topmost (Grown);
   pragma Assert (Choice.Found and Choice.Slot = 12);
   pragma Assert (not F.Topmost (Empty).Found);
   Ada.Text_IO.Put_Line ("COMPOSITOR FOCUS: PASS all 256 eligibility masks and a grown table");
end Focus_Tests;
