with Ada.Text_IO;
with Compositor_Focus;
procedure Focus_Tests is
   package F is new Compositor_Focus (8);
   Items : F.Candidates;
   Choice : F.Selection;
   Expected : Integer;
begin
   for Mask in 0 .. 255 loop
      Expected := -1;
      for I in F.Index loop
         Items (I) := (Mask / 2 ** I) mod 2 = 1;
         if Items (I) then Expected := I; end if;
      end loop;
      Choice := F.Topmost (Items);
      pragma Assert (Choice.Found = (Expected /= -1));
      if Choice.Found then pragma Assert (Choice.Slot = Expected); end if;
   end loop;
   Ada.Text_IO.Put_Line ("COMPOSITOR FOCUS: PASS all 256 eligibility masks");
end Focus_Tests;
