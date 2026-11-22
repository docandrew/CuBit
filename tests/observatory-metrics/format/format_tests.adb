with Ada.Text_IO; use Ada.Text_IO;
with Observatory_Format_Budget;
procedure Format_Tests is
   package B renames Observatory_Format_Budget;
   Item : B.State;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
begin
   for Rows in B.Row_Count loop
      B.Start (Item, Rows);
      for I in 0 .. Rows - 1 loop
         Check (B.Active (Item) and B.Current (Item) = I);
         Check (B.Pending (Item) = Rows - I);
         B.Advance (Item);
      end loop;
      Check (not B.Active (Item));
   end loop;
   for Stop in B.Row_Count loop
      B.Start (Item, 16);
      for I in 1 .. Stop loop B.Advance (Item); end loop;
      B.Cancel (Item); Check (not B.Active (Item));
   end loop;
   Put_Line ("PASS format budget:" & Checks'Image & " checks");
end Format_Tests;
