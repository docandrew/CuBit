with Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.Tooltips; use CuBit.UI.Tooltips;
with Client_Tooltip_Policy;

--  CuBit.UI.Tooltips: delay, sliding, dismissal, regions before controls,
--  damage covering the drawn box, placement on screen, and draw cost.
procedure Tooltips_Tests is
   package Controls renames CuBit.UI.Controls;
   DELAY_MS : constant := Client_Tooltip_Policy.SHOW_DELAY_MS;
   WIDTH : constant := 400;
   HEIGHT : constant := 240;
   BYTES_PER_PIXEL : constant := 4;
   Pixels : aliased array (0 .. WIDTH * HEIGHT - 1) of Unsigned_32 := [others => 0];
   C : constant Canvas := (addr => Pixels'Address, width => WIDTH, height => HEIGHT,
                           pitch => WIDTH * BYTES_PER_PIXEL, others => <>);
   Screen : constant Rect := (0, 0, WIDTH, HEIGHT);
   SAVE_ID : constant Controls.Control_ID := 10;
   OPEN_ID : constant Controls.Control_ID := 11;
   PLAIN_ID : constant Controls.Control_ID := 12;
   Map : Controls.Control_Map;
   Table : Tip_Table;
   Tip : Tooltip;
   Damage : Rect;
   Failures, Checks : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   function Inside (Inner, Outer : Rect) return Boolean is
     (Inner.x >= Outer.x and then Inner.y >= Outer.y and then Inner.x + Inner.w <= Outer.x + Outer.w
      and then Inner.y + Inner.h <= Outer.y + Outer.h);

   procedure Frame is
   begin
      Controls.Clear (Map);
      Controls.Add_Button (Map, SAVE_ID, (10, 10, 40, 24), (10, 10, 40, 24));
      Controls.Add_Button (Map, OPEN_ID, (50, 10, 40, 24), (50, 10, 40, 24));
      Controls.Add_Button (Map, PLAIN_ID, (90, 10, 40, 24), (90, 10, 40, 24));
      Clear (Table);
      Set (Table, SAVE_ID, "Save", "Ctrl+S");
      Set (Table, OPEN_ID, "Open");
      Set_Region (Table, (200, 100, 150, 20), "a-very-long-file-name-that-was-cut.txt");
      Draw (C, Tip, Table, Screen, Classic);
   end Frame;
begin
   Frame;
   Pointer_At (Tip, Table, Map, 20, 20, 1_000, Damage);
   Check (not Visible (Tip) and then Is_Empty (Damage), "nothing shows at once");
   Check (Next_Deadline (Tip) = 1_000 + DELAY_MS, "the delay is the deadline");
   Tick (Tip, Table, 1_000 + DELAY_MS - 1, Damage);
   Check (not Visible (Tip), "not before the delay");
   Pointer_At (Tip, Table, Map, 21, 20, 1_100, Damage);
   Check (Next_Deadline (Tip) = 1_100 + DELAY_MS, "a move restarts the delay");
   Tick (Tip, Table, 1_100 + DELAY_MS, Damage);
   Check (Visible (Tip) and then Text (Tip, Table) = "Save", "shows after resting");
   Check (not Is_Empty (Damage) and then Damage = Area_Of (Tip), "showing damages the laid-out box: "
          & Damage.x'Image & Damage.y'Image & Damage.w'Image & Damage.h'Image);
   Frame;
   Check (Area_Of (Tip) = Damage, "Draw puts it where the damage said");
   Check (Next_Deadline (Tip) = Client_Tooltip_Policy.NEVER, "no deadline while shown");
   declare
      Old_Box : constant Rect := Area_Of (Tip);
   begin
      Pointer_At (Tip, Table, Map, 60, 20, 1_700, Damage);
      Check (Visible (Tip) and then Text (Tip, Table) = "Open", "sliding to the next shows it at once");
      Check (Inside (Old_Box, Damage) and then Inside (Area_Of (Tip), Damage), "sliding damages both boxes");
   end;
   Frame;
   Pointer_At (Tip, Table, Map, 100, 20, 1_800, Damage);
   Check (not Visible (Tip), "a control without a tip hides it");
   Check (not Is_Empty (Damage), "hiding damages the old box");
   Pointer_At (Tip, Table, Map, 20, 20, 2_000, Damage);
   Tick (Tip, Table, 2_000 + DELAY_MS, Damage);
   Frame;
   Dismiss (Tip, Damage);
   Check (not Visible (Tip) and then not Is_Empty (Damage), "a click dismisses");
   Pointer_At (Tip, Table, Map, 22, 21, 3_000, Damage);
   Tick (Tip, Table, 4_000, Damage);
   Check (not Visible (Tip), "it stays dismissed on the same control");
   Pointer_At (Tip, Table, Map, 60, 20, 4_100, Damage);
   Tick (Tip, Table, 4_100 + DELAY_MS, Damage);
   Check (Visible (Tip) and then Text (Tip, Table) = "Open", "another control shows again");
   Pointer_At (Tip, Table, Map, 395, 105, 5_000, Damage);
   Check (not Visible (Tip), "leaving every target hides it");
   Pointer_At (Tip, Table, Map, 340, 110, 5_100, Damage);
   Tick (Tip, Table, 5_100 + DELAY_MS, Damage);
   Frame;
   Check (Text (Tip, Table) = "a-very-long-file-name-that-was-cut.txt", "a region (a cut cell) has its tip");
   Check (Inside (Area_Of (Tip), Screen), "placed on screen near the edge");

   --  Cost: Pointer_At over a full table, and drawing the tip.
   declare
      ROUNDS : constant := 100_000;
      Start : Time := Clock;
      Took : Duration;
   begin
      for K in 1 .. MAXIMUM_TIPS - 3 loop
         Set_Region (Table, (0, 200, 1, 1), "filler");
      end loop;
      for K in 1 .. ROUNDS loop
         Pointer_At (Tip, Table, Map, 20 + K mod 2, 20, 6_000 + Unsigned_64 (K), Damage);
      end loop;
      Took := Clock - Start;
      Ada.Text_IO.Put_Line ("bench: pointer_at (96 tips)" & Natural'Image (Natural (Float (Took) * 1.0E9 / Float (ROUNDS))) & " ns");
      Pointer_At (Tip, Table, Map, 340, 110, 10_000_000, Damage);
      Tick (Tip, Table, 10_000_000 + DELAY_MS, Damage);
      Start := Clock;
      for K in 1 .. ROUNDS / 10 loop
         Draw (C, Tip, Table, Screen, Classic);
      end loop;
      Took := Clock - Start;
      Ada.Text_IO.Put_Line ("bench: draw" & Natural'Image (Natural (Float (Took) * 1.0E9 / Float (ROUNDS / 10))) & " ns");
   end;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("PASS:" & Natural'Image (Checks) & " checks");
   else
      Ada.Text_IO.Put_Line ("FAIL:" & Natural'Image (Failures) & " of" & Natural'Image (Checks));
   end if;
end Tooltips_Tests;
