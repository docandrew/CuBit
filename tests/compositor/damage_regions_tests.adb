--  LAT-002 regression: a typing frame next to an animating window stays
--  small. A caret and an animating 3D canvas are separate damage boxes,
--  through capture, a retried or replaced frame (Restore) and the next
--  frame's damage; only past Compositor_Damage.Capacity, the named ceiling,
--  do boxes fall back to their envelope. Before the fix Restore re-added a
--  frame as its envelope: caret plus canvas became one box of ~half the
--  screen.
with Ada.Text_IO;
with Compositor_Damage;
procedure Damage_Regions_Tests is
   package D renames Compositor_Damage;
   use type D.Box;
   --  A 1920x1080 desktop: a Teapots canvas and a text caret far from it.
   Canvas : constant D.Box := (80, 120, 980, 820);
   Caret  : constant D.Box := (1400, 600, 1402, 620);
   function Area (B : D.Box) return Natural is ((B.Right - B.Left) * (B.Bottom - B.Top));
   function Total (S : D.State) return Natural is
      Sum : Natural := 0;
   begin
      for I in 1 .. D.Count (S) loop
         Sum := Sum + Area (D.Item (S, I));
      end loop;
      return Sum;
   end Total;
   Pending, Frame : D.State;
   Accepted : Boolean;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Ada.Text_IO.Put_Line ("DAMAGE-REGIONS: FAIL " & What);
         raise Program_Error;
      end if;
   end Check;
begin
   D.Add (Pending, Canvas);
   D.Add (Pending, Caret);
   Check (D.Count (Pending) = 2 and then Total (Pending) = Area (Canvas) + Area (Caret),
          "a caret and a canvas are two boxes");
   D.Capture (Pending, Frame, Accepted);
   Check (Accepted and then D.Count (Frame) = 2, "the frame holds both boxes");
   --  The frame is retried or replaced while the canvas animates again.
   D.Add (Pending, Canvas);
   D.Restore (Pending, Frame);
   Check (D.Count (Pending) = 2, "restoring keeps the caret apart from the canvas");
   Check (Total (Pending) = Area (Canvas) + Area (Caret),
          "restored damage is the canvas and the caret, not their envelope");
   Check (D.Covers (Pending, Caret) and then D.Covers (Pending, Canvas), "both stay covered");
   --  A typing frame alone: only the caret.
   D.Clear (Pending);
   D.Add (Pending, Caret);
   D.Capture (Pending, Frame, Accepted);
   D.Restore (Pending, Frame);
   Check (D.Count (Pending) = 1 and then Total (Pending) = Area (Caret), "a lone caret restores as itself");
   --  Past the ceiling the boxes fall back to one envelope (bounded work).
   D.Clear (Pending);
   for I in 0 .. D.Capacity loop
      D.Add (Pending, (I * 100, 0, I * 100 + 10, 10));
   end loop;
   Check (D.Count (Pending) = 1 and then D.Item (Pending, 1) = (0, 0, D.Capacity * 100 + 10, 10),
          "more boxes than the ceiling collapse to their envelope");
   Ada.Text_IO.Put_Line ("DAMAGE-REGIONS: PASS checks=" & Natural'Image (Checks));
end Damage_Regions_Tests;
