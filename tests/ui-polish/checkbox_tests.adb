--  CuBit.UI.Widgets.Checkbox enabled and disabled: an enabled box is a
--  control a press and release toggle; a disabled one is drawn greyed with
--  its state kept (face fill, muted mark), registers no control and does not
--  toggle.
with Ada.Text_IO; use Ada.Text_IO;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.State;
with CuBit.UI.Widgets;
procedure Checkbox_Tests is
   package Controls renames CuBit.UI.Controls;
   use type Color;
   use type Controls.Control_ID;
   type Surface is array (0 .. 63, 0 .. 63) of Color;
   Pixels : aliased Surface := [others => [others => 0]];
   C : constant Canvas := (addr => Pixels'Address, width => 64, height => 64, pitch => 256, others => <>);
   Box : constant Rect := (10, 10, 16, 16);
   Full : constant Rect := (0, 0, 64, 64);
   T : constant Theme := CuBit_Alloy;
   UI : CuBit.UI.State.UI_State;
   Map : Controls.Control_Map;
   Result : Widget_Result;
   ID : constant Controls.Control_ID := 7;

   --  One frame with the pointer at the box's centre, down or up.
   procedure Frame (Checked : in out Boolean; Enabled : Boolean; Down : Boolean;
                    Pressed, Released : Boolean := False) is
   begin
      CuBit.UI.State.Set_Pointer (UI, Box.x + Box.w / 2, Box.y + Box.h / 2, down => Down,
                                  pressed => Pressed, released => Released);
      CuBit.UI.State.Begin_Frame (UI);
      Controls.Clear (Map);
      CuBit.UI.Widgets.Checkbox (C, UI, Map, ID, Box, Full, T, Checked, Result, enabled => Enabled);
      CuBit.UI.State.Finish_Frame (UI);
   end Frame;

   procedure Click (Checked : in out Boolean; Enabled : Boolean) is
   begin
      Frame (Checked, Enabled, Down => False);
      Frame (Checked, Enabled, Down => True, Pressed => True);
      Frame (Checked, Enabled, Down => False, Released => True);
      Frame (Checked, Enabled, Down => False);
   end Click;

   Checked : Boolean := False;
   Corner : Color;
   Mark : Color;
begin
   --  Enabled: a control, and a click toggles it.
   Click (Checked, Enabled => True);
   pragma Assert (Checked, "an enabled checkbox toggles on a click");
   pragma Assert (Controls.Hit (Map, Box.x + 4, Box.y + 4) = ID, "an enabled checkbox is a control");
   Corner := Pixels (Box.y + 3, Box.x + 3);
   pragma Assert (Corner = T.selection, "an enabled checked box is filled with the selection colour");

   --  Disabled: shown checked, greyed; no control; a click changes nothing.
   Click (Checked, Enabled => False);
   pragma Assert (Checked, "a disabled checkbox keeps its state on a click");
   pragma Assert (Controls.Hit (Map, Box.x + 4, Box.y + 4) /= ID, "a disabled checkbox is not a control");
   pragma Assert (not Result.hot and then not Result.active and then not Result.activated,
                  "a disabled checkbox reports nothing");
   Corner := Pixels (Box.y + 3, Box.x + 3);
   pragma Assert (Corner = T.face, "a disabled box is filled with the face colour");
   --  The check mark's first stroke, muted.
   Mark := Pixels (Box.y + Box.h / 2, Box.x + Box.w / 2 - 4);
   pragma Assert (Mark = T.muted, "a disabled box's mark is muted");

   --  Disabled and unchecked stays unchecked.
   Checked := False;
   Click (Checked, Enabled => False);
   pragma Assert (not Checked, "a disabled unchecked checkbox stays unchecked");

   --  Enabled again, it toggles again.
   Click (Checked, Enabled => True);
   pragma Assert (Checked, "enabled again, it toggles");
   Put_Line ("PASS: checkbox enabled and disabled");
end Checkbox_Tests;
