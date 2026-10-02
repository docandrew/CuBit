with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.State;
with CuBit.UI.Widgets;
procedure Native_Tabs_Tests is
   package Controls renames CuBit.UI.Controls;
   package Widgets renames CuBit.UI.Widgets;
   Pixels : aliased array (0 .. 160 * 90 - 1) of Unsigned_32 := [others => 0];
   C : constant Canvas := (addr => Pixels'Address, width => 160, height => 90,
     pitch => 640, others => <>);
   Map : Controls.Control_Map;
   St : CuBit.UI.State.UI_State;
   Content : Canvas;
   Colors : Theme;
   Result : Widget_Result;
   Changed, Handled : Boolean;
   Parent : constant Rect := (10, 10, 100, 30);
   Child : constant Rect := (90, 12, 18, 26);
   procedure Draw (Orientation : Tab_Orientation) is
   begin
      Controls.Clear (Map);
      Widgets.Tab (C, St, Map, 1, Parent, (0, 0, 160, 90),
        Current_Theme, True, Content, Colors, Result, Orientation);
      Widgets.Label (Content, (15, 12, 70, 26), Colors, "caption");
      -- Arbitrary artwork and a native close button live inside the same tab.
      Fill_Rect (Content, (12, 12, 2, 2), Colors.accent);
      Widgets.Button (Content, St, Map, 2, Child, Content.clip,
        Colors, "x", Result, retainedInput => True);
   end Draw;
   procedure Dispatch (ID : Natural; Action : Controls.Pointer_Action;
                       X, Y : Natural) is
   begin
      Controls.Dispatch_Pointer (Map, ID, Action, X, Y, Changed, Handled);
      pragma Assert (Handled);
   end Dispatch;
begin
   for Orientation in Tab_Orientation loop
      for Cycle in 1 .. 100 loop
         Draw (Orientation);
         pragma Assert (Controls.Hit (Map, 95, 20) = 2);
         pragma Assert (Controls.Hit (Map, 40, 20) = 1);
         pragma Assert (Content.clip = Rect'(12, 12, 96, 26));
         Dispatch (2, Controls.Pointer_Press, 95, 20);
         Draw (Orientation); -- Repainting must retain capture state.
         pragma Assert (Controls.Is_Active (Map, 2));
         Dispatch (2, Controls.Pointer_Release, 95, 20);
         pragma Assert (Controls.Take_Activated (Map, 2));
         pragma Assert (not Controls.Take_Activated (Map, 1));
         pragma Assert (not Controls.Take_Activated (Map, 2));
         Dispatch (1, Controls.Pointer_Press, 40, 20);
         Dispatch (1, Controls.Pointer_Release, 40, 20);
         pragma Assert (Controls.Take_Activated (Map, 1));
         pragma Assert (not Controls.Take_Activated (Map, 2));
         Dispatch (2, Controls.Pointer_Press, 95, 20);
         Dispatch (2, Controls.Pointer_Release, 140, 60);
         pragma Assert (not Controls.Take_Activated (Map, 2));
         Dispatch (2, Controls.Pointer_Press, 95, 20);
         Dispatch (2, Controls.Pointer_Cancel, 95, 20);
         Dispatch (2, Controls.Pointer_Release, 95, 20);
         pragma Assert (not Controls.Take_Activated (Map, 2));
      end loop;
   end loop;
   -- Parent clipping applies to child hit bounds and child pixels alike.
   Controls.Clear (Map);
   Widgets.Tab (With_Clip (C, (10, 10, 88, 30)), St, Map, 1,
     Parent, (0, 0, 160, 90), Current_Theme, False,
     Content, Colors, Result, Vertical);
   pragma Assert (Content.clip = Rect'(12, 12, 86, 26));
   Widgets.Button (Content, St, Map, 2, Child, Content.clip,
     Colors, "x", Result, retainedInput => True);
   pragma Assert (Controls.Hit (Map, 97, 20) = 2);
   pragma Assert (Controls.Hit (Map, 98, 20) = 0);
   Controls.Clear (Map);
   Widgets.Tab (C, St, Map, 1, (159, 89, 1, 1), (0, 0, 160, 90),
     Current_Theme, False, Content, Colors, Result);
   pragma Assert (Is_Empty (Content.clip));
   Widgets.Button (Content, St, Map, 2, Child, Content.clip,
     Colors, "x", Result, retainedInput => True);
   pragma Assert (Controls.Hit (Map, 95, 20) = 0);
   -- Disabled navigation must not register a target. Enabled icon/caption
   -- controls retain native press/release semantics and respect pixel clipping.
   for Icon in Widgets.Navigation_Icon loop
      Controls.Clear (Map);
      Pixels := [others => 16#123456#];
      Widgets.Navigation_Button (With_Clip (C, (10, 10, 45, 30)), St, Map, 7,
        (10, 10, 72, 26), (0, 0, 160, 90), Current_Theme, Icon,
        "Forward", False, Result);
      pragma Assert (Controls.Hit (Map, 30, 20) = 0);
      for Y in 0 .. 89 loop
         for X in 0 .. 159 loop
            if X < 10 or X >= 55 or Y < 10 or Y >= 40 then
               pragma Assert (Pixels (Y * 160 + X) = 16#123456#);
            end if;
         end loop;
      end loop;
      Widgets.Navigation_Button (C, St, Map, 7, (10, 10, 72, 26),
        (0, 0, 160, 90), Current_Theme, Icon, "Back", True, Result);
      pragma Assert (Controls.Hit (Map, 30, 20) = 7);
      Dispatch (7, Controls.Pointer_Press, 30, 20);
      Controls.Clear (Map);
      Widgets.Navigation_Button (C, St, Map, 7, (10, 10, 72, 26),
        (0, 0, 160, 90), Current_Theme, Icon, "Back", True, Result);
      Dispatch (7, Controls.Pointer_Release, 30, 20);
      pragma Assert (Controls.Take_Activated (Map, 7));
   end loop;
   Ada.Text_IO.Put_Line
     ("NATIVE-TAB-CONTAINER: PASS 200 cycles, child isolation, clipping, cancel");
end Native_Tabs_Tests;
