------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The integrated Deskbar's look (docs/development-backlog.md UI-027): the
--  taskbar's geometry and one palette per colour scheme.
--
--  @description
--  Shared with the window chrome (CuBit.UI.Window_Chrome): square, flat,
--  bordered; each task button carries its app's colour as a Stripe_Height
--  stripe on top, the same typed App_Color the window's title stripe uses.
--  The focused window's button is white and semibold; a group whose windows
--  are all minimized is dimmed, its stripe the window's precomputed muted
--  stripe. All values are solid fills: nothing here is a per-pixel blend.
--
--  Layout, left to right along the bottom bar: Apps (the CuBit logo), a
--  separator, the task buttons, then the status button (notification
--  count, status icons, clock) that opens the Deskbar panel above it.
------------------------------------------------------------------------------
with CuBit.Appearance;
with CuBit.Theme;
with CuBit.UI_Metrics;

package Desktop_Deskbar_Style with SPARK_Mode is
   use type CuBit.Appearance.Color_Scheme;
   subtype Color is CuBit.Theme.Color;
   package M renames CuBit.UI_Metrics;

   --  Geometry, in logical pixels, derived from the UI em (CuBit.UI_Metrics):
   --  a slim bar whose buttons hold the large atlas icons (24 pixels at the
   --  16-pixel em) at 1:1, so they stay crisp at every density, with
   --  Space_S above and below them; widths are in ems so labels at the base
   --  text size fit. Rules and stripes are hairlines.
   Button_Height     : constant := M.Large_Icon_Edge + 2 * M.Space_S;
   Bar_Height        : constant := Button_Height + 2 * M.Space_S;
   Bar_Edge          : constant := 1;    --  the bar's top rule
   Bar_Inset         : constant := M.Space_M;    --  left and right of the bar's content
   Button_Top        : constant := (Bar_Height - Button_Height) / 2;
   Button_Gap        : constant := M.Space_S;
   Stripe_Height     : constant := 3;
   Icon_Left         : constant := M.Space_S + M.Space_XS;
   Icon_Label_Gap    : constant := M.Space_M;
   Label_Right       : constant := M.Space_M + M.Space_XS;
   Apps_Width        : constant := 6 * M.Base_Em;
   Separator_Height  : constant := M.Large_Icon_Edge;
   Separator_Margin  : constant := M.Space_S;
   --  A task button is as wide as its content (Desktop_Deskbar_Layout),
   --  up to Task_Maximum, and shrinks down to Task_Minimum when crowded.
   Task_Maximum      : constant := 17 * M.Base_Em;
   Task_Minimum      : constant := 4 * M.Base_Em;
   --  A group's window count, at the button's right.
   Count_Width       : constant := M.Line_Height;
   Count_Height      : constant := M.Line_Height;
   Count_Gap         : constant := M.Space_S + M.Space_XS;
   Focus_Inset       : constant := 3;    --  keyboard focus ring inside a button

   Status_Height     : constant := Button_Height;
   Status_Top        : constant := (Bar_Height - Status_Height) / 2;
   Status_Padding    : constant := M.Space_L;
   Status_Item_Gap   : constant := M.Space_M;
   Badge_Size        : constant := M.Line_Height;

   --  The Deskbar panel and the toast, right-aligned above the status button.
   Panel_Width       : constant := 25 * M.Base_Em;
   Panel_Stripe      : constant := 4;
   Panel_Margin      : constant := M.Space_M;
   Panel_Padding     : constant := M.Space_L;
   Panel_Row         : constant := M.Line_Height + 2 * M.Space_L;
   Panel_Header      : constant := Panel_Row;
   Panel_Section     : constant := M.Section_Height;
   --  A notification: its title and detail lines, Space_XS apart.
   Panel_Note_Row    : constant := 2 * M.Line_Height + M.Space_XS + M.Space_L;
   Panel_Footer      : constant := M.Section_Height;
   Panel_Visible_Notes : constant := 5;
   Slider_Width      : constant := 12 * M.Base_Em;
   Slider_Height     : constant := M.Space_S;
   Thumb_Width       : constant := M.Space_M + M.Space_XS;
   Thumb_Height      : constant := M.Line_Height;
   Close_Box         : constant := M.Line_Height - M.Space_XS;
   Toggle_Height     : constant := M.Line_Height + M.Space_XS;
   Toggle_Width      : constant := 2 * Toggle_Height;
   Toast_Height      : constant := 2 * M.Line_Height + 2 * M.Space_L;

   --  A group's window list, above its button.
   List_Width        : constant := 22 * M.Base_Em;
   List_Row          : constant := Button_Height;
   List_Padding      : constant := M.Space_S;

   type Palette is record
      Bar, Bar_Edge, Separator           : Color;
      Button, Button_Edge, Button_Text   : Color;
      Active, Active_Edge, Active_Text   : Color;
      Dimmed_Text                        : Color;
      Status, Status_Edge, Status_Open   : Color;
      Badge, Badge_Text                  : Color;
      Panel, Panel_Edge, Panel_Stripe    : Color;
      Section, Section_Text              : Color;
      Row, Row_Rule, Muted               : Color;
      Track, Thumb, Thumb_Edge           : Color;
      Focus                              : Color;
      --  Text on a Focus fill (a switched-on toggle).
      Focus_Text                         : Color;
   end record;

   --  Every text colour here meets 4.5:1 against the fills it is drawn on
   --  (Dimmed_Text on Button, Muted on Row and Panel, Focus_Text on Focus).

   --  Light: the mockup's neutrals (TaskHaiku board 6).
   Deskbar_Purple : constant Color := 16#5B3FC4#;

   Alloy_Light : constant Palette :=
     (Bar          => 16#F4F6F8#,
      Bar_Edge     => CuBit.Theme.Edge,
      Separator    => CuBit.Theme.Edge,
      Button       => 16#EEF1F4#,
      Button_Edge  => 16#D3D8DE#,
      Button_Text  => 16#3B444D#,
      Active       => CuBit.Theme.White,
      Active_Edge  => 16#9AA3AD#,
      Active_Text  => CuBit.Theme.Text,
      Dimmed_Text  => 16#636D78#,
      Status       => CuBit.Theme.White,
      Status_Edge  => CuBit.Theme.Edge,
      Status_Open  => 16#4A535C#,
      Badge        => CuBit.Theme.Danger,
      Badge_Text   => CuBit.Theme.White,
      Panel        => 16#F4F6F8#,
      Panel_Edge   => 16#4A535C#,
      Panel_Stripe => Deskbar_Purple,
      Section      => 16#E9ECEF#,
      Section_Text => 16#3B444D#,
      Row          => CuBit.Theme.White,
      Row_Rule     => 16#E1E5E9#,
      Muted        => CuBit.Theme.Muted,
      Track        => CuBit.Theme.Edge,
      Thumb        => CuBit.Theme.White,
      Thumb_Edge   => 16#4A535C#,
      Focus        => 16#2F4F9E#,
      Focus_Text   => CuBit.Theme.White);

   --  Dark: the same structure on the window chrome's dark slate.
   Alloy_Dark : constant Palette :=
     (Bar          => 16#1C2328#,
      Bar_Edge     => 16#0F1417#,
      Separator    => 16#3A454E#,
      Button       => 16#2E373E#,
      Button_Edge  => 16#3A454E#,
      Button_Text  => 16#D5DCE1#,
      Active       => 16#46525B#,
      Active_Edge  => 16#6E7B86#,
      Active_Text  => 16#F4F6F8#,
      Dimmed_Text  => 16#96A2AB#,
      Status       => 16#2E373E#,
      Status_Edge  => 16#3A454E#,
      Status_Open  => 16#8E9BA4#,
      Badge        => CuBit.Theme.Danger,
      Badge_Text   => CuBit.Theme.White,
      Panel        => 16#232A30#,
      Panel_Edge   => 16#0F1417#,
      Panel_Stripe => Deskbar_Purple,
      Section      => 16#1C2328#,
      Section_Text => 16#D5DCE1#,
      Row          => 16#2E373E#,
      Row_Rule     => 16#3A454E#,
      Muted        => 16#9AA6AF#,
      Track        => 16#4C5862#,
      Thumb        => 16#D5DCE1#,
      Thumb_Edge   => 16#0F1417#,
      Focus        => 16#7FA6FF#,
      Focus_Text   => 16#0F1417#);

   function Colors (Scheme : CuBit.Appearance.Color_Scheme) return Palette is
     (if Scheme = CuBit.Appearance.Alloy_Light then Alloy_Light else Alloy_Dark);
end Desktop_Deskbar_Style;
