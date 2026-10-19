------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Window chrome: the desktop's title bar and the toolkit's menu bar
------------------------------------------------------------------------------
--  The compositor draws the frame, the title bar and its buttons from one
--  palette per colour scheme. The two colours a client must match travel
--  in the theme instead (Theme.titleBar, Theme.menuRule): the client
--  toolkit's menu bar is filled with titleBar, the active title bar's
--  colour, so it continues the compositor's title bar below the rule. Each window
--  also carries its app's identity colour (the top stripe and the title
--  tile), declared with the app's launcher entry (CCL Launch_Entry `color`);
--  an undeclared colour is the theme accent.
--
--  Layout, top to bottom: Border, the app-colour Stripe, the title Bar, the
--  title/menu Rule, then the client (whose menu bar, if any, ends in
--  Theme.menuRule). All values are solid fills: nothing here is a per-pixel blend.
with CuBit.Appearance;
with CuBit.Theme;

package CuBit.UI.Window_Chrome with SPARK_Mode is
   --  Geometry, in logical pixels.
   Border_Width       : constant := 1;
   Stripe_Height      : constant := 4;
   Bar_Height         : constant := 28;
   Rule_Height        : constant := 1;
   Bar_Top            : constant := Border_Width + Stripe_Height;
   --  The client starts below the rule.
   Client_Top         : constant := Bar_Top + Bar_Height + Rule_Height;
   --  The title tile: the app's icon, drawn this size (Desktop_Icons).
   Tile_Size          : constant := 16;
   Tile_Left          : constant := 8;
   Tile_Title_Gap     : constant := 6;
   Title_Left         : constant := Tile_Left + Tile_Size + Tile_Title_Gap;
   Button_Width       : constant := 22;
   Button_Height      : constant := 20;
   Button_Gap         : constant := 4;
   Button_Right_Inset : constant := 6;
   Glyph_Size         : constant := 10;

   --  One window button's three colours.
   type Button_Colors is record
      Fill, Edge, Glyph : Theme_Color;
   end record;

   type Palette is record
      Active_Border, Inactive_Border : Theme_Color;
      --  The active bar is Theme.titleBar.
      Inactive_Bar                   : Theme_Color;
      Active_Title, Inactive_Title   : Theme_Color;
      --  Between the title bar and the client (its menu bar).
      Rule                           : Theme_Color;
      Button, Button_Hover           : Button_Colors;
      Inactive_Glyph                 : Theme_Color;
      Close_Hover                    : Button_Colors;
      --  A maximize the window does not allow: drawn in place, greyed.
      Unavailable                    : Button_Colors;
   end record;

   --  Light scheme: the "App-colour stripe" design (CuBit.Theme's neutrals).
   Light_Active_Border   : constant Theme_Color := 16#4A535C#;
   Light_Inactive_Border : constant Theme_Color := 16#6E7782#;
   Light_Inactive_Bar    : constant Theme_Color := 16#E9ECEF#;
   --  4.5:1 on the inactive bar.
   Light_Inactive_Title  : constant Theme_Color := 16#5F6973#;
   Light_Glyph           : constant Theme_Color := 16#3B444D#;
   Light_Hover_Fill      : constant Theme_Color := 16#E4E8EC#;
   Light_Hover_Edge      : constant Theme_Color := 16#AEB6BF#;
   Close_Hover_Edge      : constant Theme_Color := 16#A63A31#;
   Light_Unavailable_Fill  : constant Theme_Color := 16#F1F3F5#;
   Light_Unavailable_Edge  : constant Theme_Color := 16#DDE1E5#;
   Light_Unavailable_Glyph : constant Theme_Color := 16#BFC5CC#;

   Alloy_Light : constant Palette :=
     (Active_Border   => Light_Active_Border,
      Inactive_Border => Light_Inactive_Border,
      Inactive_Bar    => Light_Inactive_Bar,
      Active_Title    => CuBit.Theme.Text,
      Inactive_Title  => Light_Inactive_Title,
      Rule            => CuBit.Theme.Edge,
      Button          => (CuBit.Theme.White, CuBit.Theme.Edge, Light_Glyph),
      Button_Hover    => (Light_Hover_Fill, Light_Hover_Edge, CuBit.Theme.Text),
      Inactive_Glyph  => CuBit.Theme.Muted,
      Close_Hover     => (CuBit.Theme.Danger, Close_Hover_Edge, CuBit.Theme.White),
      Unavailable     => (Light_Unavailable_Fill, Light_Unavailable_Edge,
                          Light_Unavailable_Glyph));

   --  Dark scheme: the same structure on CuBit_Alloy_Dark's slate.
   Dark_Active_Border   : constant Theme_Color := 16#0F1417#;
   Dark_Inactive_Border : constant Theme_Color := 16#1C2328#;
   Dark_Inactive_Bar    : constant Theme_Color := 16#232A30#;
   Dark_Active_Title    : constant Theme_Color := 16#E7ECEF#;
   Dark_Inactive_Title  : constant Theme_Color := 16#8E9BA4#;
   Dark_Rule            : constant Theme_Color := 16#46525B#;
   Dark_Unavailable_Edge : constant Theme_Color := 16#3A454E#;
   Dark_Button_Fill     : constant Theme_Color := 16#343E46#;
   Dark_Button_Edge     : constant Theme_Color := 16#4C5862#;
   Dark_Glyph           : constant Theme_Color := 16#D5DCE1#;
   Dark_Hover_Fill      : constant Theme_Color := 16#3F4A53#;
   Dark_Hover_Edge      : constant Theme_Color := 16#5D6A75#;
   Dark_Unavailable_Fill  : constant Theme_Color := 16#2E373E#;
   Dark_Unavailable_Glyph : constant Theme_Color := 16#56626B#;

   Alloy_Dark : constant Palette :=
     (Active_Border   => Dark_Active_Border,
      Inactive_Border => Dark_Inactive_Border,
      Inactive_Bar    => Dark_Inactive_Bar,
      Active_Title    => Dark_Active_Title,
      Inactive_Title  => Dark_Inactive_Title,
      Rule            => Dark_Rule,
      Button          => (Dark_Button_Fill, Dark_Button_Edge, Dark_Glyph),
      Button_Hover    => (Dark_Hover_Fill, Dark_Hover_Edge, Dark_Active_Title),
      Inactive_Glyph  => Dark_Inactive_Title,
      Close_Hover     => (CuBit.Theme.Danger, Close_Hover_Edge, CuBit.Theme.White),
      Unavailable     => (Dark_Unavailable_Fill, Dark_Unavailable_Edge,
                          Dark_Unavailable_Glyph));

   function Colors (Scheme : CuBit.Appearance.Color_Scheme) return Palette is
     (if Scheme = CuBit.Appearance.Alloy_Light then Alloy_Light else Alloy_Dark);

   --  An inactive window's stripe and tile: the app colour Muted_Percent of
   --  the way toward the inactive title bar. Computed once per window when
   --  its colour or the scheme changes, never per pixel.
   Muted_Percent : constant := 45;
   function Muted (App, Bar : Theme_Color) return Theme_Color;

   --  The colours one window is drawn with, precomputed from its app colour
   --  (the launcher entry's, or the theme accent when it declares none).
   type Window_Colors is record
      Stripe, Muted_Stripe : Theme_Color := 0;
   end record;
   function For_App (App : Theme_Color; Chrome : Palette) return Window_Colors is
     ((Stripe => App, Muted_Stripe => Muted (App, Chrome.Inactive_Bar)));
end CuBit.UI.Window_Chrome;
