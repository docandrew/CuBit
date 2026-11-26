------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The Deskbar's hover card (UI-061): its parts and where they go. A task
--  button names an app; resting on it (or focusing it from the keyboard)
--  shows this card above it, with the app's name, where that name came
--  from, a live thumbnail of one of its windows and every window's title.
--
--  @description
--  Flat and square like the window chrome and the window list: the theme's
--  panel, a 1-pixel edge, the app-colour stripe on top. From the top:
--  stripe, header (icon, app name, identity), an optional note (where the
--  name came from when the manifest declares none), the thumbnail (fitted
--  into Thumbnail_Bound by Client_Thumbnails.Fit), then one row per window
--  up to Maximum_Rows and a "more" row past that.
--
--  Layout is pure and proved (tests/ui061): every part lies inside the
--  card, the rows do not overlap, and the card stands above the bar and
--  inside the screen whenever the screen has room for it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Client_Thumbnails;
with CuBit.UI_Metrics;
with Desktop_Deskbar_Style;

package Desktop_Task_Cards with SPARK_Mode is
   package M renames CuBit.UI_Metrics;
   package DBS renames Desktop_Deskbar_Style;
   package CT renames Client_Thumbnails;

   --  Geometry, in logical pixels, from the UI em: as wide as a group's
   --  window list, so the two read as one family.
   Width          : constant := DBS.List_Width;
   Padding        : constant := M.Space_M;
   Stripe_Height  : constant := DBS.Panel_Stripe;
   Header_Height  : constant := M.Control_Height;
   Note_Height    : constant := M.Line_Height + M.Space_XS;
   Row_Height     : constant := M.Compact_Row_Height;
   Row_Gap        : constant := M.Space_XS;
   Thumbnail_Gap  : constant := M.Space_S;
   --  The thumbnail's bound: the card's inner width, and 12 ems high.
   Thumbnail_Width_Bound  : constant := Width - 2 * Padding;
   Thumbnail_Height_Bound : constant := 12 * M.Base_Em;
   --  A focus or selection marker before a row's title.
   Marker_Size    : constant := M.Space_S;
   Marker_Gap     : constant := M.Space_S;
   --  Rows past this many are summed up in one "N more" row.
   Maximum_Rows   : constant := 8;
   --  Above the button, as the window list stands.
   Margin         : constant := DBS.Panel_Margin;

   Thumbnail_Bound : constant CT.Size := (Thumbnail_Width_Bound, Thumbnail_Height_Bound);

   --  The largest card: every part, the tallest thumbnail and every row.
   Maximum_Height : constant :=
     Stripe_Height + Header_Height + Note_Height + Thumbnail_Height_Bound + Thumbnail_Gap
     + (Maximum_Rows + 1) * (Row_Height + Row_Gap) + Padding;

   subtype Coordinate is Natural range 0 .. 65_535;
   type Box is record
      X, Y : Coordinate := 0;
      W, H : Coordinate := 0;
   end record;

   subtype Window_Count is Natural range 0 .. 65_535;
   subtype Shown_Rows is Natural range 0 .. Maximum_Rows;
   subtype Row_Index is Positive range 1 .. Maximum_Rows;
   type Row_Boxes is array (Row_Index) of Box;

   --  What the card shows: how many windows, whether a note line is
   --  needed, and the thumbnail's size (none when the window has no
   --  pixels yet).
   type Content is record
      Windows : Window_Count := 0;
      Note : Boolean := False;
      Has_Thumbnail : Boolean := False;
      Thumbnail : CT.Size := (1, 1);
   end record;

   function Valid (C : Content) return Boolean is
     (not C.Has_Thumbnail
      or else (C.Thumbnail.Width <= Thumbnail_Width_Bound
               and then C.Thumbnail.Height <= Thumbnail_Height_Bound));

   --  The parts, relative to the card's top left.
   type Layout is record
      Height : Coordinate := 0;
      Header, Note, Thumbnail : Box;
      Rows : Row_Boxes := [others => (others => 0)];
      Shown : Shown_Rows := 0;
      More : Box;            --  "N more" (H = 0: none)
   end record;

   function Inside (Part : Box; Height : Coordinate) return Boolean is
     (Part.X + Part.W <= Width and then Part.Y + Part.H <= Height);

   function Plan (C : Content) return Layout
     with Pre => Valid (C),
          Post => Plan'Result.Height <= Maximum_Height
                  and then Plan'Result.Shown = Natural'Min (C.Windows, Maximum_Rows)
                  and then Inside (Plan'Result.Header, Plan'Result.Height)
                  and then Inside (Plan'Result.Note, Plan'Result.Height)
                  and then Inside (Plan'Result.Thumbnail, Plan'Result.Height)
                  and then Inside (Plan'Result.More, Plan'Result.Height)
                  and then (for all R in 1 .. Plan'Result.Shown =>
                              Inside (Plan'Result.Rows (R), Plan'Result.Height))
                  --  Rows stand in order, apart.
                  and then (for all R in 2 .. Plan'Result.Shown =>
                              Plan'Result.Rows (R - 1).Y + Plan'Result.Rows (R - 1).H
                                <= Plan'Result.Rows (R).Y)
                  and then (C.Has_Thumbnail = (Plan'Result.Thumbnail.H > 0))
                  and then (if C.Has_Thumbnail then
                              Plan'Result.Thumbnail.W = C.Thumbnail.Width
                              and then Plan'Result.Thumbnail.H = C.Thumbnail.Height);

   --  Where the card stands: above the button whose left edge is Button_X,
   --  its bottom Margin above Bar_Top, kept within [Screen_Left,
   --  Screen_Right). Fits is False when the screen is too small for it.
   type Placement is record
      X, Y : Coordinate := 0;
      Fits : Boolean := False;
   end record;

   function Place
     (Button_X, Screen_Left, Screen_Right, Bar_Top : Coordinate; Height : Coordinate)
      return Placement
     with Post => (if Place'Result.Fits then
                     Place'Result.X >= Screen_Left
                     and then Place'Result.X + Width <= Screen_Right
                     and then Place'Result.Y + Height + Margin = Bar_Top);
end Desktop_Task_Cards;
