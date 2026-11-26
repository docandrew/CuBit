------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The Deskbar's hover card (UI-061), drawn with the toolkit into its own
--  layer, which Desktop composes above the bar like the Deskbar panel.
--
--  @description
--  The card names the app as its task button does, says where that name
--  came from when the manifest declares none, shows a live thumbnail of one
--  of its windows and lists every window's title: the focused one in the
--  emphasis face and marked "active", minimized ones dimmed and marked
--  "minimized" (words, not colour alone), the pictured one framed.
--
--  The thumbnail is scaled by Client_Thumbnails from the window's retained
--  frame, in bands of Thumbnail_Band_Rows per call to Advance, so no single
--  frame pays for the whole picture. The pixels stay in this layer, which
--  only Desktop draws and no app can read (privacy).
--
--  The layer's memory is taken the first time a card shows: until then the
--  card costs nothing.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Interfaces;
with Compositor_Source_Content;
with CuBit.Theme;
with Desktop_Deskbar_Style;
with Desktop_Icons;
with Desktop_Task_Cards;

package Desktop_Task_Card is
   package TC renames Desktop_Task_Cards;
   package DBS renames Desktop_Deskbar_Style;

   --  Where the app's name came from: its manifest (signed with the
   --  executable), the launcher entry that started it (its menu label),
   --  Desktop itself (Settings), or nowhere (the identity stands in).
   type Name_Source is (Manifest, Launcher, Desktop_Itself, Identity_Only, Unknown);
   --  The note under the name, empty when the name is the app's own.
   function Note_Text (Source : Name_Source) return String is
     (case Source is
         when Manifest | Desktop_Itself => "",
         when Launcher => "label from the launcher",
         when Identity_Only => "no declared name",
         when Unknown => "unknown app");

   Maximum_Name : constant := 32;
   Maximum_Identity : constant := 64;
   Maximum_Title : constant := 23;

   type Row is record
      Title : String (1 .. Maximum_Title) := [others => ' '];
      Title_Length : Natural range 0 .. Maximum_Title := 0;
      Focused, Minimized, Pictured : Boolean := False;
   end record;
   type Row_Array is array (TC.Row_Index) of Row;

   --  The window whose pixels the thumbnail shows (Present False: none
   --  yet). The mapping must stay valid while Advance runs.
   type Picture is record
      Present : Boolean := False;
      Address : System.Address := System.Null_Address;
      Width, Height : Natural := 0;
      Pitch_Pixels : Natural := 0;
   end record;

   type View is record
      Name : String (1 .. Maximum_Name) := [others => ' '];
      Name_Length : Natural range 0 .. Maximum_Name := 0;
      Source : Name_Source := Unknown;
      Identity : String (1 .. Maximum_Identity) := [others => ' '];
      Identity_Length : Natural range 0 .. Maximum_Identity := 0;
      Icon : Desktop_Icons.Icon_ID := Desktop_Icons.Start;
      Stripe : CuBit.Theme.Color := 0;
      Colors : DBS.Palette := DBS.Alloy_Light;
      Windows : TC.Window_Count := 0;
      Rows : Row_Array;
      Source_Picture : Picture;
   end record;

   --  Lay out and draw the whole card except the thumbnail's pixels, which
   --  start over (Advance draws them). False: no memory for the layer.
   procedure Render (V : View; OK : out Boolean);

   --  Rows of the thumbnail drawn per Advance: about a quarter of a
   --  millisecond of scaling on the test host (tests/ui061 "TIME:").
   Thumbnail_Band_Rows : constant := 32;

   --  Draw the thumbnail's next band from Picture (the same window and size
   --  as Render's, or nothing happens); Rows: the layer rows it changed
   --  (empty when the picture is complete). Restart: draw it again from
   --  the top (new pixels in the window).
   procedure Advance (Current : Picture; Rows : out Compositor_Source_Content.Row_Band);
   procedure Restart;
   function Complete return Boolean;

   function Layout return TC.Layout;
   function Buffer return System.Address;
   function Buffer_Width return Natural is (TC.Width);
   function Buffer_Height return Natural;
   function Buffer_Pitch return Natural;

   --  The last scale's cost in timing builds (Desktop_Timing_Policy), else 0.
   function Last_Band_Microseconds return Interfaces.Unsigned_64;
end Desktop_Task_Card;
