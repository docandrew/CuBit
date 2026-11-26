------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Client_Thumbnails;
with CuBit.Benchmark_Clock;
with CuBit.Published_Clock;
with CuBit.UI; use CuBit.UI;
with CuBit.UI_Metrics;
with Desktop_Canvas_Icons;
with Desktop_Timing_Policy;

package body Desktop_Task_Card is
   package CT renames Client_Thumbnails;
   package M renames CuBit.UI_Metrics;
   use type System.Address;

   Bytes_Per_Pixel : constant := 4;
   Pitch : constant := TC.Width * Bytes_Per_Pixel;

   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return System.Address
     with Import, Convention => C, External_Name => "cualloc_allocate_zeroed";
   Heap_Alignment : constant := 64;

   --  The layer: TC.Width wide, room for the tallest card, taken once.
   Store : System.Address := System.Null_Address;
   Rows_Drawn : Natural := 0;
   Plan : TC.Layout;
   --  The thumbnail being drawn: its source's size and the next target row.
   Drawn_Picture : Picture;
   Fitted : CT.Size := (1, 1);
   Next_Row : Natural := 0;
   Band_Microseconds : Unsigned_64 := 0;

   function Layout return TC.Layout is (Plan);
   function Buffer return System.Address is (Store);
   function Buffer_Height return Natural is (Rows_Drawn);
   function Buffer_Pitch return Natural is (Pitch);
   function Last_Band_Microseconds return Unsigned_64 is (Band_Microseconds);
   function Complete return Boolean is
     (not Drawn_Picture.Present or else Next_Row >= Fitted.Height);
   procedure Restart is
   begin
      Next_Row := 0;
   end Restart;

   function Usable (P : Picture) return Boolean is
     (P.Present and then P.Address /= System.Null_Address
      and then P.Width in 1 .. CT.Maximum_Source_Extent
      and then P.Height in 1 .. CT.Maximum_Source_Extent
      and then P.Pitch_Pixels in P.Width .. CT.Maximum_Source_Extent);

   function Text_Y (Top, Height : Natural) return Natural is
     (Top + (if Height > UI_Text_Height then (Height - UI_Text_Height) / 2 else 0));

   procedure Render (V : View; OK : out Boolean) is
      C : Canvas;
      Colors : constant DBS.Palette := V.Colors;
      Note : constant String := Note_Text (V.Source);
      Shown_Picture : constant Boolean := Usable (V.Source_Picture);
   begin
      OK := True;
      if Store = System.Null_Address then
         Store := Allocate_Zeroed (Unsigned_64 (TC.Maximum_Height) * Unsigned_64 (Pitch), Heap_Alignment);
         if Store = System.Null_Address then
            OK := False;
            return;
         end if;
      end if;
      Fitted := (if Shown_Picture
                 then CT.Fit ((V.Source_Picture.Width, V.Source_Picture.Height), TC.Thumbnail_Bound)
                 else (1, 1));
      Plan := TC.Plan ((Windows => V.Windows, Note => Note'Length > 0,
                        Has_Thumbnail => Shown_Picture, Thumbnail => Fitted));
      Drawn_Picture := (if Shown_Picture then V.Source_Picture else (others => <>));
      Next_Row := 0;
      Rows_Drawn := Plan.Height;
      C := (addr => Store, width => TC.Width, height => Plan.Height, pitch => Pitch, others => <>);

      --  Flat and square: the panel, a 1-pixel edge, the app-colour stripe.
      Fill_Rect (C, (0, 0, TC.Width, Plan.Height), Colors.Panel);
      Stroke_Rect (C, (0, 0, TC.Width, Plan.Height), Colors.Panel_Edge, Colors.Panel_Edge);
      Fill_Rect (C, (0, 0, TC.Width, TC.Stripe_Height), V.Stripe);

      --  Header: icon, the app's name, its identity (quiet, right-aligned).
      declare
         H : constant TC.Box := Plan.Header;
         Name_X : constant Natural := H.X + Desktop_Icons.ICON_SIZE + DBS.Icon_Label_Gap;
         Identity : constant String := V.Identity (1 .. V.Identity_Length);
         Name : constant String :=
           (if V.Name_Length > 0 then V.Name (1 .. V.Name_Length) else Identity);
         Identity_Room : constant Natural :=
           (if V.Name_Length > 0 and then Identity'Length > 0
              and then H.X + H.W > Name_X + Emphasis_Text_Width (Name) + M.Space_L
            then H.X + H.W - (Name_X + Emphasis_Text_Width (Name) + M.Space_L) else 0);
         Shown_Identity : constant String := Fit_Code_Text (Identity, Identity_Room);
      begin
         Desktop_Canvas_Icons.Draw
           (C, V.Icon, H.X, H.Y + (H.H - Desktop_Icons.ICON_SIZE) / 2, Colors.Panel);
         Draw_Emphasis_Text_Transparent
           (C, Name_X, Text_Y (H.Y, H.H),
            Fit_UI_Text (Name, (if H.X + H.W > Name_X then H.X + H.W - Name_X else 0)),
            Colors.Active_Text);
         if Identity_Room > 0 and then Shown_Identity'Length > 0 then
            Draw_Code_Text_Transparent
              (C, H.X + H.W - Code_Text_Width (Shown_Identity),
               H.Y + (if H.H > Code_Text_Height then (H.H - Code_Text_Height) / 2 else 0),
               Shown_Identity, Colors.Muted);
         end if;
      end;

      --  Where the name came from, when not from the app's own manifest.
      if Plan.Note.H > 0 then
         Draw_UI_Text_Transparent
           (C, Plan.Note.X, Text_Y (Plan.Note.Y, Plan.Note.H), Fit_UI_Text (Note, Plan.Note.W), Colors.Muted);
      end if;

      --  The thumbnail's frame; Advance fills it band by band.
      if Plan.Thumbnail.H > 0 then
         Fill_Rect (C, (Plan.Thumbnail.X, Plan.Thumbnail.Y, Plan.Thumbnail.W, Plan.Thumbnail.H), Colors.Section);
         if Plan.Thumbnail.X > 0 and then Plan.Thumbnail.Y > 0 then
            Stroke_Rect (C, (Plan.Thumbnail.X - 1, Plan.Thumbnail.Y - 1, Plan.Thumbnail.W + 2, Plan.Thumbnail.H + 2),
                         Colors.Button_Edge, Colors.Button_Edge);
         end if;
      end if;

      --  One row per window: the pictured one framed and marked, the
      --  focused one emphasized and tagged, minimized ones dimmed and tagged.
      for R in 1 .. Plan.Shown loop
         declare
            B : constant TC.Box := Plan.Rows (R);
            Item : Row renames V.Rows (R);
            Title : constant String := Item.Title (1 .. Item.Title_Length);
            Tag : constant String :=
              (if Item.Focused then "active" elsif Item.Minimized then "minimized" else "");
            Tag_W : constant Natural := (if Tag'Length > 0 then UI_Text_Width (Tag) + M.Space_M else 0);
            Text_X : constant Natural := B.X + M.Space_S + TC.Marker_Size + TC.Marker_Gap;
            Room : constant Natural :=
              (if B.X + B.W > Text_X + Tag_W + M.Space_S then B.X + B.W - Text_X - Tag_W - M.Space_S else 0);
            Ink : constant Color :=
              (if Item.Focused then Colors.Active_Text
               elsif Item.Minimized then Colors.Dimmed_Text else Colors.Button_Text);
            Shown : constant String := Fit_UI_Text ((if Title'Length > 0 then Title else "Untitled"), Room);
         begin
            if Item.Pictured then
               Fill_Rect (C, (B.X, B.Y, B.W, B.H), Colors.Row);
               Stroke_Rect (C, (B.X, B.Y, B.W, B.H), Colors.Button_Edge, Colors.Button_Edge);
               Fill_Rect (C, (B.X + M.Space_S, B.Y + (B.H - TC.Marker_Size) / 2, TC.Marker_Size, TC.Marker_Size),
                          V.Stripe);
            end if;
            if Item.Focused then
               Draw_Emphasis_Text_Transparent (C, Text_X, Text_Y (B.Y, B.H), Shown, Ink);
            else
               Draw_UI_Text_Transparent (C, Text_X, Text_Y (B.Y, B.H), Shown, Ink);
            end if;
            if Tag'Length > 0 then
               Draw_UI_Text_Transparent
                 (C, B.X + B.W - M.Space_S - UI_Text_Width (Tag), Text_Y (B.Y, B.H), Tag, Colors.Muted);
            end if;
         end;
      end loop;
      if Plan.More.H > 0 then
         declare
            Rest : constant Natural := V.Windows - Plan.Shown;
            Image : constant String := Natural'Image (Rest);
         begin
            Draw_UI_Text_Transparent
              (C, Plan.More.X + M.Space_S + TC.Marker_Size + TC.Marker_Gap, Text_Y (Plan.More.Y, Plan.More.H),
               Image (Image'First + 1 .. Image'Last) & " more", Colors.Muted);
         end;
      end if;
   end Render;

   procedure Advance (Current : Picture; Rows : out Compositor_Source_Content.Row_Band) is
   begin
      Rows := Compositor_Source_Content.Empty_Band;
      if Store = System.Null_Address or else not Usable (Current) or else not Usable (Drawn_Picture)
        or else Current.Width /= Drawn_Picture.Width or else Current.Height /= Drawn_Picture.Height
        or else Current.Pitch_Pixels /= Drawn_Picture.Pitch_Pixels
        or else Plan.Thumbnail.H /= Fitted.Height or else Next_Row >= Fitted.Height
      then
         return;
      end if;
      Drawn_Picture.Address := Current.Address;
      declare
         Count : constant Natural := Natural'Min (Thumbnail_Band_Rows, Fitted.Height - Next_Row);
         Source : CT.Pixel_Array (0 .. Current.Height * Current.Pitch_Pixels - 1)
           with Import, Address => Current.Address;
         Target : CT.Pixel_Array (0 .. Rows_Drawn * TC.Width - 1)
           with Import, Address => Store;
         Started : constant Unsigned_64 :=
           (if Desktop_Timing_Policy.Enabled then CuBit.Benchmark_Clock.Read_Counter else 0);
      begin
         CT.Scale (Source, (Current.Width, Current.Height), Current.Pitch_Pixels,
                   Target, TC.Width, Plan.Thumbnail.X, Plan.Thumbnail.Y, Fitted.Width, Fitted.Height,
                   Next_Row, Count);
         if Desktop_Timing_Policy.Enabled then
            declare
               Hz : constant Unsigned_64 := CuBit.Published_Clock.Counter_Frequency;
               Ticks : constant Unsigned_64 := CuBit.Benchmark_Clock.Read_Counter - Started;
            begin
               Band_Microseconds :=
                 (if Hz >= 1_000_000 then Ticks / (Hz / 1_000_000) else 0);
            end;
         end if;
         Rows := Compositor_Source_Content.Band
           (Plan.Thumbnail.Y + Next_Row, Plan.Thumbnail.Y + Next_Row + Count);
         Next_Row := Next_Row + Count;
      end;
   end Advance;
end Desktop_Task_Card;
