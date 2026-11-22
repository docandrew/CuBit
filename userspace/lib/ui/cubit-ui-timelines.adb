------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Timelines is
   use CuBit.UI.Instrument;

   LINE_H : constant := CuBit.UI_Metrics.Line_Height;
   INSET : constant := 10;
   LANE_H : constant := 20;
   TICK_MINOR : constant := 12;
   TICK_MAJOR : constant := 120;
   PLAYHEAD_W : constant := 2;
   MARKER_W : constant := 3;
   LANE_NAME_CHARS : constant := 8;

   KEY_J : constant := 16#24#;
   KEY_K : constant := 16#25#;
   KEY_L : constant := 16#26#;
   KEY_COMMA : constant := 16#33#;
   KEY_PERIOD : constant := 16#34#;

   function Lane_Of (Name : String) return Lane is
      N : constant Natural := Natural'Min (Name'Length, NAME_CAPACITY);
      Result : Lane;
   begin
      Result.Name (1 .. N) := Name (Name'First .. Name'First + N - 1);
      Result.Length := N;
      return Result;
   end Lane_Of;

   procedure Set_Title (S : in out Strip; Title, Badge : String) is
      T : constant Natural := Natural'Min (Title'Length, NAME_CAPACITY);
      B : constant Natural := Natural'Min (Badge'Length, NAME_CAPACITY);
   begin
      S.Title (1 .. T) := Title (Title'First .. Title'First + T - 1);
      S.Title_Length := T;
      S.Badge (1 .. B) := Badge (Badge'First .. Badge'First + B - 1);
      S.Badge_Length := B;
   end Set_Title;

   procedure Set_Notice (S : in out Strip; Notice, Keys : String) is
      N : constant Natural := Natural'Min (Notice'Length, NOTICE_CAPACITY);
      K : constant Natural := Natural'Min (Keys'Length, NOTICE_CAPACITY);
   begin
      S.Notice (1 .. N) := Notice (Notice'First .. Notice'First + N - 1);
      S.Notice_Length := N;
      S.Keys (1 .. K) := Keys (Keys'First .. Keys'First + K - 1);
      S.Keys_Length := K;
   end Set_Notice;

   function Track_X (Area : Rect) return Integer is (Area.x + LABEL_WIDTH);
   function Track_W (Area : Rect) return Integer is (Integer (Area.w) - LABEL_WIDTH - INSET);

   procedure Draw (C : Canvas; Area : Rect; S : Strip; Colors : CuBit.UI.Instrument.Palette) is
      P : CuBit.UI.Instrument.Palette renames Colors;
      TX : constant Integer := Track_X (Area);
      TW : constant Integer := Track_W (Area);
      Lanes : constant Natural := Natural'Max (1, Natural'Min (S.Lane_Total, (Area.h - 2 * INSET) / LANE_H));
      Lane_Y0 : constant Integer := Area.y + (Integer (Area.h) - Lanes * LANE_H) / 2;
      Badge_Y : constant Integer := Area.y + INSET / 2 + LINE_H + 4;
      Head_X : constant Integer := TX + TW * S.Playhead / Permille'Last;
      Notice : constant String := S.Notice (1 .. S.Notice_Length);
      Keys : constant String := S.Keys (1 .. S.Keys_Length);
   begin
      Fill (C, Area.x, Area.y, Area.w, Area.h, P.Pane);
      Fill (C, Area.x, Area.y, Area.w, 1, P.Rule);
      Bold (C, Area.x + INSET, Area.y + INSET / 2, S.Title (1 .. S.Title_Length), P.Muted);
      if S.Badge_Length > 0 then
         Fill (C, Area.x + INSET, Badge_Y, Code_Text_Width (S.Badge (1 .. S.Badge_Length)) + 2 * CuBit.UI_Metrics.Space_S,
               LINE_H, P.Header);
         Mono (C, Area.x + INSET + CuBit.UI_Metrics.Space_S, Badge_Y, S.Badge (1 .. S.Badge_Length),
               (if S.Enabled then P.Good else P.Warn));
      end if;
      for L in 1 .. Lanes loop
         declare
            Y : constant Integer := Lane_Y0 + (L - 1) * LANE_H;
            Name_Chars : constant Natural := (if L <= S.Lane_Total then Natural'Min (S.Lanes (L).Length, LANE_NAME_CHARS)
                                              else 0);
         begin
            Fill (C, TX, Y + 1, TW, LANE_H - 2, (if L mod 2 = 0 then P.Hatch_Alt else P.Hatch));
            if Name_Chars > 0 then
               Mono (C, TX - CHAR_WIDTH * (Name_Chars + 1), Y + (LANE_H - LINE_H) / 2, S.Lanes (L).Name (1 .. Name_Chars),
                     P.Dim);
            end if;
         end;
      end loop;
      declare
         X : Integer := TX;
      begin
         while X < TX + TW loop
            declare
               Major : constant Boolean := (X - TX) mod TICK_MAJOR = 0;
            begin
               Fill (C, X, Lane_Y0 - (if Major then 6 else 3), 1, (if Major then 6 else 3), P.Dim);
            end;
            X := X + TICK_MINOR;
         end loop;
      end;
      for M in 1 .. S.Marker_Total loop
         if S.Markers (M).Lane <= Lanes then
            Fill (C, TX + TW * S.Markers (M).At_Position / Permille'Last - MARKER_W / 2,
                  Lane_Y0 + (S.Markers (M).Lane - 1) * LANE_H + 3, MARKER_W, LANE_H - 6,
                  (case S.Markers (M).Kind is
                     when Event_Mark => P.Edge, when Write_Mark => P.Changed,
                     when Fault_Mark => P.Danger, when Bookmark_Mark => P.Good));
         end if;
      end loop;
      Fill (C, Head_X - PLAYHEAD_W, Lane_Y0 - 8, PLAYHEAD_W, Lanes * LANE_H + 10, P.Accent);
      if S.Playhead = Permille'Last then
         Mono (C, Head_X - PLAYHEAD_W - Code_Text_Width ("now") - CHAR_WIDTH, Lane_Y0 - LINE_H - 4, "now", P.Accent);
      end if;
      if Notice'Length > 0 and then not S.Enabled then
         declare
            MW : constant Natural := UI_Text_Width (Notice) + 2 * INSET;
            MX : constant Integer := TX + (TW - MW) / 2;
            MY : constant Integer := Lane_Y0 + (Lanes * LANE_H - LINE_H - 6) / 2;
         begin
            Fill (C, MX, MY, MW, LINE_H + 6, P.Header);
            Frame (C, MX, MY, MW, LINE_H + 6, 1, P.Rule);
            Sans (C, MX + INSET, MY + 3, Notice, P.Muted);
            Mono (C, MX + MW + 2 * INSET, MY + 3, Fits (Keys, TX + TW - (MX + MW + 2 * INSET)), P.Dim);
         end;
      end if;
   end Draw;

   procedure Position_At (S : Strip; Area : Rect; X : Natural; Position : out Permille; Inside : out Boolean) is
      TX : constant Integer := Track_X (Area);
      TW : constant Integer := Track_W (Area);
   begin
      Inside := S.Enabled and then TW > 0 and then Integer (X) >= TX and then Integer (X) <= TX + TW;
      Position := (if Inside then Permille ((Integer (X) - TX) * Permille'Last / TW) else S.Playhead);
   end Position_At;

   function Scrub_Of (S : Strip; Code : Interfaces.Unsigned_64) return Scrub is
     (if not S.Enabled then No_Scrub
      else (case Code is
              when KEY_J => Reverse_Play, when KEY_K => Pause, when KEY_L => Forward_Play,
              when KEY_COMMA => Step_Back, when KEY_PERIOD => Step_Forward,
              when others => No_Scrub));
end CuBit.UI.Timelines;
