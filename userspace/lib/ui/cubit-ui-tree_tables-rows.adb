------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Tree_Tables.Rows is
   use CuBit.UI.Instrument;

   INSET : constant := 10;
   INDENT : constant := 16;
   TRI : constant := 9;
   SQUARE_SIZE : constant := 6;
   MARK_EDGE : constant := 3;
   GAP : constant := 5;
   HEADING_SEPARATOR : constant Character := '|';

   KEY_ENTER : constant := 16#1C#;
   KEY_SPACE : constant := 16#39#;
   KEY_UP    : constant := 16#48#;
   KEY_DOWN  : constant := 16#50#;
   KEY_LEFT  : constant := 16#4B#;
   KEY_RIGHT : constant := 16#4D#;
   KEY_PGUP  : constant := 16#49#;
   KEY_PGDN  : constant := 16#51#;
   KEY_HOME  : constant := 16#47#;
   KEY_END   : constant := 16#4F#;

   function Subtree_Last (M : Model; I : Item_Index) return Item_Index is
      Last : Item_Index := I;
   begin
      while Last < Item_Total (M) and then Depth (M, Last + 1) > Depth (M, I) loop
         Last := Last + 1;
      end loop;
      return Last;
   end Subtree_Last;

   procedure Refresh (V : in out View; M : Model) is
      I : Natural := 1;
   begin
      V.Visible_Total := 0;
      while I <= Item_Total (M) loop
         V.Visible_Total := V.Visible_Total + 1;
         V.Visible (V.Visible_Total) := I;
         I := (if Expanded (M, I) then I + 1 else Subtree_Last (M, I) + 1);
      end loop;
      if Position (V, V.Selected) = 0 then
         V.Selected := (if V.Visible_Total > 0 then V.Visible (1) else 1);
      end if;
      V.Top := Natural'Min (V.Top, Natural'Max (V.Visible_Total, 1) - 1);
   end Refresh;

   function Column_X (V : View; Area : Rect; C : Column_Index) return Integer is
     (Area.x + Integer (Area.w) * V.Starts (C) / Percent'Last + (if C = 1 then INSET else 0));
   function Column_End (V : View; Area : Rect; C : Column_Index) return Integer is
     (if C = V.Columns then Area.x + Integer (Area.w) - INSET
      else Area.x + Integer (Area.w) * V.Starts (C + 1) / Percent'Last - INSET / 2);

   procedure Draw
     (C : Canvas; Area : Rect; M : Model; V : View; Headings : String; Focused : Boolean;
      Colors : CuBit.UI.Instrument.Palette)
   is
      P : CuBit.UI.Instrument.Palette renames Colors;
      Body_Area : constant Rect := (Area.x, Area.y + ROW_HEIGHT, Area.w,
                                    (if Area.h > ROW_HEIGHT then Area.h - ROW_HEIGHT else 0));
      function Tone_Color (T : Tone) return CuBit.UI.Color is
        (case T is
           when Normal => P.Text, when Link => P.Edge, when Danger => P.Danger, when Type_Tone => P.Type_Name,
           when Kind_Tone => P.Kind_Enum, when Muted_Tone => P.Muted);
   begin
      --  The headings, '|' between columns.
      Fill (C, Area.x, Area.y, Area.w, ROW_HEIGHT, P.Pane_Alt);
      declare
         Start : Positive := Headings'First;
         Col : Column_Index := 1;
      begin
         for K in Headings'First .. Headings'Last + 1 loop
            if K > Headings'Last or else Headings (K) = HEADING_SEPARATOR then
               Sans (C, Column_X (V, Area, Col), Text_Y (Area.y, ROW_HEIGHT), Headings (Start .. K - 1), P.Dim);
               exit when Col = V.Columns;
               Col := Col + 1;
               Start := K + 1;
            end if;
         end loop;
      end;
      for Row in 1 .. Rows_In (Area) loop
         declare
            K : constant Natural := V.Top + Row;
         begin
            exit when K > V.Visible_Total;
            declare
               I : constant Item_Index := V.Visible (K);
               Y : constant Integer := Body_Area.y + (Row - 1) * ROW_HEIGHT;
               TY : constant Integer := Text_Y (Y, ROW_HEIGHT);
               X : constant Integer := Column_X (V, Area, 1) + Depth (M, I) * INDENT;
               Hot : constant Boolean := Marked (M, I);
               Has_Square : Boolean;
               Square_Color : constant CuBit.UI.Color := Square (M, I, Has_Square);
               Text_X : constant Integer := X + TRI + GAP + (if Has_Square then SQUARE_SIZE + GAP + 1 else 0);
            begin
               if I = V.Selected then
                  Fill (C, Area.x, Y, Area.w, ROW_HEIGHT, (if Focused then P.Selection else P.Caller_Line));
               elsif K mod 2 = 0 then
                  Fill (C, Area.x, Y, Area.w, ROW_HEIGHT, P.Pane_Alt);
               end if;
               if Hot then
                  Fill (C, Column_X (V, Area, V.Marked_Column) - INSET / 2, Y,
                        Column_End (V, Area, V.Marked_Column) - Column_X (V, Area, V.Marked_Column) + INSET,
                        ROW_HEIGHT, P.Changed_Fill);
                  Fill (C, Area.x, Y, MARK_EDGE, ROW_HEIGHT, P.Changed);
               end if;
               for Col in 1 .. V.Columns loop
                  declare
                     Left : constant Integer := (if Col = 1 then Text_X else Column_X (V, Area, Col));
                     Right : constant Integer := Column_End (V, Area, Col);
                     Clip : constant Canvas := With_Clip
                       (C, (Natural'Max (Left, 0), Natural'Max (Y, 0), Natural'Max (Right - Left, 0), ROW_HEIGHT));
                     Text : constant String := Fits (Cell (M, I, Col), Right - Left);
                     Note : constant String := Cell_Note (M, I, Col);
                     T : constant Tone := Cell_Tone (M, I, Col);
                  begin
                     if Col = 1 then
                        if Expandable (M, I) then
                           Triangle (C, X, Y + (ROW_HEIGHT - TRI) / 2, TRI, Expanded (M, I), P.Muted);
                        end if;
                        if Has_Square then
                           Fill (C, X + TRI + GAP, Y + (ROW_HEIGHT - SQUARE_SIZE) / 2, SQUARE_SIZE, SQUARE_SIZE,
                                 Square_Color);
                        end if;
                     end if;
                     Mono (Clip, Left, TY, Text,
                           (if Hot and then Col = V.Marked_Column and then T = Normal then P.Changed
                            elsif Is_Pager (M, I) then P.Dim else Tone_Color (T)));
                     if Note'Length > 0
                       and then Code_Text_Width (Text) + CHAR_WIDTH + Code_Text_Width (Note) <= Right - Left
                     then
                        Mono (Clip, Left + Code_Text_Width (Text) + CHAR_WIDTH, TY, Note,
                              (if T = Danger then P.Danger else P.Dim));
                     end if;
                  end;
               end loop;
            end;
         end;
      end loop;
   end Draw;

   procedure Key (V : in out View; M : Model; Area : Rect; Code : Interfaces.Unsigned_64;
                  Result : out Wish; Item : out Item_Index) is
      K : constant Natural := Position (V, V.Selected);
      Page : constant Positive := Natural'Max (1, Rows_In (Area) - 1);
   begin
      Result := No_Wish;
      Item := V.Selected;
      if K = 0 then
         return;
      end if;
      case Code is
         when KEY_UP => if K > 1 then V.Selected := V.Visible (K - 1); Result := Moved; end if;
         when KEY_DOWN => if K < V.Visible_Total then V.Selected := V.Visible (K + 1); Result := Moved; end if;
         when KEY_HOME => V.Selected := V.Visible (1); Result := Moved;
         when KEY_END => V.Selected := V.Visible (V.Visible_Total); Result := Moved;
         when KEY_PGUP => V.Selected := V.Visible (Natural'Max (1, K - Page)); Result := Moved;
         when KEY_PGDN => V.Selected := V.Visible (Natural'Min (V.Visible_Total, K + Page)); Result := Moved;
         when KEY_RIGHT =>
            if Expandable (M, V.Selected) and then not Expanded (M, V.Selected) then
               Result := Expand;
            elsif Expanded (M, V.Selected) and then K < V.Visible_Total then
               V.Selected := V.Visible (K + 1);
               Result := Moved;
            end if;
         when KEY_LEFT =>
            if Expanded (M, V.Selected) then
               Result := Collapse;
            else
               for J in reverse 1 .. K - 1 loop
                  if Depth (M, V.Visible (J)) < Depth (M, V.Selected) then
                     V.Selected := V.Visible (J);
                     Result := Moved;
                     exit;
                  end if;
               end loop;
            end if;
         when KEY_ENTER | KEY_SPACE =>
            Result := (if Is_Pager (M, V.Selected) then Next_Page
                       elsif Expandable (M, V.Selected) then Toggle else No_Wish);
         when others => null;
      end case;
      if Result = Moved then
         Ensure_Visible (V, Rows_In (Area));
      end if;
      Item := V.Selected;
   end Key;

   procedure Click (V : in out View; M : Model; Area : Rect; X, Y : Natural; Result : out Wish; Item : out Item_Index) is
      Row : constant Integer := (Integer (Y) - Area.y - ROW_HEIGHT) / ROW_HEIGHT + 1;
   begin
      Result := No_Wish;
      Item := V.Selected;
      if Integer (Y) < Area.y + ROW_HEIGHT or else Row < 1 or else V.Top + Row > V.Visible_Total then
         return;
      end if;
      V.Selected := V.Visible (V.Top + Row);
      Item := V.Selected;
      Result := Moved;
      declare
         Disclosure : constant Integer := Column_X (V, Area, 1) + Depth (M, V.Selected) * INDENT;
      begin
         if Expandable (M, V.Selected) and then Integer (X) >= Disclosure - GAP
           and then Integer (X) < Disclosure + INDENT + GAP
         then
            Result := Toggle;
         elsif Is_Pager (M, V.Selected) then
            Result := Next_Page;
         end if;
      end;
   end Click;

   procedure Wheel (V : in out View; Steps : Integer) is
   begin
      if Steps > 0 then
         V.Top := (if V.Top > 0 then V.Top - 1 else 0);
      elsif V.Top + 1 < V.Visible_Total then
         V.Top := V.Top + 1;
      end if;
   end Wheel;
end CuBit.UI.Tree_Tables.Rows;
