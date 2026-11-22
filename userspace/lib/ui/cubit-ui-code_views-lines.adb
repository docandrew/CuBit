------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Code_Views.Lines is
   use CuBit.UI.Instrument;
   MARK_W : constant := 18;
   INSET : constant := 10;
   MARK_EDGE : constant := 3;
   ARROW : constant := 10;
   DOT : constant := 10;

   procedure Draw (C : Canvas; Area : Rect; M : Model; V : View; Colors : CuBit.UI.Instrument.Palette) is
      P : CuBit.UI.Instrument.Palette renames Colors;
      X0 : constant Integer := Code_X (Area);
      Code_W : constant Integer := Integer (Area.w) - (X0 - Area.x) - INSET;
      Code_C : constant Canvas := With_Clip (C, (Natural'Max (X0, 0), Area.y, Natural'Max (Code_W, 0), Area.h));
   begin
      Fill (C, Area.x, Area.y, MARK_W + NUMBER_CHARS * CHAR_WIDTH + INSET + INSET / 2, Area.h, P.Pane_Alt);
      for Row in 1 .. Rows_In (Area) loop
         declare
            N : constant Natural := V.Top + Row;
            Y : constant Integer := Area.y + (Row - 1) * ROW_HEIGHT;
            TY : constant Integer := Text_Y (Y, ROW_HEIGHT);
         begin
            exit when N > Line_Total (M);
            declare
               Mark : constant Line_Marker := Marker (M, N);
               Num : constant String := Dec (Interfaces.Unsigned_64 (N));
               Text : String := Line (M, N);
               End_X : Integer;
               --  Runs come in order; the gaps between them are Plain.
               Next : Positive := Text'First;
               procedure Plain_Up_To (Last : Natural) is
               begin
                  if Last >= Next then
                     Mono (Code_C, X0 + (Next - Text'First) * CHAR_WIDTH, TY, Text (Next .. Last), P.Text);
                     Next := Last + 1;
                  end if;
               end Plain_Up_To;
               procedure Paint (First, Last : Positive; Style : Token_Style) is
               begin
                  if First >= Next and then Last <= Text'Last and then First <= Last then
                     Plain_Up_To (First - 1);
                     Mono (Code_C, X0 + (First - Text'First) * CHAR_WIDTH, TY, Text (First .. Last),
                           Color_Of (Style, P));
                     Next := Last + 1;
                  end if;
               end Paint;
            begin
               case Mark is
                  when Current_Line =>
                     Fill (C, Area.x, Y, Area.w, ROW_HEIGHT, P.Current_Line);
                     Fill (C, Area.x, Y, MARK_EDGE, ROW_HEIGHT, P.Current_Mark);
                     Triangle (C, Area.x + MARK_EDGE + 3, Y + (ROW_HEIGHT - ARROW) / 2, ARROW, False, P.Current_Mark);
                  when Caller_Line =>
                     Fill (C, Area.x, Y, Area.w, ROW_HEIGHT, P.Caller_Line);
                     Fill (C, Area.x, Y, MARK_EDGE, ROW_HEIGHT, P.Muted);
                  when Breakpoint =>
                     Fill (C, Area.x + MARK_EDGE + 2, Y + (ROW_HEIGHT - DOT) / 2, DOT, DOT, P.Danger);
                  when Disabled_Breakpoint =>
                     Frame (C, Area.x + MARK_EDGE + 2, Y + (ROW_HEIGHT - DOT) / 2, DOT, DOT, 2, P.Danger);
                  when No_Marker => null;
               end case;
               Mono (C, Area.x + MARK_W + (NUMBER_CHARS - Num'Length) * CHAR_WIDTH, TY, Num,
                     (case Mark is
                        when Current_Line => P.Current_Mark, when Caller_Line => P.Muted, when others => P.Dim));
               for Ch of Text loop
                  if Ch = ASCII.HT then
                     Ch := ' ';
                  end if;
               end loop;
               if Text'Length > 0 then
                  Colorize (M, N, Text, Paint'Access);
                  Plain_Up_To (Text'Last);
               end if;
               declare
                  Note : constant String := Annotation (M, N);
                  Hot : constant Boolean := Note'Length > 0 and then Annotation_Marked (M, N);
                  Note_W : constant Natural := Code_Text_Width (Note) + 2 * CuBit.UI_Metrics.Space_S;
               begin
                  End_X := X0 + (Text'Length + 3) * CHAR_WIDTH;
                  if Note'Length > 0 and then End_X + Note_W <= X0 + Code_W then
                     Fill (Code_C, End_X, Y + 2, Note_W, ROW_HEIGHT - 4, (if Hot then P.Changed_Fill else P.Header));
                     Mono (Code_C, End_X + CuBit.UI_Metrics.Space_S, TY, Note, (if Hot then P.Changed else P.Muted));
                  end if;
               end;
            end;
         end;
      end loop;
   end Draw;
end CuBit.UI.Code_Views.Lines;
