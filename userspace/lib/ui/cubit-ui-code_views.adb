------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Code_Views is
   use CuBit.UI.Instrument;
   MARK_W : constant := 18;
   INSET : constant := 10;
   FOLLOW_FRACTION : constant := 3;

   function Color_Of (Style : Token_Style; Colors : CuBit.UI.Instrument.Palette) return CuBit.UI.Color is
     (case Style is
        when Plain => Colors.Text, when Keyword => Colors.Keyword, when Type_Name => Colors.Type_Name,
        when Number => Colors.Number, when Literal => Colors.Literal, when Comment => Colors.Comment,
        when Attribute => Colors.Attribute, when Punctuation => Colors.Punctuation);

   procedure Show (V : in out View; Line : Natural; Area : Rect) is
      Rows : constant Natural := Rows_In (Area);
   begin
      if V.Follow and then Line > 0 and then Rows > 0 then
         V.Top := (if Line > Rows / FOLLOW_FRACTION then Line - Rows / FOLLOW_FRACTION - 1 else 0);
      end if;
   end Show;

   procedure Scroll (V : in out View; Lines : Integer; Line_Total : Natural) is
   begin
      V.Follow := False;
      V.Top := Natural (Integer'Max (0, Integer'Min (Integer (Natural'Max (Line_Total, 1)) - 1,
                                                     Integer (V.Top) + Lines)));
   end Scroll;

   function Code_X (Area : Rect) return Integer is (Area.x + MARK_W + NUMBER_CHARS * CHAR_WIDTH + 2 * INSET);
end CuBit.UI.Code_Views;
