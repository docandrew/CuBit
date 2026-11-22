------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Code view over a caller's lines (CuBit.UI.Code_Views)
------------------------------------------------------------------------------
generic
   type Model (<>) is limited private;
   with function Line_Total (M : Model) return Natural;
   with function Line (M : Model; N : Positive) return String;
   with function Marker (M : Model; N : Positive) return Line_Marker;
   --  Paint the line's runs: Paint (First, Last, Style) for each coloured
   --  run, positions in Text (unpainted characters are Plain).
   with procedure Colorize
     (M : Model; N : Positive; Text : String;
      Paint : not null access procedure (First, Last : Positive; Style : Token_Style));
   --  Drawn after the line's text, in a box ("" for none); Marked: in the
   --  change colour.
   with function Annotation (M : Model; N : Positive) return String;
   with function Annotation_Marked (M : Model; N : Positive) return Boolean;
package CuBit.UI.Code_Views.Lines is
   procedure Draw (C : Canvas; Area : Rect; M : Model; V : View; Colors : CuBit.UI.Instrument.Palette);
end CuBit.UI.Code_Views.Lines;
