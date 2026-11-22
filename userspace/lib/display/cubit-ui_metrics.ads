------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The UI's one size source: text and widget metrics derived from the em.
--
--  @description
--  Everything here is in logical pixels at 1x. The base UI text size is one
--  value, Base_Em; the line box comes from the bundled faces' own design
--  metrics (IBM Plex Sans, Sans SemiBold and Mono share them), and every
--  widget height, padding, gap and icon box is derived from those two, each
--  rounded to a whole logical pixel. A density (5/4, 3/2, 2x) draws Base_Em
--  * Numerator / Denominator physical pixels, so at 1x text and widgets stay
--  pixel-exact (integer scale) and every other density scales the same
--  logical layout. Hairlines (borders, rules, bevels) are one pixel at every
--  em and are not here.
--
--  Users: CuBit.Fonts (line, mono grid), Compositor_Glyph_Layout and the
--  font engine (userspace/rust/fonts: BASE_EM, BASE_LINE, which a Rust test
--  checks against the font), the toolkit's widgets, the window chrome and
--  the Desktop (Deskbar, status panel, menus).
------------------------------------------------------------------------------
package CuBit.UI_Metrics with SPARK_Mode, Pure is
   --  The faces' design metrics (hhea; font units). Descender is the
   --  magnitude below the baseline.
   Units_Per_Em       : constant := 1_000;
   Font_Ascender      : constant := 1_025;
   Font_Descender     : constant := 275;
   Font_Line_Gap      : constant := 0;
   --  IBM Plex Mono: every character's advance.
   Mono_Advance_Units : constant := 600;
   --  The widest printable ASCII advance of the Sans faces (SemiBold '%').
   Widest_Advance_Units : constant := 960;

   --  The base UI text size: the em of every UI face at 1x.
   Base_Em : constant := 16;

   --  Font units at Base_Em, rounded to the nearest logical pixel.
   Rounding : constant := Units_Per_Em / 2;

   --  The line box: ascent (the baseline's row) plus descent plus gap.
   Line_Height : constant :=
     ((Font_Ascender + Font_Descender + Font_Line_Gap) * Base_Em + Rounding) / Units_Per_Em;
   Ascent      : constant := (Font_Ascender * Base_Em + Rounding) / Units_Per_Em;
   Descent     : constant := Line_Height - Ascent;
   --  The monospace grid: one advance per character, a whole pixel so code
   --  stays a fixed grid (Plex Mono's 0.6 em, rounded).
   Mono_Advance : constant := (Mono_Advance_Units * Base_Em + Rounding) / Units_Per_Em;
   --  The widest printable glyph's advance, rounded up.
   Widest_Advance : constant := (Widest_Advance_Units * Base_Em + Units_Per_Em - 1) / Units_Per_Em;

   --  The font engine's glyph cells (CuBit.Fonts.Coverage, userspace/rust/
   --  fonts WIDTH and HEIGHT): the double-size raster's line plus a margin,
   --  and its widest glyph past the pen origin, in whole 8-pixel blocks.
   Glyph_Origin       : constant := 2;
   Glyph_Cell_Rows    : constant := 2 * Line_Height + 2;
   Cell_Block         : constant := 8;
   Glyph_Cell_Columns : constant :=
     ((2 * (Widest_Advance + Glyph_Origin) + Cell_Block - 1) / Cell_Block) * Cell_Block;
   --  One logical-size raster's width (density masks: this many logical
   --  pixels at every density): the widest glyph and its origin, twice.
   Glyph_Width_Bound  : constant := 2 * Base_Em;

   --  Spacing steps, as fractions of the em (Step_Denominator-ths),
   --  rounded to whole pixels.
   Step_Denominator : constant := 8;
   Space_XS : constant := (1 * Base_Em + Step_Denominator / 2) / Step_Denominator;  --  1/8 em
   Space_S  : constant := (2 * Base_Em + Step_Denominator / 2) / Step_Denominator;  --  1/4 em
   Space_M  : constant := (4 * Base_Em + Step_Denominator / 2) / Step_Denominator;  --  1/2 em
   Space_L  : constant := (6 * Base_Em + Step_Denominator / 2) / Step_Denominator;  --  3/4 em
   Space_XL : constant := Base_Em;                                                   --  1 em

   --  Icon boxes: icons are drawn 1:1 from their exported edges (Desktop:
   --  16, 24, 32, 48, 64; the toolkit's stock set: 16 and 32), never scaled
   --  by a fraction, so a box takes the largest export it holds. A small
   --  icon sits in a line (menus, lists, toolbars, the title tile; stock
   --  set); a large one in a control less its padding (the Deskbar).
   Small_Icon_Edge : constant := (if Line_Height >= 32 then 32 else 16);
   Large_Icon_Box  : constant := Line_Height + Space_S;
   Large_Icon_Edge : constant :=
     (if Large_Icon_Box >= 32 then 32 elsif Large_Icon_Box >= 24 then 24 else 16);

   --  Text-bearing boxes. A control (button, field, combo box) and a
   --  menu, drawer or list row put Control_Padding above and below a line.
   Control_Padding    : constant := Space_S;
   Control_Height     : constant := Line_Height + 2 * Control_Padding;
   Row_Height         : constant := Control_Height;
   --  Compact rows: tables, trees, list views, status and path bars.
   Compact_Row_Height : constant := Line_Height + Space_S;
   --  Dense rows: long scanned lists (file panes, log records): a line
   --  and Space_XS.
   Dense_Row_Height   : constant := Line_Height + Space_XS;
   --  A section heading row (drawers, panels).
   Section_Height     : constant := Row_Height + Space_XS;
   --  Text left of a row's or control's content.
   Text_Inset         : constant := Space_M;
   --  A popup menu's separator: a rule with Space_S on each side.
   Separator_Height   : constant := 2 * Space_S + 1;
   --  Vertical scroll bars, check boxes and the like: square controls a
   --  little smaller than a compact row.
   Glyph_Box          : constant := Line_Height - Space_XS;
   --  Tree indentation per level and a table's header row.
   Indent             : constant := Base_Em + Space_XS;
   Header_Height      : constant := Compact_Row_Height;
   --  A tooltip: one line with Space_S + Space_XS around it.
   Tooltip_Padding    : constant := Space_S + Space_XS;

   --  The window chrome (CuBit.UI.Window_Chrome): the title bar holds one
   --  line of the emphasis face and the window buttons with Space_S
   --  around them.
   Title_Bar_Height    : constant := Control_Height + Space_XS;
   Title_Button_Height : constant := Line_Height + Space_XS;
   Title_Button_Width  : constant := Title_Button_Height + Space_XS;
   Title_Glyph         : constant := Space_L;
   --  A client menu bar: one line, Control_Padding above and below.
   Menu_Bar_Height     : constant := Control_Height;
end CuBit.UI_Metrics;
