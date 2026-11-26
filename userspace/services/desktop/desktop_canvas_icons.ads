------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  An icon of Desktop's icon store drawn into a toolkit canvas at the
--  Deskbar's icon size, blended over a known background: the Deskbar
--  panel's and the hover card's layers (Desktop_Status_Panel,
--  Desktop_Task_Card) draw their icons with it.
------------------------------------------------------------------------------
with CuBit.UI;
with Desktop_Icons;

package Desktop_Canvas_Icons is
   procedure Draw (C : CuBit.UI.Canvas; Id : Desktop_Icons.Icon_ID; X, Y : Natural; Bg : CuBit.UI.Color);
end Desktop_Canvas_Icons;
