------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  A small instance for the hosted tests: a ceiling of 40 makes every
--  capacity boundary (first block, doublings, the ceiling) cheap to reach.
------------------------------------------------------------------------------
with CCL.Growing_Tables;
package Integer_Tables is new CCL.Growing_Tables (Element => Integer, Ceiling => 40);
