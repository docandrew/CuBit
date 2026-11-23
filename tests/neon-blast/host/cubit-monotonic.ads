------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Linux-hosted stand-in for CuBit.Monotonic (tests/neon-blast): the host's
--  monotonic clock through Ada.Real_Time. Not the CuBit clock.
------------------------------------------------------------------------------
with Interfaces;
package CuBit.Monotonic is
   function Microseconds return Interfaces.Unsigned_64;
   function Milliseconds return Interfaces.Unsigned_64;
end CuBit.Monotonic;
