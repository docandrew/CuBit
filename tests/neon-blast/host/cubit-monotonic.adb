------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;
package body CuBit.Monotonic is
   Epoch : constant Time := Clock;
   function Microseconds return Interfaces.Unsigned_64 is
     (Interfaces.Unsigned_64 (To_Duration (Clock - Epoch) * 1_000_000));
   function Milliseconds return Interfaces.Unsigned_64 is
     (Interfaces."/" (Microseconds, 1000));
end CuBit.Monotonic;
