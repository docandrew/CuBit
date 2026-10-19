------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The bound every per-window structure in Desktop shares.
--
--  @description
--  Desktop's surface table starts small and grows with use from the
--  process heap (CuAlloc), so memory is what limits the number of windows.
--  The only fixed bound is the Desktop protocol's surface capacity type
--  (CuBit.Desktop_Protocol.Surface_Capacity, which Hello reports): the
--  table, the task buttons, their layout and the tooltip targets all
--  range up to it. Desktop checks at compile time that the two agree.
------------------------------------------------------------------------------
package Desktop_Surface_Limits with SPARK_Mode, Pure is
   Maximum_Surfaces : constant := 65_535;
end Desktop_Surface_Limits;
