------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The status area's load bars (UI-027, user 2026-11-05): CPU busy over
--  all CPUs and memory in use in the status button, and one bar per CPU
--  in the Deskbar panel's System section.
--
--  @description
--  Both come from the kernel's per-CPU times publication (CuBit.CPU_Times,
--  granted by Desktop's manifest, (request-publication cpu-times)): busy
--  ticks over all ticks between two readings (CuBit.CPU_Load), and the free
--  memory the kernel refreshes in the same page. Reading it is a few loads,
--  no system call and no peer; it has no change event, so Desktop reads it
--  every Refresh_Interval_Ms and draws again only when a bar's filled
--  pixels (or the panel's figures) changed.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.UI_Metrics;

package Desktop_Load_Bars with SPARK_Mode, Pure is
   package M renames CuBit.UI_Metrics;
   subtype Percent is Natural range 0 .. 100;

   --  How often the bars are read (no event source exists).
   Refresh_Interval_Ms : constant := 1_000;

   --  Memory in use of Total bytes with Free free (0 when Total is 0 or the
   --  figures disagree).
   function Used_Percent (Total, Free : Unsigned_64) return Percent
   with Post => (if Total = 0 or else Free > Total then Used_Percent'Result = 0);

   --  The filled part of a bar Height pixels tall: any use shows at least
   --  one pixel, a full bar all of it.
   function Filled (Height : Natural; Used : Percent) return Natural
   with Pre => Height <= 10_000,
        Post => Filled'Result <= Height and then (if Used = 0 then Filled'Result = 0)
                and then (if Used = 100 then Filled'Result = Height)
                and then (if Used > 0 and then Height > 0 then Filled'Result >= 1);

   --  The bars' geometry in the status button (CPU, then memory): a line
   --  high, from the UI em (CuBit.UI_Metrics).
   Bar_Width  : constant := M.Space_S + M.Space_XS;
   Bar_Height : constant := M.Line_Height;
   Bar_Gap    : constant := M.Space_XS;

   --  Over this the bar shows its warning colour.
   High_Percent : constant := 90;

   --  The panel's System section: one vertical bar per CPU, and a
   --  horizontal memory bar.
   CPU_Bar_Width   : constant := M.Space_M;
   CPU_Bar_Height  : constant := M.Control_Height;
   CPU_Bar_Gap     : constant := M.Space_XS + 1;
   Memory_Bar_Width  : constant := 12 * M.Base_Em;
   Memory_Bar_Height : constant := M.Space_M + M.Space_XS;
end Desktop_Load_Bars;
