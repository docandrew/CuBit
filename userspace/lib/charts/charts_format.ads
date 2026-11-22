------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Chart labels (MET-002's app): values in their unit, scaled to read at a
--  glance (250 us, 1.25 ms, 3.2 s, 12 KiB, 4.1k), and clock times since
--  boot at the precision a tick step needs. The bundled faces draw ASCII
--  only, so microseconds are "us".
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Charts_Format with SPARK_Mode, Pure is
   type Unit is (Count, Microseconds, Nanoseconds, Bytes, Pixels);

   --  V in U: at most four significant digits.
   function Value_Text (V : Unsigned_64; U : Unit) return String;

   --  An axis label: V in the scale Top needs (every label of one axis in
   --  one unit: "0", "500 us", "1 ms", "1.5 ms" when Top is 2 ms, not
   --  "500 us" beside "1 ms"), zero as "0".
   function Axis_Text (V, Top : Unsigned_64; U : Unit) return String;

   --  T (microseconds since boot) as h:mm:ss, mm:ss, or with milliseconds
   --  when Step (the tick spacing) is under a second.
   function Clock_Text (T : Unsigned_64; Step : Unsigned_64) return String;

   --  A span: "5 min", "30 s", "1 h", "250 ms".
   function Span_Text (Span_Us : Unsigned_64) return String;

   --  The decimal digits of V.
   function Decimal (V : Unsigned_64) return String;
end Charts_Format;
