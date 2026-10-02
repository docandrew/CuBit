--  A 4-slot ring of 32-bit values: wrap-around and hostile indices are
--  cheap to exercise, and the proof covers the same generic.
pragma SPARK_Mode;
with Interfaces;
with CuBit.Slot_Rings;
package Slot_Ring_Small is new CuBit.Slot_Rings
  (Element => Interfaces.Unsigned_32, Slot_Bits => 2);
