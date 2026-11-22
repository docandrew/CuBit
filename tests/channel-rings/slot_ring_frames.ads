--  The driver <-> netstack frame ring's shape: 128 slots of 2 KiB.
pragma SPARK_Mode;
with CuBit.Slot_Rings;
with Frame_Slots;
package Slot_Ring_Frames is new CuBit.Slot_Rings
  (Element => Frame_Slots.Frame_Slot, Slot_Bits => 7);
