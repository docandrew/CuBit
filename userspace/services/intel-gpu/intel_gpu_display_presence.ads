with Interfaces;
with System;
with Ada.Unchecked_Conversion;
package Intel_GPU_Display_Presence with SPARK_Mode is
   use Interfaces;
   -- DFSM0x51000 pipe fields: Intel TGL Vol2c pp432-434 and Linuxv6.16
   -- intel_display_device.c runtime fuse handling. Other fields deliberately
   -- uninterpreted here: their meanings differ across display generations.
   type Bit is mod 2 with Size => 1;
   type Bits_5 is mod 2 ** 5 with Size => 5;
   type Bits_21 is mod 2 ** 21 with Size => 21;
   type Pipe_Fuses is record
      Other_0_20 : Bits_21;
      Disable_B : Bit;
      Disable_D : Bit;
      Other_23_27 : Bits_5;
      Disable_C : Bit;
      Other_29 : Bit;
      Disable_A : Bit;
      Other_31 : Bit;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Pipe_Fuses use record
      Other_0_20 at 0 range 0 .. 20;
      Disable_B at 0 range 21 .. 21;
      Disable_D at 0 range 22 .. 22;
      Other_23_27 at 0 range 23 .. 27;
      Disable_C at 0 range 28 .. 28;
      Other_29 at 0 range 29 .. 29;
      Disable_A at 0 range 30 .. 30;
      Other_31 at 0 range 31 .. 31;
   end record;
   function From_Word is new Ada.Unchecked_Conversion (Unsigned_32, Pipe_Fuses);
   type Pipe is (A, B, C, D);
   type Presence is (Unknown, Present, Absent);
   type Pipe_Set is array (Pipe) of Presence;
   type Snapshot is record
      Known : Boolean := False;
      Pipes : Pipe_Set := [others => Unknown];
   end record;
   type Held_Set is array (Pipe) of Boolean;
   -- Absent hardware needs no power lease; unknown hardware never qualifies.
   -- This checks only the pipe prerequisites, not overall GGTT ownership.
   function Required_Power_Held (Value : Snapshot; Held : Held_Set) return Boolean
     is (Value.Known and then
       (for all P in Pipe => Value.Pipes (P) = Absent or else
          (Value.Pipes (P) = Present and then Held (P))));
   -- Recognition/fuse interpretation only: NOT MMIO authorization, power,
   -- supported pixel formats, or permission to enable a native write path.
   -- Caller must supply two actual reads from the same admitted device.
   function Decode (Vendor, Device : Unsigned_16; Class : Unsigned_8;
     First, Second : Unsigned_32) return Snapshot;
end Intel_GPU_Display_Presence;
