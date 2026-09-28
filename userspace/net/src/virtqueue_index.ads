------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A virtqueue's used index, as the device writes it (virtio 1.x, 2.7.8):
--  a free-running 16-bit count of the entries it has returned. The driver
--  takes only as many new entries as it has handed the device and not yet
--  seen back; an index further ahead (a faulty or hostile device) yields
--  nothing, so the driver never reads stale ring entries as new ones.
--
--  Proved (tests/net-tcp): New_Entries never exceeds what the device
--  holds, and a ring position is always inside the queue.
------------------------------------------------------------------------------
package Virtqueue_Index with SPARK_Mode, Pure is

   type Index is mod 2 ** 16;

   Queue_Size : constant := 256;
   subtype Outstanding_Count is Natural range 0 .. Queue_Size;

   --  How far the device's index is past ours.
   function Distance (Last, Device : Index) return Natural is
     (Natural (Device - Last));

   --  The entries the device returned since Last, or none if it claims
   --  more than the Outstanding entries it holds.
   function New_Entries
     (Last, Device : Index; Outstanding : Outstanding_Count) return Natural
   is (if Distance (Last, Device) <= Outstanding then Distance (Last, Device) else 0)
   with Post => New_Entries'Result <= Outstanding and then
                (if New_Entries'Result > 0 then
                   New_Entries'Result = Distance (Last, Device));

   --  Where entry I lies in the ring.
   function Slot (I : Index) return Natural is (Natural (I mod Queue_Size))
   with Post => Slot'Result < Queue_Size;

end Virtqueue_Index;
