------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ownership of a driver's DMA descriptors (e.g. virtio-net's transmit
--  descriptors): each is either free, or in flight with the device.
--
--  The device returns descriptor ids through its used ring, so a returned
--  id is untrusted. Give_Back accepts one only if it names a descriptor
--  that is in flight; anything else (out of range, already free, returned
--  twice) changes nothing. So a faulty or hostile device cannot make the
--  driver hand one buffer out twice or overrun its free list.
--
--  Proved (tests/net-tcp): every index stays in range; Take hands out only
--  a free descriptor and marks it in flight; Give_Back frees exactly an
--  in-flight descriptor and refuses everything else; the free list never
--  holds a descriptor twice nor one that is in flight.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

generic
   Count : Positive;
package Descriptor_Pool with SPARK_Mode is

   subtype Id is Natural range 0 .. Count - 1;
   subtype Free_Count is Natural range 0 .. Count;

   type Flags is array (Id) of Boolean;
   type Stack is array (Id) of Id;

   type Pool is record
      In_Flight : Flags := [others => False];
      Free      : Stack := [others => 0];
      Top       : Free_Count := 0;   --  Free (0 .. Top - 1) are free
   end record;

   --  The free list holds distinct descriptors, none of them in flight.
   function Consistent (P : Pool) return Boolean is
     ((for all K in 0 .. P.Top - 1 => not P.In_Flight (P.Free (K))) and then
      (for all I in 0 .. P.Top - 1 =>
         (for all J in 0 .. P.Top - 1 =>
            (if I /= J then P.Free (I) /= P.Free (J)))));

   --  Every descriptor free.
   procedure Initialize (P : out Pool)
   with Post => Consistent (P) and then P.Top = Count and then
                (for all D in Id => not P.In_Flight (D));

   --  A free descriptor for the device, if any.
   procedure Take (P : in out Pool; D : out Id; OK : out Boolean)
   with
     Pre  => Consistent (P),
     Post => Consistent (P) and then
             OK = (P'Old.Top > 0) and then
             (if OK then
                not P'Old.In_Flight (D) and then P.In_Flight (D) and then
                P.Top = P'Old.Top - 1 and then
                (for all E in Id => (if E /= D then P.In_Flight (E) = P'Old.In_Flight (E)))
              else P = P'Old);

   --  The device returned Raw: free it if it names one of ours in flight.
   procedure Give_Back (P : in out Pool; Raw : Unsigned_32; OK : out Boolean)
   with
     Pre  => Consistent (P),
     Post => Consistent (P) and then
             OK = (Raw < Unsigned_32 (Count) and then
                   P'Old.In_Flight (Natural (Raw)) and then P'Old.Top < Count) and then
             (if OK then
                not P.In_Flight (Natural (Raw)) and then P.Top = P'Old.Top + 1 and then
                (for all E in Id =>
                   (if E /= Natural (Raw) then P.In_Flight (E) = P'Old.In_Flight (E)))
              else P = P'Old);

end Descriptor_Pool;
