-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- A frame a process owns, as the kernel holds it (KERN-014,
-- docs/kernel-memory-map.md): its physical base, with no kernel address.
--
-- @description
-- Process frames leave the kernel's linear map (KERN-014 slice 4), so the
-- kernel must not reach their contents by an address it computes. This
-- type carries no address and has no conversion to one; the kernel writes
-- into a process frame only through its per-CPU window (Kernel_Window).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Process_Frames with SPARK_Mode, Pure is
   Frame_Bytes : constant := 4096;

   type Process_Frame is private;
   No_Frame : constant Process_Frame;

   function Make (Physical : Unsigned_64) return Process_Frame with
     Pre  => Physical mod Frame_Bytes = 0,
     Post => Base (Make'Result) = Physical;

   function Base (Item : Process_Frame) return Unsigned_64;
   function Is_Frame (Item : Process_Frame) return Boolean is (Base (Item) /= 0);
private
   type Process_Frame is record
      Physical : Unsigned_64 := 0;
   end record;
   No_Frame : constant Process_Frame := (Physical => 0);
   function Make (Physical : Unsigned_64) return Process_Frame is
     ((Physical => Physical));
   function Base (Item : Process_Frame) return Unsigned_64 is (Item.Physical);
end Process_Frames;
