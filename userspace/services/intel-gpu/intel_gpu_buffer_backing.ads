with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents;
package Intel_GPU_Buffer_Backing with SPARK_Mode is
   -- Supervisor/driver-only bootstrap arena, not public Mesa handles.
   Request_Label : constant := 16#0236#;
   Extent_Request_Label : constant := 16#0237#;
   subtype Slot is Positive range 1 .. 16;
   subtype Page_Count is Positive range 1 .. 4096;
   -- Contiguous CPU arena backed by independently allocated physical extents.
   Capacity : constant Unsigned_64 := 32 * 1024 * 1024;
   -- Same per-process DMA aperture convention used by other native drivers.
   -- Intel's firmware/context windows are separate; these are CPU, not GPU VA.
   CPU_Base : constant Unsigned_64 := 16#0000_7000_0000_0000#;
   function Valid_Physical (Physical : Unsigned_64) return Boolean is
     (Physical /= 0 and then Physical mod 4096 = 0 and then
      Physical <= 2 ** 32 - Capacity);
   pragma Compile_Time_Error (Capacity /= Intel_GPU_Physical_Extents.Capacity,
                              "buffer arena/extent capacity disagree");
end Intel_GPU_Buffer_Backing;
