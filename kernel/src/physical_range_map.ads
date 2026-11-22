pragma Ada_2022;
with Interfaces; use Interfaces;
with Physical_Ranges;

-------------------------------------------------------------------------------
-- Physical_Range_Map (CAP-001)
--
-- The kernel's one physical classification (Physical_Ranges), built once in
-- Modules.setup before the first user process exists and immutable after
-- that. Read without locks by MAP_DEVICE, MAP_INTO, MAPFB and
-- POLICY_MINT_CAPABILITY. Until Seal_From_Boot runs it authorizes nothing.
-------------------------------------------------------------------------------
package Physical_Range_Map with SPARK_Mode => Off is

   Kernel_Map : Physical_Ranges.Map := Physical_Ranges.Empty_Map;

   -- Firmware memory map, boot modules and framebuffer (Multiboot), the
   -- kernel image and low memory, and the kernel-driven MMIO windows
   -- (local APIC/MSI, every I/O APIC, HPET). Idempotent.
   procedure Seal_From_Boot;

   -- [Base, Base + Bytes) lies within one loader boot module's pages: the
   -- only RAM MAP_INTO may share, and only read-only.
   function Within_Boot_Module (Base, Bytes : Unsigned_64) return Boolean;

end Physical_Range_Map;
