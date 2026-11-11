------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The platform-register pool by handle (ACPI-POOL-001, Platform_Pool_ABI):
--  devmgr adds the register blocks it discovered; the ACPI pair maps
--  inside them, checked once by the kernel at map time.
------------------------------------------------------------------------------
pragma Ada_2022;
with Platform_Pool_ABI;

package CuBit.Handles.Platform_Pools is

   --  MAP_DEVICE through Pool (a Device_Memory tagged Platform_Pool_Tag):
   --  Pages pages at Physical (page aligned) to Virtual, uncached. 0 when
   --  mapped, else a Platform_Pool_ABI.Refusal code, with Why filled in
   --  (the claimant and its pages when Claimed).
   function Map
     (Pool : Device_Memory; Physical, Virtual, Pages : Unsigned_64;
      Read_Only : Boolean; Why : out Platform_Pool_ABI.Refusal_Record) return Unsigned_64;

   --  Size the pool (devmgr's root, with grant) for Members members and
   --  Claims claims, derived from what devmgr discovered; once, before the
   --  first member. 0, or a Refusal word.
   function Declare_Pool (Pool : Device_Memory; Members, Claims : Unsigned_64) return Unsigned_64;

   --  Add [Physical, Physical + Bytes) (whole pages) to the pool Pool
   --  (devmgr's root, with grant) as a member of Kind, on the authority of
   --  Source (an untagged device-memory root covering it, with grant). 0,
   --  or a Refusal word.
   function Insert
     (Pool, Source : Device_Memory; Physical, Bytes : Unsigned_64;
      Kind : Platform_Pool_ABI.Pool_Member_Kind) return Unsigned_64;

end CuBit.Handles.Platform_Pools;
