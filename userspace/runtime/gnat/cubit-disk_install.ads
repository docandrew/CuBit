------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The installer's authority over whole disks (SYS-001, HW-001 step 4).
--
--  @description
--  Service role disk-install is devmgr's endpoint badged with
--  Authority_Tag. procmgr mints it only for a manifest request the trusted
--  startup plan approves (CuBit.Authority_Policy.Disk_Installation), so
--  the installer holds it and nothing else does.
--
--  Over it, Open_Disk asks devmgr for one disk's Block.Device.V1 endpoint
--  (a call accepting a capability). The access is named, never implied:
--  Read_Only gives the driver's untagged endpoint, Read_Write one badged
--  with CuBit.Block_Devices.Write_Authority_Tag. A request without
--  Authority_Tag, or naming no disk devmgr serves, is refused without a
--  capability.
--
--  This package is the pure wire encoding.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Disk_Install with Pure, SPARK_Mode is

   --  The catalog's disk-install role (userspace/ccl/catalogs).
   Service_Role : constant := 30;
   --  devmgr's endpoint badge for the role. ASCII "DISKINST".
   Authority_Tag : constant Unsigned_64 := 16#4449_534B_494E_5354#;

   Open_Disk : constant Unsigned_32 := 16#024A#;
   Open_Words : constant := 2;
   Ordinal_Word : constant := 0;
   Access_Word : constant := 1;

   Reply_OK    : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;

   --  A disk is named by its place among the block devices devmgr's
   --  drivers serve, in PCI location order.
   Maximum_Disks : constant := 16;
   type Disk_Ordinal is range 0 .. Maximum_Disks - 1;

   type Disk_Access is (Read_Only, Read_Write);
   for Disk_Access use (Read_Only => 1, Read_Write => 2);

   type Open_Request is record
      Disk  : Disk_Ordinal := Disk_Ordinal'First;
      Mode  : Disk_Access := Read_Only;
   end record;

   type Request_Words is array (Ordinal_Word .. Access_Word) of Unsigned_64;

   function Encode (Item : Open_Request) return Request_Words is
     ([Ordinal_Word => Unsigned_64 (Item.Disk),
       Access_Word  => Unsigned_64 (Disk_Access'Enum_Rep (Item.Mode))]);

   --  Valid: both words name a disk ordinal and an access exactly.
   procedure Decode
     (Words : Request_Words; Item : out Open_Request; Valid : out Boolean)
     with Post =>
       (if Valid then Encode (Item) = Words)
       and then Valid =
         (Words (Ordinal_Word) <= Unsigned_64 (Disk_Ordinal'Last)
          and then Words (Access_Word) in
            Unsigned_64 (Disk_Access'Enum_Rep (Read_Only)) |
            Unsigned_64 (Disk_Access'Enum_Rep (Read_Write)));

end CuBit.Disk_Install;
