------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The block write grant for one unit of a driver that serves several
--  disks on one endpoint (FS-034: xhci.drv and its USB sticks).
--
--  @description
--  A badge is the tag devmgr mints on a copy of a driver's endpoint
--  (CuBit.Handles.Badged); the kernel stamps it on every request that
--  comes through that copy (authorityTag) and lets a badge be set only
--  once, on an untagged endpoint held with Grant. A unit badge is that
--  tag carrying the unit it grants: ASCII "BLKU" in the high half, the
--  unit in the low half (never 0). The driver chooses its units and never
--  reuses one while it runs, so a badge minted for a stick that has gone
--  names nothing on the stick that takes its place.
--
--  Holding a unit badge is the write grant for that unit and for nothing
--  else on the endpoint: the driver routes each request by the badge, and
--  a request without one reaches only what an unbadged copy may reach
--  (xhci: the read-only boot medium). The boot medium is never given a
--  unit, so no badge can name it for writing.
--
--  Unit badges are a block authority of their own; they never equal the
--  whole-device grant (CuBit.Block_Devices.Write_Authority_Tag, "BLKWRITE")
--  or the untagged 0.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Block_Unit_Badges with Pure, SPARK_Mode is

   --  ASCII "BLKU", the badge's high half.
   Unit_Prefix : constant Unsigned_64 := 16#424C_4B55#;
   Prefix_Shift : constant := 32;
   --  The high half of CuBit.Block_Devices.Write_Authority_Tag ("BLKW"):
   --  a different prefix, so no unit badge is the whole-device grant.
   Whole_Device_Prefix : constant Unsigned_64 := 16#424C_4B57#;

   Maximum_Unit : constant := Unsigned_32'Last;
   subtype Maybe_Unit is Unsigned_32;
   No_Unit : constant Maybe_Unit := 0;
   subtype Unit is Maybe_Unit range 1 .. Maximum_Unit;

   Low_Half : constant Unsigned_64 := 16#FFFF_FFFF#;

   function Prefix_Of (Tag : Unsigned_64) return Unsigned_64 is (Shift_Right (Tag, Prefix_Shift));

   --  The unit a tag names, or No_Unit for any other tag.
   function Unit_Of (Tag : Unsigned_64) return Maybe_Unit is
     (if Prefix_Of (Tag) = Unit_Prefix then Maybe_Unit (Tag and Low_Half) else No_Unit)
     with Post =>
       (if Unit_Of'Result /= No_Unit
        then Prefix_Of (Tag) = Unit_Prefix
             and then Tag = (Shift_Left (Unit_Prefix, Prefix_Shift) or Unsigned_64 (Unit_Of'Result)))
       and then (if Prefix_Of (Tag) /= Unit_Prefix then Unit_Of'Result = No_Unit);

   --  The badge that grants Item.
   function Badge (Item : Unit) return Unsigned_64 is
     (Shift_Left (Unit_Prefix, Prefix_Shift) or Unsigned_64 (Item))
     with Post =>
       Unit_Of (Badge'Result) = Item
       and then Badge'Result /= 0
       and then Prefix_Of (Badge'Result) = Unit_Prefix
       and then Prefix_Of (Badge'Result) /= Whole_Device_Prefix;

end CuBit.Block_Unit_Badges;
