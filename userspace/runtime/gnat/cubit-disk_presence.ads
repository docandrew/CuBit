------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A driver's report to devmgr that one of its disks came or went (FS-034:
--  USB sticks plugged in and pulled out after boot).
--
--  @description
--  devmgr calls its drivers; a driver never calls devmgr back for this
--  (the synchronous IPC graph stays a DAG). So the report goes up
--  asynchronously: the driver submits it (capSubmit, no completion) to
--  devmgr's endpoint and keeps it until the kernel takes it into
--  devmgr's queue; a full queue only delays it, nothing is dropped
--  (xhci's Disk_Reports keeps the order per device). The kernel-stamped
--  sender says which driver instance reports; devmgr takes the report
--  only from a driver it launched for disks of this kind.
--
--  Words: 0 = the unit (CuBit.Block_Unit_Badges.Unit, the driver's own
--  name for the disk, never reused while it runs); 1 = the state (bits
--  0..7: Present or Gone) and, for Present, bit 8 = write protected. Every
--  other bit is zero. A report devmgr cannot decode exactly is refused.
--
--  This package is the pure wire encoding.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Block_Unit_Badges;

package CuBit.Disk_Presence with Pure, SPARK_Mode is

   package Units renames CuBit.Block_Unit_Badges;

   Report_Label : constant Unsigned_32 := 16#0245#;
   Report_Words : constant := 2;
   Unit_Word : constant := 0;
   State_Word : constant := 1;

   type Presence is (Present, Gone);
   for Presence use (Present => 1, Gone => 2);

   Write_Protected_Bit : constant Unsigned_64 := 16#100#;

   type Report is record
      Unit : Units.Unit := Units.Unit'First;
      State : Presence := Gone;
      --  Only for Present (a Gone report carries none).
      Write_Protected : Boolean := False;
   end record;

   --  A report that can be sent: Write_Protected only with Present.
   function Well_Formed (Item : Report) return Boolean is
     (Item.State = Present or else not Item.Write_Protected);

   type Words is array (Unit_Word .. State_Word) of Unsigned_64;

   function Encode (Item : Report) return Words is
     ([Unit_Word  => Unsigned_64 (Item.Unit),
       State_Word => Unsigned_64 (Presence'Enum_Rep (Item.State))
                     + (if Item.Write_Protected then Write_Protected_Bit else 0)])
     with Pre => Well_Formed (Item);

   --  Valid: Length is Report_Words, word 0 names a unit, word 1 a state
   --  and nothing else (write protection only with Present); then Item
   --  encodes back to exactly these words.
   procedure Decode
     (Length : Natural; Item_Words : Words; Item : out Report; Valid : out Boolean)
     with Post =>
       (if Valid then Well_Formed (Item)
                      and then Encode (Item) (Unit_Word) = Item_Words (Unit_Word)
                      and then Encode (Item) (State_Word) = Item_Words (State_Word))
       and then Valid =
         (Length = Report_Words
          and then Item_Words (Unit_Word) in 1 .. Unsigned_64 (Units.Maximum_Unit)
          and then (Item_Words (State_Word) = Unsigned_64 (Presence'Enum_Rep (Present))
                    or else Item_Words (State_Word) =
                      Unsigned_64 (Presence'Enum_Rep (Present)) + Write_Protected_Bit
                    or else Item_Words (State_Word) = Unsigned_64 (Presence'Enum_Rep (Gone))));

end CuBit.Disk_Presence;
