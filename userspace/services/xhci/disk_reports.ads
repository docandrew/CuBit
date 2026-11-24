------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  What xhci.drv still has to tell devmgr about its disks (FS-034): per
--  device slot, the disk there and whether devmgr knows of it, and the
--  disk that left it unannounced.
--
--  @description
--  Reports are state, delivered in order and never dropped
--  (CuBit.Disk_Presence): the driver asks Next for the report a slot owes,
--  submits it, and calls Sent only once the kernel took it into devmgr's
--  queue; until then Next keeps answering the same report.
--
--  Proved per slot:
--  - devmgr hears Gone for every unit it heard Present for, and before it
--    hears Present for the next disk in that slot;
--  - devmgr never hears Gone for a unit it was never told about (a disk
--    that came and went while devmgr's queue was full is not reported);
--  - at most one Gone waits per slot (Pending_Gone), so the state is
--    bounded by the slot count (the controller's own limit).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Block_Unit_Badges; use CuBit.Block_Unit_Badges;
with CuBit.Disk_Presence;

generic
   type Slot is range <>;
package Disk_Reports with SPARK_Mode is

   package DP renames CuBit.Disk_Presence;
   use type DP.Presence;

   type Slot_State is record
      Current : Maybe_Unit := No_Unit;   --  the disk served in the slot now
      Write_Protected : Boolean := False;
      Announced : Boolean := False;      --  devmgr was told Current is present
      Pending_Gone : Maybe_Unit := No_Unit;  --  told present, gone, Gone not yet sent
   end record;

   function Consistent (S : Slot_State) return Boolean is
     ((if S.Announced then S.Current /= No_Unit and then S.Pending_Gone = No_Unit)
      and then (S.Current = No_Unit or else S.Current /= S.Pending_Gone));

   type State is array (Slot) of Slot_State;

   function Valid (Item : State) return Boolean is
     (for all S in Slot => Consistent (Item (S)));

   function Has_Disk (Item : State; S : Slot) return Boolean is (Item (S).Current /= No_Unit);

   --  A disk is ready in S (S had none).
   procedure Appear (Item : in out State; S : Slot; U : Unit; Write_Protected : Boolean)
     with Pre  => Valid (Item) and then Item (S).Current = No_Unit
                  and then Item (S).Pending_Gone /= U,
          Post => Valid (Item) and then Item (S).Current = U and then not Item (S).Announced
                  and then Item (S).Pending_Gone = Item'Old (S).Pending_Gone
                  and then (for all O in Slot => (if O /= S then Item (O) = Item'Old (O)));

   --  The disk in S is gone (nothing if there was none).
   procedure Vanish (Item : in out State; S : Slot)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then Item (S).Current = No_Unit and then not Item (S).Announced
                  --  an announced disk now owes its Gone
                  and then (if Item'Old (S).Announced
                            then Item (S).Pending_Gone = Item'Old (S).Current
                            else Item (S).Pending_Gone = Item'Old (S).Pending_Gone)
                  and then (for all O in Slot => (if O /= S then Item (O) = Item'Old (O)));

   --  The report S owes devmgr, if any: a pending Gone first.
   procedure Next (Item : State; S : Slot; Due : out Boolean; Report : out DP.Report)
     with Pre  => Valid (Item),
          Post => DP.Well_Formed (Report)
                  and then Due = (Item (S).Pending_Gone /= No_Unit
                                  or else (Item (S).Current /= No_Unit and then not Item (S).Announced))
                  and then (if Item (S).Pending_Gone /= No_Unit
                            then Report.State = DP.Gone and then Report.Unit = Item (S).Pending_Gone)
                  and then (if Due and then Item (S).Pending_Gone = No_Unit
                            then Report.State = DP.Present and then Report.Unit = Item (S).Current);

   --  Next's report for S reached devmgr's queue.
   procedure Sent (Item : in out State; S : Slot)
     with Pre  => Valid (Item),
          Post => Valid (Item)
                  and then (if Item'Old (S).Pending_Gone /= No_Unit
                            then Item (S).Pending_Gone = No_Unit
                                 and then Item (S).Current = Item'Old (S).Current
                                 and then Item (S).Announced = Item'Old (S).Announced
                            elsif Item'Old (S).Current /= No_Unit
                            then Item (S).Announced and then Item (S).Current = Item'Old (S).Current
                            else Item (S) = Item'Old (S))
                  and then (for all O in Slot => (if O /= S then Item (O) = Item'Old (O)));

end Disk_Reports;
