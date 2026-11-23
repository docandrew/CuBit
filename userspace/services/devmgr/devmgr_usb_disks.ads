------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  devmgr's USB disks (FS-034): which xhci.drv unit is which @usb:N
--  volume, and what devmgr still owes the filesystem about each.
--
--  @description
--  xhci.drv reports each disk that comes or goes (CuBit.Disk_Presence).
--  devmgr gives a new disk the lowest free name (@usb:0 first), attaches
--  it to the filesystem with a copy of the driver's endpoint badged with
--  the unit (CuBit.Block_Unit_Badges), and detaches it when it goes. The
--  calls to the filesystem carry a deadline: one that times out is tried
--  again later, never dropped, so devmgr's loop never waits long on a
--  filesystem that is busy with a slow stick.
--
--  Proved:
--  - a name is held by at most one (driver, unit) at a time;
--  - a name is given again only once the filesystem acknowledged the
--    detach of the disk that held it (Free);
--  - every disk the filesystem may have been told about (an attach that
--    was answered, or one that timed out: Maybe_Known) is detached when it
--    goes or its driver ends.
--  The table's size is the protocol's (Fs_Attachments.Device_Ordinal: 256
--  names); past it a disk is refused with a CAPACITY reason.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Block_Unit_Badges;
with Fs_Attachments;

package Devmgr_USB_Disks with SPARK_Mode is

   package Units renames CuBit.Block_Unit_Badges;

   Maximum_Disks : constant := Fs_Attachments.Maximum_Ordinal + 1;
   subtype Ordinal is Fs_Attachments.Device_Ordinal;

   --  Calls to the filesystem wait this long at most, and one that timed
   --  out (or a filesystem not there yet) is tried again this much later.
   Call_Deadline_Ms : constant := 250;
   Retry_Ms : constant := 250;

   type Phase is
     (Free,          --  the name is unused
      Attach_Owed,   --  a disk is here; the filesystem has not been told
      Attached,      --  the filesystem took it
      Refused,       --  the filesystem refused it: nothing to detach
      Detach_Owed);  --  gone; the filesystem must forget it before the name is reused

   --  Which driver instance (devmgr's inventory entry, never 0) reported.
   subtype Driver_Index is Positive;

   type Disk is record
      State : Phase := Free;
      Driver : Natural := 0;
      Unit : Units.Maybe_Unit := Units.No_Unit;
      Write_Protected : Boolean := False;
      Retry_At : Unsigned_64 := 0;
      --  An attach call went unanswered: the filesystem may have taken it.
      Told : Boolean := False;
   end record;

   --  The filesystem may know the disk: it must be detached when it goes.
   function Maybe_Known (D : Disk) return Boolean is
     (D.State in Attached | Detach_Owed or else (D.State = Attach_Owed and then D.Told));

   type Table is array (Ordinal) of Disk;

   function Holds (D : Disk; Driver : Driver_Index; Unit : Units.Unit) return Boolean is
     (D.State /= Free and then D.Driver = Driver and then D.Unit = Unit);

   --  Every used name has a driver and a unit, and no two names hold the
   --  same (driver, unit).
   function Valid (T : Table) return Boolean is
     ((for all O in Ordinal =>
         (if T (O).State /= Free then T (O).Driver /= 0 and then T (O).Unit /= Units.No_Unit))
      and then
      (for all A in Ordinal =>
         (for all B in Ordinal =>
            (if A /= B and then T (A).State /= Free and then T (B).State /= Free
             then T (A).Driver /= T (B).Driver or else T (A).Unit /= T (B).Unit))));

   function Find (T : Table; Driver : Driver_Index; Unit : Units.Unit) return Natural
     with Post => (if Find'Result /= 0 then Find'Result - 1 in Ordinal
                                       and then Holds (T (Find'Result - 1), Driver, Unit))
                  and then (if Find'Result = 0 then (for all O in Ordinal => not Holds (T (O), Driver, Unit)));

   type Placement is (Placed, Already_Known, No_Free_Name);

   --  A disk is present: the lowest free name, Attach_Owed.
   procedure Note_Present
     (T : in out Table; Driver : Driver_Index; Unit : Units.Unit; Write_Protected : Boolean;
      Result : out Placement; Name : out Ordinal)
     with Pre  => Valid (T),
          Post => Valid (T)
                  and then (if Result = Placed
                            then T'Old (Name).State = Free and then T (Name).State = Attach_Owed
                                 and then Holds (T (Name), Driver, Unit)
                                 and then (for all O in Ordinal => (if O /= Name then T (O) = T'Old (O)))
                            else T = T'Old)
                  and then (Result = Already_Known) = (for some O in Ordinal => Holds (T'Old (O), Driver, Unit));

   --  A disk is gone: Detach_Owed if the filesystem may know it, else Free.
   procedure Note_Gone
     (T : in out Table; Driver : Driver_Index; Unit : Units.Unit; Found : out Boolean; Name : out Ordinal)
     with Pre  => Valid (T),
          Post => Valid (T)
                  and then Found = (for some O in Ordinal => Holds (T'Old (O), Driver, Unit))
                  and then (if Found
                            then (if Maybe_Known (T'Old (Name))
                                  then T (Name).State = Detach_Owed
                                  else T (Name).State = Free)
                                 and then (for all O in Ordinal => (if O /= Name then T (O) = T'Old (O)))
                            else T = T'Old);

   --  A driver ended: each of its disks as if gone.
   procedure Driver_Ended (T : in out Table; Driver : Driver_Index)
     with Pre  => Valid (T),
          Post => Valid (T)
                  and then (for all O in Ordinal =>
                              (if T'Old (O).State /= Free and then T'Old (O).Driver = Driver
                               then (if Maybe_Known (T'Old (O))
                                     then T (O).State = Detach_Owed else T (O).State = Free)
                               else T (O) = T'Old (O)));

   --  The first name with a call owed and due at Now, or Found False.
   procedure Next_Due (T : Table; Now : Unsigned_64; Found : out Boolean; Name : out Ordinal)
     with Post => (if Found then T (Name).State in Attach_Owed | Detach_Owed
                                 and then T (Name).Retry_At <= Now);

   --  The earliest time a call is owed (Unsigned_64'Last: none).
   function Next_Wake (T : Table) return Unsigned_64;

   type Call_Outcome is (Answered_OK, Answered_Refused, Not_Answered);

   procedure Attach_Done (T : in out Table; Name : Ordinal; Outcome : Call_Outcome; Now : Unsigned_64)
     with Pre  => Valid (T) and then T (Name).State = Attach_Owed,
          Post => Valid (T)
                  --  An unanswered attach is tried again (the filesystem
                  --  answers a repeated attach of an attached name OK).
                  and then T (Name).State = (case Outcome is
                                               when Answered_OK => Attached,
                                               when Not_Answered => Attach_Owed,
                                               when Answered_Refused => Refused)
                  and then (if Outcome = Not_Answered then T (Name).Told)
                  and then (for all O in Ordinal => (if O /= Name then T (O) = T'Old (O)));

   --  Only an acknowledged detach frees the name; otherwise it is tried
   --  again from Now + Retry_Ms.
   procedure Detach_Done (T : in out Table; Name : Ordinal; Outcome : Call_Outcome; Now : Unsigned_64)
     with Pre  => Valid (T) and then T (Name).State = Detach_Owed,
          Post => Valid (T)
                  and then (if Outcome = Answered_OK then T (Name).State = Free
                            else T (Name).State = Detach_Owed)
                  and then (for all O in Ordinal => (if O /= Name then T (O) = T'Old (O)));

   --  The filesystem was not there to call: try again from Now + Retry_Ms.
   procedure Postpone (T : in out Table; Name : Ordinal; Now : Unsigned_64)
     with Pre  => Valid (T),
          Post => Valid (T) and then T (Name).State = T'Old (Name).State
                  and then (for all O in Ordinal => (if O /= Name then T (O) = T'Old (O)));

end Devmgr_USB_Disks;
