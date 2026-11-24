------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Identifying a USB mass-storage logical unit as a disk CuBit may serve:
--  the command sequence and every decision from its (untrusted) answers.
--
--  @description
--  INQUIRY (a connected direct-access device), TEST UNIT READY until the
--  medium is ready (REQUEST SENSE after each refusal says whether to wait,
--  give up because no medium is present, or fail), READ CAPACITY(10), and
--  (16) when (10) overflows, then MODE SENSE(6) for the write-protect bit.
--  Last, the block holding byte 32768: a medium that carries an ISO 9660
--  volume descriptor there (a CD image, such as CuBit's own live image
--  written to a stick) is never served as a writable disk (ISO_Medium).
--
--  The xHCI adapter runs Command, sends it, and calls Advance with the
--  transport's verdict and the bytes the host received. Advance always
--  makes progress (Remaining decreases), so a probe ends after at most
--  Remaining (Start) commands whatever the device answers.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with SCSI;
with SCSI_Disks;
with USB_Bulk_Only;

package USB_Disk_Probe with SPARK_Mode is
   use type SCSI.Data_Direction;
   use type USB_Bulk_Only.Status_Kind;
   use type SCSI_Disks.Geometry;

   type Step is
     (Inquire, Test_Ready, Read_Sense, Capacity_10, Capacity_16, Mode_Sense,
      Check_ISO,
      --  Finished:
      Ready,          --  a disk to serve
      ISO_Medium,     --  an ISO 9660 medium: never a writable disk
      Not_A_Disk,     --  no connected direct-access unit
      No_Medium,      --  no medium (an empty card reader slot)
      Failed);        --  the unit did not answer as a disk should
   subtype Active is Step range Inquire .. Check_ISO;
   subtype Finished is Step range Ready .. Failed;

   --  TEST UNIT READY attempts before giving up. A stick answers the first
   --  ones with a power-on unit attention; one that is spinning up says
   --  "becoming ready". The adapter waits Ready_Retry_Ms between them.
   Ready_Attempt_Limit : constant := 20;
   Ready_Retry_Ms : constant := 100;
   subtype Attempt_Count is Natural range 0 .. Ready_Attempt_Limit;

   --  Allocation lengths (SPC-4): the standard INQUIRY data, fixed sense,
   --  and MODE SENSE's header plus pages (as Linux's sd asks).
   Inquiry_Allocation : constant := SCSI.Standard_Inquiry_Bytes;
   Sense_Allocation   : constant := SCSI.Fixed_Sense_Bytes;
   Mode_Allocation    : constant := 192;

   --  ISO 9660 (ECMA-119 6.2.1, 8.1): the volume descriptor set starts at
   --  byte 32768; a descriptor begins with its type (0 to 3, or 255),
   --  "CD001" and version 1.
   ISO_Descriptor_Offset : constant := 32_768;
   ISO_Signature_Bytes   : constant := 7;
   function ISO_Signature (Data : SCSI.Bytes) return Boolean is
     (Data'Length >= ISO_Signature_Bytes
      and then Data (Data'First) in 0 .. 3 | 255
      and then Data (Data'First + 1) = Character'Pos ('C')
      and then Data (Data'First + 2) = Character'Pos ('D')
      and then Data (Data'First + 3) = Character'Pos ('0')
      and then Data (Data'First + 4) = Character'Pos ('0')
      and then Data (Data'First + 5) = Character'Pos ('1')
      and then Data (Data'First + 6) = 1);

   --  The most bytes a probe command moves (the ISO check reads the one
   --  block holding byte 32768, at most 4096 bytes; the rest are smaller).
   Largest_Probe_Bytes : constant := 4096;

   type Probe is record
      Current   : Step := Inquire;
      Attempts  : Attempt_Count := 0;
      --  From INQUIRY (RMB): the medium is removable.
      Removable : Boolean := False;
      Identity  : SCSI.Inquiry_Data;
      --  From READ CAPACITY.
      Geometry  : SCSI_Disks.Geometry;
      --  From MODE SENSE: WP set. Mode_Unknown: the device would not say
      --  (it is then served writable; a write it refuses with DATA
      --  PROTECT makes it read only).
      Write_Protected : Boolean := False;
      Mode_Unknown    : Boolean := False;
      --  The last sense data's meaning (diagnostics).
      Sense     : SCSI_Disks.Sense_Meaning := SCSI_Disks.No_Error;
   end record;

   function Start return Probe is ((others => <>));

   --  The ISO check's block: the one holding byte 32768.
   function ISO_Block (G : SCSI_Disks.Geometry) return Unsigned_64 is
     (ISO_Descriptor_Offset / Unsigned_64 (SCSI_Disks.Bytes_Of (G.Size)));

   --  The next command.
   function Command (P : Probe) return SCSI.Command_Block
     with Pre  => P.Current in Active
                  and then (if P.Current = Check_ISO then
                              ISO_Block (P.Geometry) < P.Geometry.Blocks),
          Post => SCSI.Well_Formed (Command'Result)
                  and then Command'Result.Expected <= Largest_Probe_Bytes
                  and then Command'Result.Direction /= SCSI.To_Device;

   --  A measure that every Advance decreases: the probe terminates.
   function Remaining (P : Probe) return Natural is
     (case P.Current is
         when Inquire     => 6 + 2 * Ready_Attempt_Limit,
         when Test_Ready  => 5 + 2 * (Ready_Attempt_Limit - P.Attempts),
         when Read_Sense  => 4 + 2 * (Ready_Attempt_Limit - P.Attempts),
         when Capacity_10 => 4,
         when Capacity_16 => 3,
         when Mode_Sense  => 2,
         when Check_ISO   => 1,
         when Finished    => 0);

   --  The command's verdict: Kind from USB_Bulk_Only.Decode_Status (a
   --  transport fault is Invalid, after the adapter's reset recovery), and
   --  Data the bytes the host received. Data's length is the moved count.
   procedure Advance
     (P : in out Probe; Kind : USB_Bulk_Only.Status_Kind; Data : SCSI.Bytes)
     with Pre  => P.Current in Active
                  and then Data'Length <= Largest_Probe_Bytes,
          Post => Remaining (P) < Remaining (P'Old)
                  --  Only a disk the ISO check passed is served (a disk
                  --  too small to hold byte 32768 holds no ISO 9660).
                  and then (if P.Current = Ready then
                              (P'Old.Current = Check_ISO
                               and then Kind = USB_Bulk_Only.Good
                               and then not ISO_Signature (Data))
                              or else
                              (P'Old.Current = Mode_Sense
                               and then ISO_Block (P.Geometry) >= P.Geometry.Blocks))
                  and then (if P.Current = Check_ISO then
                              ISO_Block (P.Geometry) < P.Geometry.Blocks)
                  and then (if P'Old.Current = Check_ISO
                              and then Kind = USB_Bulk_Only.Good
                              and then ISO_Signature (Data)
                            then P.Current = ISO_Medium)
                  --  Geometry and protection are only ever set by their
                  --  own commands.
                  and then (if P'Old.Current not in Capacity_10 | Capacity_16
                            then P.Geometry = P'Old.Geometry)
                  and then (if P'Old.Current /= Mode_Sense then
                              P.Write_Protected = P'Old.Write_Protected
                            else
                              P.Write_Protected =
                                (Kind = USB_Bulk_Only.Good
                                 and then Data'Length >= SCSI.Mode_Header_6_Bytes
                                 and then Data (Data'First) >= SCSI.Mode_Header_6_Bytes - 1
                                 and then (Data (Data'First + 2) and SCSI.Write_Protect_Bit) /= 0))
                  --  A command that did not pass decides nothing about
                  --  identity, capacity or the ISO check: the probe fails.
                  and then (if P'Old.Current in Inquire | Capacity_10 | Capacity_16 | Check_ISO
                              and then Kind /= USB_Bulk_Only.Good
                            then P.Current = Failed);
end USB_Disk_Probe;
