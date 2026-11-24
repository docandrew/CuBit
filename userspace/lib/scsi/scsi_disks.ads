------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a direct-access SCSI disk's responses mean to a block driver
--  (SBC-3, SPC-4): which INQUIRY data is a disk, its geometry from READ
--  CAPACITY(10) or (16), and what a failed command's sense data says about
--  the medium. Transport-blind: USB mass storage (usb_disk_probe) uses it,
--  and any later SCSI disk transport can.
--
--  Every response is untrusted device data. The contracts state the
--  decoded meaning exactly in terms of the response bytes.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with SCSI; use SCSI;

package SCSI_Disks with SPARK_Mode, Pure is

   ---------------------------------------------------------------------------
   --  Identification.
   ---------------------------------------------------------------------------
   --  A connected direct-access block device (SPC-4 6.6.2, peripheral
   --  qualifier 000b, device type 00h).
   function Is_Disk (Item : Inquiry_Data) return Boolean is
     (Item.Qualifier = Unit_Connected
      and then Item.Device_Type = Direct_Access_Device);

   ---------------------------------------------------------------------------
   --  Geometry.
   ---------------------------------------------------------------------------
   --  The logical block sizes CuBit serves a disk with: 512 (USB sticks,
   --  SD cards) and 4096 (4Kn disks, some SSD enclosures).
   type Disk_Block_Size is (Block_512, Block_4096);
   function Bytes_Of (Size : Disk_Block_Size) return Block_Length is
     (case Size is when Block_512 => 512, when Block_4096 => 4096);

   subtype Disk_Blocks is Unsigned_64 range 1 .. Unsigned_64'Last;

   type Geometry is record
      Blocks : Disk_Blocks := 1;
      Size   : Disk_Block_Size := Block_512;
   end record;

   type Capacity_Outcome is
     (Capacity_Valid,
      Capacity_16_Required,     --  (10) overflowed: ask READ CAPACITY(16)
      Unsupported_Block_Size,   --  neither 512 nor 4096 bytes
      Malformed_Capacity);      --  wrong length, or a last LBA of 2**64 - 1

   --  Big-endian fields of a response, for the contracts.
   function Big_Endian_32 (Data : Bytes; First : Positive) return Unsigned_32
   is (Shift_Left (Unsigned_32 (Data (First)), 24)
       or Shift_Left (Unsigned_32 (Data (First + 1)), 16)
       or Shift_Left (Unsigned_32 (Data (First + 2)), 8)
       or Unsigned_32 (Data (First + 3)))
     with Pre => First >= Data'First and then Data'Last >= First
                 and then Data'Last - First >= 3;

   function Big_Endian_64 (Data : Bytes; First : Positive) return Unsigned_64
   is (Shift_Left (Unsigned_64 (Big_Endian_32 (Data, First)), 32)
       or Unsigned_64 (Big_Endian_32 (Data, First + 4)))
     with Pre => First >= Data'First and then Data'Last >= First
                 and then Data'Last - First >= 7;

   --  Whether Length (a response's block length field) is a served size,
   --  and which.
   function Served (Length : Unsigned_32) return Boolean is
     (Length = 512 or else Length = 4096);
   function Size_Of (Length : Unsigned_32) return Disk_Block_Size is
     (if Length = 4096 then Block_4096 else Block_512)
     with Pre => Served (Length);

   --  READ CAPACITY(10) parameter data: last LBA (bytes 1 .. 4), block
   --  length (bytes 5 .. 8).
   procedure From_Capacity_10
     (Data : Bytes; Result : out Geometry; Outcome : out Capacity_Outcome)
     with Post =>
       (if Data'Length /= Capacity_10_Bytes then
          Outcome = Malformed_Capacity
        elsif Big_Endian_32 (Data, Data'First) = Capacity_10_Overflow then
          Outcome = Capacity_16_Required
        elsif not Served (Big_Endian_32 (Data, Data'First + 4)) then
          Outcome = Unsupported_Block_Size
        else
          Outcome = Capacity_Valid
          and then Result.Blocks =
            Unsigned_64 (Big_Endian_32 (Data, Data'First)) + 1
          and then Bytes_Of (Result.Size) =
            Big_Endian_32 (Data, Data'First + 4));

   --  READ CAPACITY(16) parameter data: last LBA (bytes 1 .. 8), block
   --  length (bytes 9 .. 12); the rest (protection, exponents) is not
   --  needed to serve the disk. At least those 12 bytes.
   Capacity_16_Minimum : constant := 12;
   procedure From_Capacity_16
     (Data : Bytes; Result : out Geometry; Outcome : out Capacity_Outcome)
     with Post =>
       Outcome /= Capacity_16_Required
       and then
       (if Data'Length < Capacity_16_Minimum
          or else Big_Endian_64 (Data, Data'First) = Unsigned_64'Last
        then
          Outcome = Malformed_Capacity
        elsif not Served (Big_Endian_32 (Data, Data'First + 8)) then
          Outcome = Unsupported_Block_Size
        else
          Outcome = Capacity_Valid
          and then Result.Blocks = Big_Endian_64 (Data, Data'First) + 1
          and then Bytes_Of (Result.Size) =
            Big_Endian_32 (Data, Data'First + 8));

   ---------------------------------------------------------------------------
   --  Sense data: what a failed command says about the medium.
   ---------------------------------------------------------------------------
   --  Additional sense codes (SPC-4 Annex D).
   ASC_Becoming_Ready     : constant Unsigned_8 := 16#04#;   --  with key Not Ready
   ASC_Write_Protected    : constant Unsigned_8 := 16#27#;
   ASC_Medium_Changed     : constant Unsigned_8 := 16#28#;   --  not ready to ready
   ASC_Reset_Occurred     : constant Unsigned_8 := 16#29#;   --  power on, reset
   ASC_Medium_Not_Present : constant Unsigned_8 := 16#3A#;

   type Sense_Meaning is
     (No_Error,           --  no sense, or a recovered error
      Retry_Later,        --  becoming ready, or a power-on/reset unit attention
      Medium_Absent,      --  no medium (an empty card reader)
      Medium_Changed,     --  the medium may have been replaced
      Write_Protected,    --  the medium refuses writes
      Command_Refused,    --  illegal request: the command is not supported
      Medium_Failure);    --  anything else: the command failed

   function Meaning (Sense : Sense_Data) return Sense_Meaning is
     (case Sense.Key is
         when No_Sense | Recovered_Error | Completed => No_Error,
         when Not_Ready =>
           (if Sense.Code = ASC_Medium_Not_Present then Medium_Absent
            elsif Sense.Code = ASC_Becoming_Ready then Retry_Later
            else Medium_Failure),
         when Unit_Attention =>
           (if Sense.Code = ASC_Medium_Changed then Medium_Changed
            elsif Sense.Code = ASC_Medium_Not_Present then Medium_Absent
            else Retry_Later),
         when Data_Protect => Write_Protected,
         when Illegal_Request => Command_Refused,
         when others => Medium_Failure);
end SCSI_Disks;
