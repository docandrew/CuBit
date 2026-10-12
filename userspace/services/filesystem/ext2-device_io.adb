------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Messages; use CuBit.Messages;
with Ext2.Block_Cache; use Ext2.Block_Cache;

package body Ext2.Device_IO is

   use type System.Address;

   --  Device requests issued (and barriers among them), for profiling
   --  the I/O each operation costs (deviceRequests).
   deviceCalls, deviceFlushes : Unsigned_64 := 0;

   procedure deviceRequests (requests, flushes : out Unsigned_64) is
   begin
      requests := deviceCalls;
      flushes := deviceFlushes;
   end deviceRequests;

   procedure deviceRead
     (fs     : Filesystem;
      offset : Storage_Offset;
      dest   : System.Address;
      len    : Storage_Count;
      status : out Read_Status)
   is
   begin
      status := Read_Device_Error;
      if offset < 0 then
         status := Read_Out_Of_Range;
         return;
      end if;

      --  Convert byte offset/len to multi-sector reads via IPC.
      --  Read up to the session transfer bound per IPC call into the
      --  transitional grant buffer, then copy to dest.
      declare
         byteOff       : Unsigned_64 := Unsigned_64 (offset);
         remaining     : Unsigned_64 := Unsigned_64 (len);
         dstOff        : Storage_Offset := 0;
         lba           : Unsigned_64;
         secOff        : Unsigned_64;
         copyLen       : Unsigned_64;
         sectorsNeeded : Unsigned_64;
         deviceBlockSize : constant Unsigned_64 :=
           Unsigned_64 (fs.device.description.logicalBlockSize);
         maxSectors    : constant Unsigned_64 := Unsigned_64'Min
           (Unsigned_64 (fs.device.grantBytes) / deviceBlockSize,
            Unsigned_64 (fs.device.description.maxTransferBlocks));
         msg           : Message;
         ignore        : MessageTag;
      begin
         while remaining > 0 loop
            lba    := byteOff / deviceBlockSize;
            secOff := byteOff mod deviceBlockSize;

            if lba >= fs.device.description.blockCount then
               status := Read_Out_Of_Range;
               return;
            end if;

            --  Calculate how many sectors to read in this batch
            sectorsNeeded :=
              (remaining + secOff + deviceBlockSize - 1) /
              deviceBlockSize;
            if sectorsNeeded > maxSectors then
               sectorsNeeded := maxSectors;
            end if;

            if sectorsNeeded = 0 or else
               sectorsNeeded > fs.device.description.blockCount - lba
            then
               status := Read_Out_Of_Range;
               return;
            end if;

            --  Read multiple sectors from driver
            msg.tag := (label  => OP_READ_BLOCKS,
                        length => 4,
                        flags  => 0,
                        reserved  => 0);
            msg.authorityTag := 0;
            msg.words := [0 => lba,
                          1 => fs.device.grant.slot,
                          2 => sectorsNeeded,
                          3 => fs.device.grant.generation];

            deviceCalls := deviceCalls + 1;
            ignore := capCall (fs.device.endpointSlot, msg, CuBit.Messages.Wait_Forever);

            if msg.tag.label /= REPLY_OK or else
               msg.tag.length /= 1 or else
               msg.tag.flags /= 0 or else msg.tag.reserved /= 0 or else
               msg.words (0) /= sectorsNeeded * deviceBlockSize
            then
               debugPrint ("Ext2: read reply not OK." & ASCII.LF);
               return;
            end if;

            --  Copy relevant portion from grant buffer to dest
            copyLen :=
              sectorsNeeded * deviceBlockSize - secOff;
            if copyLen > remaining then
               copyLen := remaining;
            end if;

            declare
               srcSlice : String (1 .. Natural (copyLen))
                 with Import,
                      Address => fs.device.grantBuffer +
                        Storage_Offset (secOff);
               dstSlice : String (1 .. Natural (copyLen))
                 with Import,
                      Address => dest + dstOff;
            begin
               dstSlice := srcSlice;
            end;

            byteOff   := byteOff + copyLen;
            dstOff    := dstOff + Storage_Offset (copyLen);
            remaining := remaining - copyLen;
         end loop;
         status := Read_Complete;
      end;
   end deviceRead;

   procedure deviceWrite
     (fs     : Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count;
      status : out Write_Status;
      fua    : Boolean := False)
   is
   begin
      status := Write_Device_Error;

      if fs.writeQuarantined then
         status := Write_Read_Only;
         return;
      end if;

      if Is_Read_Only (fs.device.description)
      then
         status := Write_Read_Only;
         return;
      end if;

      if offset < 0 then
         status := Write_Out_Of_Range;
         return;
      end if;

      declare
         byteOff   : Unsigned_64 := Unsigned_64 (offset);
         remaining : Unsigned_64 := Unsigned_64 (len);
         srcOff    : Storage_Offset := 0;
         lba       : Unsigned_64;
         secOff    : Unsigned_64;
         copyLen   : Unsigned_64;
         deviceBlockSize : constant Unsigned_64 :=
           Unsigned_64 (fs.device.description.logicalBlockSize);
         msg       : Message;
         ignore    : MessageTag;
         fromCache, cacheReadFailed : Boolean;
      begin
         while remaining > 0 loop
            lba    := byteOff / deviceBlockSize;
            secOff := byteOff mod deviceBlockSize;

            if lba >= fs.device.description.blockCount then
               status := Write_Out_Of_Range;
               return;
            end if;

            if secOff /= 0 or remaining < deviceBlockSize then
               --  Partial sector: read-modify-write
               copyLen := deviceBlockSize - secOff;
               if copyLen > remaining then
                  copyLen := remaining;
               end if;

               --  The sector's other bytes: cached blocks equal the
               --  acknowledged disk state. Outside the volume's blocks (or
               --  before admission), read the sector itself.
               cachedSector (fs, lba, deviceBlockSize, fromCache, cacheReadFailed);
               if cacheReadFailed then
                  debugPrint ("Ext2: write RMW read failed." & ASCII.LF);
                  return;
               elsif not fromCache then
                  msg.tag := (label  => OP_READ_BLOCKS,
                              length => 4,
                              flags  => 0,
                              reserved  => 0);
                  msg.authorityTag := 0;
                  msg.words := [0 => lba,
                                1 => fs.device.grant.slot,
                                2 => 1,
                                3 => fs.device.grant.generation];
            deviceCalls := deviceCalls + 1;
            ignore := capCall (fs.device.endpointSlot, msg, CuBit.Messages.Wait_Forever);

                  if msg.tag.label /= REPLY_OK or else
                     msg.tag.length /= 1 or else
                     msg.tag.flags /= 0 or else msg.tag.reserved /= 0 or else
                     msg.words (0) /= deviceBlockSize
                  then
                     debugPrint
                       ("Ext2: write RMW read failed." & ASCII.LF);
                     return;
                  end if;
               end if;

               --  Overlay our data onto the grant buffer
               declare
                  dstSlice : String (1 .. Natural (copyLen))
                    with Import,
                         Address => fs.device.grantBuffer +
                           Storage_Offset (secOff);
                  srcSlice : String (1 .. Natural (copyLen))
                    with Import,
                         Address => src + srcOff;
               begin
                  dstSlice := srcSlice;
               end;

               --  Write the modified sector back
               msg.tag := (label  => OP_WRITE_BLOCKS,
                           length => 4,
                           flags  => 0,
                           reserved  => 0);
               msg.authorityTag := 0;
               msg.words := [0 => lba,
                             1 => fs.device.grant.slot,
                             2 => 1,
                             3 => fs.device.grant.generation];
            deviceCalls := deviceCalls + 1;
            ignore := capCall (fs.device.endpointSlot, msg, CuBit.Messages.Wait_Forever);

               if msg.tag.label /= REPLY_OK or else
                  msg.tag.length /= 1 or else
                  msg.tag.flags /= 0 or else msg.tag.reserved /= 0 or else
                  msg.words (0) /= deviceBlockSize
               then
                  debugPrint
                    ("Ext2: write RMW write failed." & ASCII.LF);
                  return;
               end if;
            else
               --  Sector-aligned: batch write full sectors
               declare
                  maxWriteSectors : constant Unsigned_64 :=
                    Unsigned_64'Min
                      (Unsigned_64 (fs.device.grantBytes) /
                         deviceBlockSize,
                       Unsigned_64
                         (fs.device.description.maxTransferBlocks));
                  sectorsNeeded : Unsigned_64 :=
                    remaining / deviceBlockSize;
               begin
                  if sectorsNeeded > maxWriteSectors then
                     sectorsNeeded := maxWriteSectors;
                  end if;

                  if sectorsNeeded = 0 or else
                     sectorsNeeded >
                       fs.device.description.blockCount - lba
                  then
                     status := Write_Out_Of_Range;
                     return;
                  end if;

                  copyLen :=
                    sectorsNeeded * deviceBlockSize;

                  --  Copy source data into grant buffer
                  declare
                     dstSlice : String (1 .. Natural (copyLen))
                       with Import, Address => fs.device.grantBuffer;
                     srcSlice : String (1 .. Natural (copyLen))
                       with Import, Address => src + srcOff;
                  begin
                     dstSlice := srcSlice;
                  end;

                  --  Write sectors
                  msg.tag := (label  => OP_WRITE_BLOCKS,
                              length => 4,
                              flags  => (if fua then WRITE_FLAG_FUA else 0),
                              reserved  => 0);
                  msg.authorityTag := 0;
                  msg.words := [0 => lba,
                                1 => fs.device.grant.slot,
                                2 => sectorsNeeded,
                                3 => fs.device.grant.generation];
            deviceCalls := deviceCalls + 1;
            ignore := capCall (fs.device.endpointSlot, msg, CuBit.Messages.Wait_Forever);

                  if msg.tag.label /= REPLY_OK or else
                     msg.tag.length /= 1 or else
                     msg.tag.flags /= 0 or else msg.tag.reserved /= 0 or else
                     msg.words (0) /= copyLen
                  then
                     debugPrint
                       ("Ext2: batch write failed." & ASCII.LF);
                     return;
                  end if;
               end;
            end if;

            byteOff   := byteOff + copyLen;
            srcOff    := srcOff + Storage_Offset (copyLen);
            remaining := remaining - copyLen;
         end loop;
         status := Write_Complete;
      end;
   end deviceWrite;

   procedure barrier (fs : in out Filesystem; ok : out Boolean) is
      msg : Message := NULL_MESSAGE;
   begin
      ok := True;
      if not Has_Volatile_Cache (fs.device.description) then
         Home_Writes_Pending := False;
         return;
      end if;
      msg.tag := (label => OP_FLUSH_DEVICE, length => 0, flags => 0, reserved => 0);
      deviceCalls := deviceCalls + 1;
      deviceFlushes := deviceFlushes + 1;
      msg.tag := capCall (fs.device.endpointSlot, msg, CuBit.Messages.Wait_Forever);
      ok := msg.tag.label = CuBit.Block_Devices.REPLY_OK and then
        msg.tag.length = 1 and then msg.tag.flags = 0 and then
        msg.tag.reserved = 0 and then msg.words (0) = 0;
      if ok then
         Home_Writes_Pending := False;
      else
         fs.writeQuarantined := True;
      end if;
   end barrier;

   procedure writeDurable
     (fs : in out Filesystem; number : Unsigned_32; source : System.Address;
      ok : out Boolean)
   is
      status : Write_Status;
      useFua : constant Boolean :=
        Has_Volatile_Cache (fs.device.description) and then
        Supports_FUA (fs.device.description);
   begin
      deviceWrite (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize), source,
                   Storage_Count (fs.blkSize), status, fua => useFua);
      ok := status = Write_Complete;
      if ok and then not useFua then
         barrier (fs, ok);
      end if;
      if not ok then
         fs.writeQuarantined := True;
      end if;
   end writeDurable;

end Ext2.Device_IO;
