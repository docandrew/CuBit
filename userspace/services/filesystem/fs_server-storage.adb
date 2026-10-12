with CuBit.Block_Devices;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Volume_Descriptions;
with Ext2_Support;
with ISO9660;
with Fs_Path_Checks;
with Fs_Server.Access_Control; use Fs_Server.Access_Control;
with Fs_Server.Delegations; use Fs_Server.Delegations;
with Fs_Server.Replies; use Fs_Server.Replies;
with Fs_Server.Watches; use Fs_Server.Watches;

package body Fs_Server.Storage is

   use type Ext2.Flush_Status;
   use ASCII;

   FS_PAGE_SIZE : constant := 4096;

   --  Generic endpoint admission. The registry role is only a boot-order hint;
   --  grant creation is authorized by the endpoint already installed by devmgr.
   procedure ensureVolume (Volume : Volume_Index; Result : out Admission_Result) is
      Device : constant Device_Binding := Binding (Volumes, Volume);
      Context : Volume_Context renames Contexts (Volume);
      Grant_OK, Revoked : Boolean;
      Provider : Process_ID;
      Raw : Unsigned_64;
   begin
      Result := Context.Status;
      if Context.Status /= Provider_Not_Ready then
         return;
      end if;
      if Device.Ready_Role /= 0 then
         Provider := Registered_Driver (Device.Ready_Role);
         if Provider = No_Process then
            return; -- not supplied yet; no session or identity was replaced
         end if;
      end if;
      if Context.Buffer = System.Null_Address then
         Raw := syscall
           (SYSCALL_SBRK, Unsigned_64 (Device.Transfer_Pages + 1) * FS_PAGE_SIZE);
         if Raw = 0 or else Raw = Unsigned_64'Last then
            Result := Insufficient_Resources;
            return;
         end if;
         Context.Buffer := toAddr
           ((Raw + FS_PAGE_SIZE - 1) and not (FS_PAGE_SIZE - 1));
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (Device.Endpoint, Context.Buffer, Device.Transfer_Pages, True,
         Context.Grant, Grant_OK);
      if not Grant_OK then
         Result := Grant_Rejected;
         return;
      end if;
      Ext2.initBlockDevice
        (Context.Fs, Device.Endpoint, Context.Grant, Context.Buffer,
         Unsigned_32 (Device.Transfer_Pages) * FS_PAGE_SIZE, Result);
      Context.Status := Result;
      bumpNamespace;   --  names on this volume resolve anew
      if Result = Admitted then
         debugPrint ("FS Server: volume " & Volume_List.Name (Volumes, Volume) & " ready." & LF);
      else
         CuBit.Memory_Grants.Revoke (Context.Grant, Revoked);
         --  No rebinding/retry after admission uncertainty. A pending revoked
         --  grant must not be overwritten with a new reference.
         debugPrint ("FS Server: volume " & Volume_List.Name (Volumes, Volume) &
                     " rejected: " & Description (Result) & LF);
      end if;
   end ensureVolume;

   procedure mountBootImage (address, size : Unsigned_64) is
   begin
      if address = 0 then
         debugPrint ("FS Server: No ramdisk found, disk-only mode." & LF);
         cpioOk := False;
         return;
      end if;
      debugPrint ("FS Server: Got ramdisk, initializing CPIO..." & LF);

      Cpio.init (cpioArchive, toAddr (address), size, cpioOk);
      if not cpioOk then
         debugPrint ("FS Server: Invalid CPIO archive on ramdisk." & LF);
      elsif Cpio.findFile (cpioArchive, "live-rw.ext2") < cpioArchive.count then
         declare
            Result : Registration_Result;
            admission : Admission_Result;
         begin
            Register
              (Volumes, "mem:0",
               (Endpoint => CAP_SLOT_RAMDISK, Ready_Role => 0, Transfer_Pages => 128),
               Default_Write_Volume, Result);
            if Result = Registered then
               ensureVolume (Volume_Index (Default_Write_Volume), admission);
               if admission = Admitted then
                  debugPrint ("FS Server: Writable memory filesystem ready." & LF);
               end if;
            end if;
         end;
      end if;
   end mountBootImage;

   procedure registerVolumes (ok : out Boolean) is
      Volume : Volume_Reference;
      Result : Registration_Result;
   begin
      ok := False;
      Register (Volumes, "ata:0",
        (Endpoint => CAP_SLOT_ATA, Ready_Role => DRIVER_ATA, Transfer_Pages => 8),
        Volume, Result);
      if Result /= Registered then
         debugPrint ("FS Server: volume list initialization failed." & LF);
         return;
      end if;
      Register (Volumes, "nvme:0",
        (Endpoint => CAP_SLOT_NVME, Ready_Role => DRIVER_NVME, Transfer_Pages => 128),
        Volume, Result);
      if Result /= Registered then
         debugPrint ("FS Server: volume list initialization failed." & LF);
         return;
      end if;
      ok := True;
   end registerVolumes;

   package VD renames CuBit.Volume_Descriptions;
   use type VD.Volume_Kind;
   pragma Compile_Time_Error
     (VD.Record_Bytes /= FQ.Volume_Description_Bytes, "Volume.Description.V1 size");

   procedure handleDescribeVolume (owner : Process_ID; pathLength, rangeBytes : Unsigned_64) is
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));
      selection : Path_Selection;
      reference : Volume_Reference;
      relStart : Integer;
      item : VD.Description;
      bytes : VD.Record_Image;

      procedure setName (name : String) is
      begin
         item.Length := Natural'Min (name'Length, VD.Maximum_Name_Bytes);
         for i in 1 .. item.Length loop
            item.Name (i) := Character'Pos (name (name'First + i - 1));
         end loop;
      end setName;
   begin
      if pathLength = 0 or else pathLength > MAXIMUM_PATH_BYTES or else
        pathLength > rangeBytes or else rangeBytes < VD.Record_Bytes
      then
         sendReply (owner, REPLY_ERR, 0);
         return;
      end if;
      declare
         source : String (1 .. Natural (pathLength)) with Import, Address => curArena;
      begin
         pathBuffer (1 .. Natural (pathLength)) := source;   --  copy, then validate
      end;
      declare
         path : String renames pathBuffer (1 .. Natural (pathLength));
      begin
         if Fs_Path_Checks.Has_Traversal (path) then
            sendReply (owner, REPLY_ERR, 0);
            return;
         elsif not checkAccess (owner, path, ACL_READ) then
            sendReply (owner, REPLY_ACCESS_DENIED, 0);
            return;
         end if;
         Select_Path (Volumes, path, selection, reference, relStart);
      end;
      if selection = Unqualified then
         reference := Default_Write_Volume;
         selection := (if reference = No_Volume then Unknown_Volume else Known_Volume);
      end if;
      case selection is
         when Known_Volume =>
            declare
               volume : constant Volume_Index := Volume_Index (reference);
               admission : Admission_Result;
            begin
               ensureVolume (volume, admission);
               if admission /= Admitted then
                  sendReply (owner, REPLY_IO_ERROR, 0);
                  return;
               end if;
               declare
                  fs : Ext2.Filesystem renames Contexts (volume).Fs;
                  free : constant Unsigned_64 := Unsigned_64 (fs.sb.freeBlocks);
                  total : constant Unsigned_64 := Unsigned_64 (fs.sb.blockCount);
               begin
                  item.Kind := (if (fs.sb.compatibleFeatures and Ext2_Support.Compat_Has_Journal) /= 0
                                then VD.Ext3 else VD.Ext2);
                  item.Flags :=
                    (if fs.writeQuarantined or else
                        CuBit.Block_Devices.Is_Read_Only (fs.device.description)
                     then VD.Read_Only else 0) +
                    (if fs.journal.Active and then item.Kind = VD.Ext3 then VD.Journaled else 0) +
                    (if CuBit.Block_Devices.Can_Persist (fs.device.description)
                     then VD.Durable_Flush else 0);
                  item.Block := VD.Block_Size'Max (VD.Smallest_Block,
                                  VD.Block_Size'Min (VD.Largest_Block, fs.blkSize));
                  item.Total_Blocks := total;
                  item.Free_Blocks := Unsigned_64'Min (free, total);
                  item.Releasing_Blocks :=
                    Unsigned_64'Min (Unsigned_64 (fs.pendingCount), total - item.Free_Blocks);
                  item.Total_Inodes := Unsigned_64 (fs.sb.inodeCount);
                  item.Free_Inodes := Unsigned_64'Min (Unsigned_64 (fs.sb.freeInodes), item.Total_Inodes);
                  setName (Volume_List.Name (Volumes, volume));
               end;
            end;
         when Boot_Archive =>
            if not cpioOk then
               sendReply (owner, REPLY_NOT_FOUND, 0);
               return;
            end if;
            item := (Kind => VD.Boot_Archive, Flags => VD.Read_Only, Block => VD.Smallest_Block,
                     Total_Blocks => (cpioArchive.size + VD.Smallest_Block - 1) / VD.Smallest_Block,
                     others => <>);
            setName (Volume_List.Boot_Archive_Name);
         when Optical_Volume =>
            declare
               blocks : Unsigned_64;
               mounted : Boolean;
            begin
               ISO9660.Describe (blocks, mounted);
               if not mounted then
                  sendReply (owner, REPLY_NOT_FOUND, 0);
                  return;
               end if;
               item := (Kind => VD.ISO_9660, Flags => VD.Read_Only,
                        Block => ISO_Records.Block_Bytes, Total_Blocks => blocks, others => <>);
               setName (Volume_List.Optical_Name);
            end;
         when Unqualified | Unknown_Volume | Invalid_Path =>
            sendReply (owner, REPLY_NOT_FOUND, 0);
            return;
      end case;
      if not VD.Valid (item) then
         sendReply (owner, REPLY_ERR, 0);
         return;
      end if;
      VD.Encode (item, bytes);
      declare
         target : VD.Record_Image with Import, Address => curArena;
      begin
         target := bytes;
      end;
      sendReply (owner, REPLY_OK, VD.Record_Bytes);
   end handleDescribeVolume;

   procedure commitTick is
      status : Ext2.Flush_Status;
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner /= No_Process then
            harvestOwner (q);
         end if;
      end loop;
      for slot in files'Range loop
         if files (slot).active and then files (slot).objectKind = FILE_OBJECT then
            reportModified (slot);
         end if;
      end loop;
      settleWatches;
      for V in Contexts'Range loop
         if Contexts (V).Status = Admitted and then Ext2.dirtyBlocks (Contexts (V).Fs) then
            Ext2.Flush (Contexts (V).Fs, status);
            if status /= Ext2.Flush_Complete then
               debugPrint ("FS: periodic commit failed" & LF);
            end if;
         end if;
      end loop;
   end commitTick;

end Fs_Server.Storage;
