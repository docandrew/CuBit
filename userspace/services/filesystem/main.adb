------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace Ext2 filesystem server.
--
--  Serves file operations from a ramdisk Ext2 image via IPC.
--  Receives OP_OPEN/OP_READ/OP_SEEK/OP_CLOSE messages from clients
--  and replies with file data.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;

with CuBit.Messages; use CuBit.Messages;
with Cpio;
with Ext2;

procedure main is
   use ASCII;

   --  IPC operation labels (must match kernel/src/ipc_labels.ads)
   OP_OPEN    : constant Unsigned_32 := 16#0001#;
   OP_CLOSE   : constant Unsigned_32 := 16#0002#;
   OP_READ    : constant Unsigned_32 := 16#0003#;
   OP_WRITE   : constant Unsigned_32 := 16#0004#;
   OP_SEEK    : constant Unsigned_32 := 16#0006#;
   OP_READDIR : constant Unsigned_32 := 16#0007#;
   REPLY_OK   : constant Unsigned_32 := 16#F000#;
   REPLY_ERR  : constant Unsigned_32 := 16#F001#;

   --  Sysinfo query for ramdisk address
   --  (uses SYSINFO_RAMDISK_ADDRESS from CuBit.Messages)

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 256 * 4096;  --  1 MiB

   --  Maximum open files and path length
   MAX_OPEN_FILES : constant := 32;
   MAX_PATH_LEN   : constant := 256;

   --  Backend kind for file handles
   type BackendKind is (CPIO_RAMDISK, EXT2_ATA, EXT2_NVME);

   --  File handle entry (tracks which backend each file uses)
   type FileEntry is record
      active      : Boolean      := False;
      backend     : BackendKind  := CPIO_RAMDISK;
      inodeNum    : Unsigned_32  := 0;      --  ext2 only
      ino         : Ext2.Inode;             --  ext2 only
      cpioFileIdx : Natural      := 0;      --  cpio only
      offset      : Unsigned_64  := 0;
      ownerPID    : ProcessID    := NO_PROCESS;
   end record;

   type FileTable is array (0 .. MAX_OPEN_FILES - 1) of FileEntry;
   files : FileTable;

   --  CPIO ramdisk archive
   cpioArchive : Cpio.Archive;
   cpioOk      : Boolean := False;

   --  ATA-backed filesystem context (lazy initialized)
   ataFs          : Ext2.Filesystem;
   ataInitialized : Boolean := False;
   ataInitFailed  : Boolean := False;  --  true after a real init attempt fails

   --  ATA grant buffer (8 pages = 32KB for multi-sector bulk reads)
   ATA_GRANT_PAGES : constant := 8;
   ataGrantBuf     : System.Address := System.Null_Address;
   ataGrantId      : Unsigned_64 := 0;

   --  NVMe-backed filesystem context (lazy initialized)
   nvmeFs          : Ext2.Filesystem;
   nvmeInitialized : Boolean := False;

   --  NVMe grant buffer (128 pages = 512KB for large PRP transfers)
   NVME_GRANT_PAGES : constant := 128;
   nvmeGrantBuf     : System.Address := System.Null_Address;
   nvmeGrantId      : Unsigned_64 := 0;

   --  Conversion helpers
   function toAddr is new Ada.Unchecked_Conversion
     (Unsigned_64, System.Address);

   --  Find a free file handle
   function allocHandle return Integer is
   begin
      for i in files'Range loop
         if not files (i).active then
            return i;
         end if;
      end loop;
      return -1;
   end allocHandle;

   --  Send a reply with the given label and word0 value
   procedure sendReply
     (dest   : ProcessID;
      label  : Unsigned_32;
      word0  : Unsigned_64)
   is
      replyMsg : Message;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 1,
                       flags  => 0,
                       badge  => 0);
      replyMsg.words := (0 => word0, others => 0);
      ignore := reply (dest, replyMsg);
   end sendReply;

   --  ATA driver PID (discovered at runtime via sysinfo)
   ataDriverPID  : Unsigned_64 := 0;

   --  NVMe driver PID (discovered at runtime via sysinfo)
   nvmeDriverPID : Unsigned_64 := 0;

   --  Page size for grant buffer allocation
   FS_PAGE_SIZE : constant := 4096;

   --  Check if path starts with "@ata:" or "@nvme:" prefix.
   --  Returns True in the matching output and sets relStart to
   --  the index after the prefix.
   procedure parseScheme
     (pathStr  : String;
      isATA    : out Boolean;
      isNVMe   : out Boolean;
      relStart : out Natural)
   is
   begin
      isATA    := False;
      isNVMe   := False;
      relStart := pathStr'First;

      if pathStr'Length >= 6 and then
         pathStr (pathStr'First)     = '@' and then
         pathStr (pathStr'First + 1) = 'n' and then
         pathStr (pathStr'First + 2) = 'v' and then
         pathStr (pathStr'First + 3) = 'm' and then
         pathStr (pathStr'First + 4) = 'e' and then
         pathStr (pathStr'First + 5) = ':'
      then
         isNVMe   := True;
         relStart := pathStr'First + 6;
      elsif pathStr'Length >= 5 and then
         pathStr (pathStr'First)     = '@' and then
         pathStr (pathStr'First + 1) = 'a' and then
         pathStr (pathStr'First + 2) = 't' and then
         pathStr (pathStr'First + 3) = 'a' and then
         pathStr (pathStr'First + 4) = ':'
      then
         isATA    := True;
         relStart := pathStr'First + 5;
      end if;
   end parseScheme;

   --  Lazy-initialize the ATA filesystem on first @ata: open.
   --  Creates a grant buffer and reads the superblock via ATA IPC.
   procedure ensureATA (ok : out Boolean) is
      grantOk : Boolean;
   begin
      if ataInitialized then
         ok := True;
         return;
      end if;

      if ataInitFailed then
         ok := False;
         return;
      end if;

      --  Discover ATA driver PID via sysinfo
      if ataDriverPID = 0 then
         ataDriverPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_ATA);
         if ataDriverPID = 0 or ataDriverPID = Unsigned_64'Last then
            debugPrint ("FS Server: ATA driver not registered." & LF);
            ataDriverPID := 0;
            ok := False;
            return;
         end if;
      end if;

      --  Allocate a page-aligned grant buffer via sbrk
      if ataGrantBuf = System.Null_Address then
         declare
            raw : Unsigned_64;
            aligned : Unsigned_64;
            allocSize : constant Unsigned_64 :=
              Unsigned_64 (ATA_GRANT_PAGES) * FS_PAGE_SIZE + FS_PAGE_SIZE;
         begin
            raw := syscall (SYSCALL_SBRK, allocSize);
            if raw = Unsigned_64'Last then
               debugPrint ("FS Server: sbrk failed for ATA grant buffer." & LF);
               ok := False;
               return;
            end if;
            aligned := (raw + FS_PAGE_SIZE - 1) and not (FS_PAGE_SIZE - 1);
            ataGrantBuf := toAddr (aligned);
         end;
      end if;

      --  Create a grant to the ATA driver for data transfer
      createGrant
        (grantee   => ataDriverPID,
         localAddr => ataGrantBuf,
         numPages  => ATA_GRANT_PAGES,
         readWrite => True,
         grantId   => ataGrantId,
         success   => grantOk);

      if not grantOk then
         debugPrint ("FS Server: Failed to create ATA grant." & LF);
         ok := False;
         return;
      end if;

      debugPrint ("FS Server: ATA grant OK, grantId=" & LF);
      debugPrint ("FS Server: Initializing ATA ext2 filesystem..." & LF);

      Ext2.initATA (ataFs, CAP_SLOT_ATA, ataGrantId, ataGrantBuf, ok);

      if ok then
         ataFs.grantBufSize := ATA_GRANT_PAGES * 4096;
         ataInitialized := True;
         debugPrint ("FS Server: ATA ext2 filesystem initialized." & LF);
      else
         ataInitFailed := True;
         debugPrint ("FS Server: ATA ext2 init failed (no ext2?)." & LF);
         revokeGrant (ataGrantId);
      end if;
   end ensureATA;

   --  Lazy-initialize the NVMe filesystem on first @nvme: open.
   --  Uses same IPC protocol as ATA (OP_READ_BLOCK).
   procedure ensureNVMe (ok : out Boolean) is
      grantOk : Boolean;
   begin
      if nvmeInitialized then
         ok := True;
         return;
      end if;

      --  Discover NVMe driver PID via sysinfo
      if nvmeDriverPID = 0 then
         nvmeDriverPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_NVME);
         if nvmeDriverPID = 0 or nvmeDriverPID = Unsigned_64'Last then
            nvmeDriverPID := 0;
            ok := False;
            return;
         end if;
      end if;

      --  Allocate a page-aligned grant buffer via sbrk
      if nvmeGrantBuf = System.Null_Address then
         declare
            raw       : Unsigned_64;
            aligned   : Unsigned_64;
            allocSize : constant Unsigned_64 :=
              Unsigned_64 (NVME_GRANT_PAGES) * FS_PAGE_SIZE + FS_PAGE_SIZE;
         begin
            raw := syscall (SYSCALL_SBRK, allocSize);
            if raw = Unsigned_64'Last then
               debugPrint ("FS Server: sbrk failed for NVMe grant buffer." & LF);
               ok := False;
               return;
            end if;
            aligned := (raw + FS_PAGE_SIZE - 1) and not (FS_PAGE_SIZE - 1);
            nvmeGrantBuf := toAddr (aligned);
         end;
      end if;

      --  Create a grant to the NVMe driver for data transfer
      createGrant
        (grantee   => nvmeDriverPID,
         localAddr => nvmeGrantBuf,
         numPages  => NVME_GRANT_PAGES,
         readWrite => True,
         grantId   => nvmeGrantId,
         success   => grantOk);

      if not grantOk then
         debugPrint ("FS Server: Failed to create NVMe grant." & LF);
         ok := False;
         return;
      end if;

      debugPrint ("FS Server: NVMe grant OK, initializing ext2..." & LF);

      --  Reuse initATA with NVMe cap slot (same IPC protocol)
      Ext2.initATA (nvmeFs, CAP_SLOT_NVME, nvmeGrantId, nvmeGrantBuf, ok);

      if ok then
         --  Override backend to NVME for readBytes dispatch
         nvmeFs.backend := Ext2.NVME;
         nvmeFs.grantBufSize := NVME_GRANT_PAGES * 4096;
         nvmeInitialized := True;
         debugPrint ("FS Server: NVMe ext2 filesystem initialized." & LF);
      else
         debugPrint ("FS Server: NVMe ext2 init failed." & LF);
         revokeGrant (nvmeGrantId);
      end if;
   end ensureNVMe;

   --  Handle OP_OPEN
   --  words(0) = grant_id (where path string is)
   --  words(1) = path_length
   --  words(2) = flags (unused for now)
   procedure handleOpen (sender : ProcessID; msg : Message) is
      grantId   : constant Unsigned_64 := msg.words (0);
      pathLen   : constant Unsigned_64 := msg.words (1);
      grantAddr : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;

      handle     : Integer;
      inodeNum   : Unsigned_32 := 0;
      isATA      : Boolean;
      isNVMe     : Boolean;
      relStart   : Natural;
      useBackend : BackendKind := CPIO_RAMDISK;
      cpioIdx    : Natural := 0;
   begin
      if pathLen = 0 or pathLen > MAX_PATH_LEN then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      --  Read path from grant buffer and parse scheme prefix
      declare
         pathStr : String (1 .. Natural (pathLen))
           with Import, Address => toAddr (grantAddr);

         --  Helper: skip optional device selector "0/" after scheme prefix
         procedure skipSelector
           (relPath : String;
            skipIdx : out Natural)
         is
         begin
            skipIdx := relPath'First;
            if relPath'Length > 0 and then
               relPath (relPath'First) in '0' .. '9'
            then
               skipIdx := relPath'First + 1;
               if skipIdx <= relPath'Last and then
                  relPath (skipIdx) = '/'
               then
                  skipIdx := skipIdx + 1;
               end if;
            end if;
         end skipSelector;
      begin
         parseScheme (pathStr, isATA, isNVMe, relStart);

         if isNVMe then
            useBackend := EXT2_NVME;

            --  Lazy-init NVMe filesystem
            declare
               ok : Boolean;
            begin
               ensureNVMe (ok);
               if not ok then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;
            end;

            --  Resolve path on NVMe filesystem
            declare
               relPath : String renames
                 pathStr (relStart .. Natural (pathLen));
               skipIdx : Natural;
            begin
               skipSelector (relPath, skipIdx);
               if skipIdx > relPath'Last then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;
               inodeNum := Ext2.resolvePath
                 (nvmeFs, relPath (skipIdx .. relPath'Last));
            end;

         elsif isATA then
            useBackend := EXT2_ATA;

            --  Lazy-init ATA filesystem
            declare
               ok : Boolean;
            begin
               ensureATA (ok);
               if not ok then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;
            end;

            --  Resolve path on ATA filesystem (skip selector/prefix)
            declare
               relPath : String renames
                 pathStr (relStart .. Natural (pathLen));
               skipIdx : Natural;
            begin
               skipSelector (relPath, skipIdx);
               if skipIdx > relPath'Last then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;
               inodeNum := Ext2.resolvePath
                 (ataFs, relPath (skipIdx .. relPath'Last));
            end;
         else
            --  No scheme prefix: try CPIO ramdisk first, then disk.
            if cpioOk then
               cpioIdx := Cpio.findFile (cpioArchive, pathStr);
               if cpioIdx < cpioArchive.count then
                  useBackend := CPIO_RAMDISK;
                  inodeNum := 1;
               end if;
            end if;

            --  Fallback to disk if not found in ramdisk
            if inodeNum = 0 then
               declare
                  ok : Boolean;
               begin
                  ensureATA (ok);
                  if ok then
                     inodeNum := Ext2.resolvePath (ataFs, pathStr);
                     if inodeNum /= 0 then
                        useBackend := EXT2_ATA;
                     end if;
                  end if;
               end;
            end if;

            if inodeNum = 0 then
               declare
                  ok : Boolean;
               begin
                  ensureNVMe (ok);
                  if ok then
                     inodeNum := Ext2.resolvePath (nvmeFs, pathStr);
                     if inodeNum /= 0 then
                        useBackend := EXT2_NVME;
                     end if;
                  end if;
               end;
            end if;
         end if;
      end;

      if inodeNum = 0 then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      --  Allocate file handle
      handle := allocHandle;
      if handle < 0 then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      --  Set up file entry with backend tracking
      files (handle).active      := True;
      files (handle).inodeNum    := inodeNum;
      files (handle).offset      := 0;
      files (handle).ownerPID    := sender;
      files (handle).backend     := useBackend;
      files (handle).cpioFileIdx := cpioIdx;

      case useBackend is
         when CPIO_RAMDISK =>
            null;  --  cpio files don't need inode
         when EXT2_ATA =>
            Ext2.readInode (ataFs, inodeNum, files (handle).ino);
         when EXT2_NVME =>
            Ext2.readInode (nvmeFs, inodeNum, files (handle).ino);
      end case;

      --  Reply with handle in words(0) and file size in words(1)
      declare
         fsize    : Unsigned_64 := 0;
         replyMsg : Message;
         ignore   : Unsigned_64;
      begin
         case useBackend is
            when CPIO_RAMDISK =>
               fsize := cpioArchive.files (cpioIdx).dataSize;
            when EXT2_ATA =>
               fsize := Ext2.fileSize (files (handle).ino);
            when EXT2_NVME =>
               fsize := Ext2.fileSize (files (handle).ino);
         end case;

         replyMsg.tag := (label  => REPLY_OK,
                          length => 2,
                          flags  => 0,
                          badge  => 0);
         replyMsg.words := (0 => Unsigned_64 (handle),
                            1 => fsize,
                            others => 0);
         ignore := reply (sender, replyMsg);
      end;
   end handleOpen;

   --  Handle OP_READ
   --  words(0) = file_handle
   --  words(1) = grant_id (buffer to write data into)
   --  words(2) = count (bytes to read)
   procedure handleRead (sender : ProcessID; msg : Message) is
      handle    : constant Integer := Integer (msg.words (0));
      grantId   : constant Unsigned_64 := msg.words (1);
      count     : constant Unsigned_64 := msg.words (2);
      grantAddr : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;
      bytesRead : Unsigned_64;
   begin
      if handle < 0 or handle >= MAX_OPEN_FILES or
         not files (handle).active
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      case files (handle).backend is
         when CPIO_RAMDISK =>
            bytesRead := Cpio.readData
              (cpioArchive,
               files (handle).cpioFileIdx,
               files (handle).offset,
               toAddr (grantAddr),
               count);
         when EXT2_ATA =>
            bytesRead := Ext2.readData
              (ataFs,
               files (handle).ino,
               files (handle).offset,
               toAddr (grantAddr),
               count);
         when EXT2_NVME =>
            bytesRead := Ext2.readData
              (nvmeFs,
               files (handle).ino,
               files (handle).offset,
               toAddr (grantAddr),
               count);
      end case;

      files (handle).offset := files (handle).offset + bytesRead;
      sendReply (sender, REPLY_OK, bytesRead);
   end handleRead;

   --  Handle OP_WRITE
   --  words(0) = file_handle
   --  words(1) = grant_id (buffer containing data to write)
   --  words(2) = count (bytes to write)
   procedure handleWrite (sender : ProcessID; msg : Message) is
      handle       : constant Integer := Integer (msg.words (0));
      grantId      : constant Unsigned_64 := msg.words (1);
      count        : constant Unsigned_64 := msg.words (2);
      grantAddr    : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;
      bytesWritten : Unsigned_64;
   begin
      if handle < 0 or handle >= MAX_OPEN_FILES or
         not files (handle).active
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      case files (handle).backend is
         when CPIO_RAMDISK =>
            --  CPIO ramdisk is read-only
            sendReply (sender, REPLY_ERR, 0);
            return;
         when EXT2_ATA =>
            bytesWritten := Ext2.writeData
              (ataFs,
               files (handle).ino,
               files (handle).offset,
               toAddr (grantAddr),
               count);
         when EXT2_NVME =>
            bytesWritten := Ext2.writeData
              (nvmeFs,
               files (handle).ino,
               files (handle).offset,
               toAddr (grantAddr),
               count);
      end case;

      files (handle).offset := files (handle).offset + bytesWritten;
      sendReply (sender, REPLY_OK, bytesWritten);
   end handleWrite;

   --  Handle OP_SEEK
   --  words(0) = file_handle
   --  words(1) = offset
   --  words(2) = whence (0=SET, 1=CUR, 2=END)
   procedure handleSeek (sender : ProcessID; msg : Message) is
      handle  : constant Integer := Integer (msg.words (0));
      seekOff : constant Unsigned_64 := msg.words (1);
      whence  : constant Unsigned_64 := msg.words (2);
      newOff  : Unsigned_64;
      size    : Unsigned_64;
   begin
      if handle < 0 or handle >= MAX_OPEN_FILES or
         not files (handle).active
      then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      case files (handle).backend is
         when CPIO_RAMDISK =>
            size := cpioArchive.files (files (handle).cpioFileIdx).dataSize;
         when EXT2_ATA | EXT2_NVME =>
            size := Ext2.fileSize (files (handle).ino);
      end case;

      case whence is
         when 0 =>  --  SEEK_SET
            newOff := seekOff;
         when 1 =>  --  SEEK_CUR
            newOff := files (handle).offset + seekOff;
         when 2 =>  --  SEEK_END
            newOff := size + seekOff;
         when others =>
            sendReply (sender, REPLY_ERR, Unsigned_64'Last);
            return;
      end case;

      files (handle).offset := newOff;
      sendReply (sender, REPLY_OK, newOff);
   end handleSeek;

   --  Handle OP_CLOSE
   --  words(0) = file_handle
   procedure handleClose (sender : ProcessID; msg : Message) is
      handle : constant Integer := Integer (msg.words (0));
   begin
      if handle < 0 or handle >= MAX_OPEN_FILES or
         not files (handle).active
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      files (handle).active := False;
      sendReply (sender, REPLY_OK, 0);
   end handleClose;

   --  Handle OP_READDIR
   --  words(0) = grant_id (path in grant buffer, results written there)
   --  words(1) = path_length (0 = list root "/")
   procedure handleReaddir (sender : ProcessID; msg : Message) is
      grantId   : constant Unsigned_64 := msg.words (0);
      pathLen   : constant Unsigned_64 := msg.words (1);
      grantAddr : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;

      isATA       : Boolean;
      isNVMe      : Boolean;
      relStart    : Natural;
      dirInodeNum : Unsigned_32;
      dirIno      : Ext2.Inode;
      written     : Unsigned_64;
   begin
      if pathLen = 0 then
         --  No path means list root (ramdisk)
         if not cpioOk then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;
         written := Cpio.listFiles (cpioArchive,
                                    toAddr (grantAddr),
                                    GRANT_SLOT_SIZE);
         sendReply (sender, REPLY_OK, written);
         return;
      end if;

      if pathLen > MAX_PATH_LEN then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      declare
         pathStr : String (1 .. Natural (pathLen))
           with Import, Address => toAddr (grantAddr);

         procedure resolveAndReadDir
           (theFs : Ext2.Filesystem;
            relPath : String)
         is
            skipIdx : Natural := relPath'First;
         begin
            --  Skip optional device selector (e.g., "0/")
            if relPath'Length > 0 and then
               relPath (relPath'First) in '0' .. '9'
            then
               skipIdx := relPath'First + 1;
               if skipIdx <= relPath'Last and then
                  relPath (skipIdx) = '/'
               then
                  skipIdx := skipIdx + 1;
               end if;
            end if;

            if skipIdx > relPath'Last then
               dirInodeNum := Ext2.ROOT_INODE;
            else
               dirInodeNum := Ext2.resolvePath
                 (theFs, relPath (skipIdx .. relPath'Last));
            end if;

            if dirInodeNum = 0 then
               sendReply (sender, REPLY_ERR, 0);
               return;
            end if;

            Ext2.readInode (theFs, dirInodeNum, dirIno);
            written := Ext2.readDir (theFs, dirIno,
                                     toAddr (grantAddr),
                                     GRANT_SLOT_SIZE);
         end resolveAndReadDir;
      begin
         parseScheme (pathStr, isATA, isNVMe, relStart);

         if isNVMe then
            declare
               ok : Boolean;
            begin
               ensureNVMe (ok);
               if not ok then
                  sendReply (sender, REPLY_ERR, 0);
                  return;
               end if;
            end;

            resolveAndReadDir (nvmeFs,
              pathStr (relStart .. Natural (pathLen)));

         elsif isATA then
            declare
               ok : Boolean;
            begin
               ensureATA (ok);
               if not ok then
                  sendReply (sender, REPLY_ERR, 0);
                  return;
               end if;
            end;

            resolveAndReadDir (ataFs,
              pathStr (relStart .. Natural (pathLen)));

         else
            --  Ramdisk (cpio) - just list all files
            if not cpioOk then
               sendReply (sender, REPLY_ERR, 0);
               return;
            end if;

            written := Cpio.listFiles (cpioArchive,
                                       toAddr (grantAddr),
                                       GRANT_SLOT_SIZE);
         end if;
      end;

      sendReply (sender, REPLY_OK, written);
   end handleReaddir;

   --  Deferred reply table for async I/O (Phase 3 foundation)
   MAX_PENDING_CLIENTS : constant := 8;
   type PendingClient is record
      clientPID : ProcessID    := NO_PROCESS;
      handle    : Integer      := -1;
      destAddr  : System.Address := System.Null_Address;
      remaining : Unsigned_64  := 0;
      token     : Unsigned_64  := 0;
      active    : Boolean      := False;
   end record;
   pendingClients : array (0 .. MAX_PENDING_CLIENTS - 1) of PendingClient;

   --  Main message loop variables
   sender : ProcessID;
   msg    : Message;
   rdAddr : Unsigned_64;
   rdSize : Unsigned_64;
begin
   debugPrint ("FS Server: Starting..." & LF);

   --  Get ramdisk address and size from kernel
   rdAddr := getInfo (SYSINFO_RAMDISK_ADDRESS);
   rdSize := getInfo (SYSINFO_RAMDISK_SIZE);
   if rdAddr = 0 or rdAddr = Unsigned_64'Last then
      debugPrint ("FS Server: No ramdisk found, disk-only mode." & LF);
      cpioOk := False;
   else
      debugPrint ("FS Server: Got ramdisk, initializing CPIO..." & LF);

      Cpio.init (cpioArchive, toAddr (rdAddr), rdSize, cpioOk);
      if not cpioOk then
         debugPrint ("FS Server: Invalid CPIO archive on ramdisk." & LF);
      end if;
   end if;

   --  Register as DRIVER_FS so other services can discover us
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_FS);
   end;

   debugPrint ("FS Server: Entering message loop." & LF);

   --  Main IPC message loop.
   --  Uses blocking receive for lowest latency.  Future async work
   --  can switch to receiveNB + pollCompletion when capSubmit-based
   --  driver I/O is implemented.
   loop
      receive (sender, msg);

      case msg.tag.label is
         when OP_OPEN =>
            handleOpen (sender, msg);
         when OP_READ =>
            handleRead (sender, msg);
         when OP_WRITE =>
            handleWrite (sender, msg);
         when OP_SEEK =>
            handleSeek (sender, msg);
         when OP_CLOSE =>
            handleClose (sender, msg);
         when OP_READDIR =>
            handleReaddir (sender, msg);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end loop;
end main;
