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
with Ext2;

procedure main is
   use ASCII;

   --  IPC operation labels (must match kernel/src/ipc_labels.ads)
   OP_OPEN    : constant Unsigned_32 := 16#0001#;
   OP_CLOSE   : constant Unsigned_32 := 16#0002#;
   OP_READ    : constant Unsigned_32 := 16#0003#;
   OP_SEEK    : constant Unsigned_32 := 16#0006#;
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

   --  File handle entry (tracks which backend each file uses)
   type FileEntry is record
      active   : Boolean            := False;
      inodeNum : Unsigned_32        := 0;
      ino      : Ext2.Inode;
      offset   : Unsigned_64        := 0;
      ownerPID : ProcessID          := NO_PROCESS;
      backend  : Ext2.BlockBackend  := Ext2.RAMDISK;
   end record;

   type FileTable is array (0 .. MAX_OPEN_FILES - 1) of FileEntry;
   files : FileTable;

   --  Ramdisk filesystem context
   fs : Ext2.Filesystem;
   fsOk : Boolean;

   --  ATA-backed filesystem context (lazy initialized)
   ataFs          : Ext2.Filesystem;
   ataInitialized : Boolean := False;

   --  ATA grant buffer (one page, allocated at startup via sbrk)
   ATA_GRANT_PAGES : constant := 1;
   ataGrantBuf     : System.Address := System.Null_Address;
   ataGrantId      : Unsigned_64 := 0;

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
   ataDriverPID : Unsigned_64 := 0;

   --  Page size for grant buffer allocation
   FS_PAGE_SIZE : constant := 4096;

   --  Check if path starts with "@ata:" prefix.
   --  Returns True and sets relStart to the index after the prefix.
   procedure parseScheme
     (pathStr  : String;
      isATA    : out Boolean;
      relStart : out Natural)
   is
   begin
      isATA    := False;
      relStart := pathStr'First;

      if pathStr'Length >= 5 and then
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
         begin
            raw := syscall (SYSCALL_SBRK, FS_PAGE_SIZE + FS_PAGE_SIZE);
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
         ataInitialized := True;
         debugPrint ("FS Server: ATA ext2 filesystem initialized." & LF);
      else
         debugPrint ("FS Server: ATA ext2 init failed (no ext2?)." & LF);
         revokeGrant (ataGrantId);
      end if;
   end ensureATA;

   --  Handle OP_OPEN
   --  words(0) = grant_id (where path string is)
   --  words(1) = path_length
   --  words(2) = flags (unused for now)
   procedure handleOpen (sender : ProcessID; msg : Message) is
      grantId   : constant Unsigned_64 := msg.words (0);
      pathLen   : constant Unsigned_64 := msg.words (1);
      grantAddr : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;

      handle   : Integer;
      inodeNum : Unsigned_32;
      isATA    : Boolean;
      relStart : Natural;
      useBackend : Ext2.BlockBackend;
   begin
      if pathLen = 0 or pathLen > MAX_PATH_LEN then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      --  Read path from grant buffer and parse scheme prefix
      declare
         pathStr : String (1 .. Natural (pathLen))
           with Import, Address => toAddr (grantAddr);
      begin
         parseScheme (pathStr, isATA, relStart);

         if isATA then
            useBackend := Ext2.ATA;

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
            --  Format: @ata:0/path or @ata:path
            declare
               relPath : String renames
                 pathStr (relStart .. Natural (pathLen));
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
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;

               inodeNum := Ext2.resolvePath
                 (ataFs, relPath (skipIdx .. relPath'Last));
            end;
         else
            useBackend := Ext2.RAMDISK;
            if not fsOk then
               sendReply (sender, REPLY_ERR, Unsigned_64'Last);
               return;
            end if;
            inodeNum := Ext2.resolvePath (fs, pathStr);
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
      files (handle).active   := True;
      files (handle).inodeNum := inodeNum;
      files (handle).offset   := 0;
      files (handle).ownerPID := sender;
      files (handle).backend  := useBackend;

      case useBackend is
         when Ext2.RAMDISK =>
            Ext2.readInode (fs, inodeNum, files (handle).ino);
         when Ext2.ATA =>
            Ext2.readInode (ataFs, inodeNum, files (handle).ino);
      end case;

      sendReply (sender, REPLY_OK, Unsigned_64 (handle));
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
         when Ext2.RAMDISK =>
            bytesRead := Ext2.readData
              (fs,
               files (handle).ino,
               files (handle).offset,
               toAddr (grantAddr),
               count);
         when Ext2.ATA =>
            bytesRead := Ext2.readData
              (ataFs,
               files (handle).ino,
               files (handle).offset,
               toAddr (grantAddr),
               count);
      end case;

      files (handle).offset := files (handle).offset + bytesRead;
      sendReply (sender, REPLY_OK, bytesRead);
   end handleRead;

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

      size := Ext2.fileSize (files (handle).ino);
      --  fileSize is computed from the inode which is already cached,
      --  so no backend dispatch needed here.

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

   --  Main message loop variables
   sender : ProcessID;
   msg    : Message;
   rdAddr : Unsigned_64;
begin
   debugPrint ("FS Server: Starting..." & LF);

   --  Get ramdisk address from kernel
   rdAddr := getInfo (SYSINFO_RAMDISK_ADDRESS);
   if rdAddr = 0 or rdAddr = Unsigned_64'Last then
      debugPrint ("FS Server: No ramdisk found, ATA-only mode." & LF);
      fsOk := False;
   else
      debugPrint ("FS Server: Got ramdisk address, initializing Ext2..." & LF);

      --  Initialize Ext2 filesystem from ramdisk
      Ext2.init (fs, toAddr (rdAddr), fsOk);
      if not fsOk then
         debugPrint ("FS Server: Invalid Ext2 filesystem on ramdisk." & LF);
      end if;
   end if;

   debugPrint ("FS Server: Entering message loop." & LF);

   --  Main IPC message loop
   loop
      receive (sender, msg);

      case msg.tag.label is
         when OP_OPEN =>
            handleOpen (sender, msg);
         when OP_READ =>
            handleRead (sender, msg);
         when OP_SEEK =>
            handleSeek (sender, msg);
         when OP_CLOSE =>
            handleClose (sender, msg);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end loop;
end main;
