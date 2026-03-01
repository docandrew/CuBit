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
   MAGIC_RAMDISK_ADDRESS : constant Unsigned_64 := 1000;

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 256 * 4096;  --  1 MiB

   --  Maximum open files and path length
   MAX_OPEN_FILES : constant := 32;
   MAX_PATH_LEN   : constant := 256;

   --  File handle entry
   type FileEntry is record
      active   : Boolean     := False;
      inodeNum : Unsigned_32 := 0;
      ino      : Ext2.Inode;
      offset   : Unsigned_64 := 0;
      ownerPID : ProcessID   := NO_PROCESS;
   end record;

   type FileTable is array (0 .. MAX_OPEN_FILES - 1) of FileEntry;
   files : FileTable;

   --  Filesystem context
   fs : Ext2.Filesystem;
   fsOk : Boolean;

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
   begin
      if pathLen = 0 or pathLen > MAX_PATH_LEN then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      --  Read path from grant buffer
      declare
         pathStr : String (1 .. Natural (pathLen))
           with Import, Address => toAddr (grantAddr);
      begin
         --  Resolve path to inode
         inodeNum := Ext2.resolvePath (fs, pathStr);
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

      --  Set up file entry
      files (handle).active   := True;
      files (handle).inodeNum := inodeNum;
      files (handle).offset   := 0;
      files (handle).ownerPID := sender;
      Ext2.readInode (fs, inodeNum, files (handle).ino);

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

      bytesRead := Ext2.readData
        (fs,
         files (handle).ino,
         files (handle).offset,
         toAddr (grantAddr),
         count);

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
   rdAddr := getInfo (MAGIC_RAMDISK_ADDRESS, 0);
   if rdAddr = 0 or rdAddr = Unsigned_64'Last then
      debugPrint ("FS Server: No ramdisk found, exiting." & LF);
      return;
   end if;

   debugPrint ("FS Server: Got ramdisk address, initializing Ext2..." & LF);

   --  Initialize Ext2 filesystem from ramdisk
   Ext2.init (fs, toAddr (rdAddr), fsOk);
   if not fsOk then
      debugPrint ("FS Server: Invalid Ext2 filesystem on ramdisk." & LF);
      return;
   end if;

   debugPrint ("FS Server: Ext2 initialized. Entering message loop." & LF);

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
