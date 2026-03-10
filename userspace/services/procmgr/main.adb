------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Process Manager Service
--
--  Handles OP_SPAWN requests from userspace clients. Reads an ELF binary
--  from the filesystem service and calls SYSCALL_SPAWN to load it into a
--  new process.
--
--  Capability slots:
--    1 = CAP_ENDPOINT to filesystem server
--    4 = CAP_PROCESS with RIGHT_EXECUTE + RIGHT_GRANT
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

procedure main is
   use ASCII;

   --  IPC label constants
   OP_SPAWN   : constant Unsigned_32 := 16#0100#;
   OP_OPEN    : constant Unsigned_32 := 16#0001#;
   OP_READ    : constant Unsigned_32 := 16#0003#;
   OP_CLOSE   : constant Unsigned_32 := 16#0002#;
   REPLY_OK   : constant Unsigned_32 := 16#F000#;
   REPLY_ERR  : constant Unsigned_32 := 16#F001#;

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 256 * 4096;  -- 1 MiB

   --  ELF buffer — starts small, grows dynamically via sbrk when needed.
   INITIAL_BUF_PAGES : constant := 16;  --  64 KB starting size
   PAGE_SIZE         : constant := 4096;
   bufPages    : Natural := INITIAL_BUF_PAGES;
   bufCapacity : Natural := INITIAL_BUF_PAGES * PAGE_SIZE;

   --  FS capability slot
   CAP_SLOT_FS_LOCAL : constant Unsigned_64 := 1;

   --  FS server PID (for grant creation)
   fsPID : ProcessID := NO_PROCESS;

   --  ELF buffer (also serves as grant buffer to FS for zero-copy reads)
   elfBuf : System.Address := System.Null_Address;

   --  Grant to FS server covering elfBuf
   fsGrantId : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  printDec - print a small unsigned number in decimal
   ---------------------------------------------------------------------------
   procedure printDec (val : Unsigned_32) is
      buf : String (1 .. 10);
      pos : Natural := buf'Last;
      v   : Unsigned_32 := val;
   begin
      if v = 0 then
         debugPrint ("0");
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      debugPrint (buf (pos + 1 .. buf'Last));
   end printDec;

   ---------------------------------------------------------------------------
   --  sendReply - send a reply message to a waiting sender
   ---------------------------------------------------------------------------
   procedure sendReply
     (dest  : ProcessID;
      label : Unsigned_32;
      word0 : Unsigned_64)
   is
      replyMsg : Message := NULL_MESSAGE;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 1,
                       flags  => 0,
                       badge  => 0);
      replyMsg.words := (0 => word0, others => 0);
      ignore := reply (dest, replyMsg);
   end sendReply;

   ---------------------------------------------------------------------------
   --  ensureBuffer - grow elfBuf if needed for a file of the given size.
   --  Extends via sbrk (contiguous from previous heap end), then re-grants
   --  the enlarged buffer to the FS server.
   ---------------------------------------------------------------------------
   procedure ensureBuffer (needed : Unsigned_64) is
      newPages : Natural;
      extra    : Natural;
      ret      : Unsigned_64;
      ok       : Boolean;
   begin
      if needed <= Unsigned_64 (bufCapacity) then
         return;
      end if;

      --  Round up to page-aligned size
      newPages := Natural ((needed + Unsigned_64 (PAGE_SIZE) - 1) /
                           Unsigned_64 (PAGE_SIZE));
      extra := newPages - bufPages;

      debugPrint ("procmgr: growing buffer to ");
      printDec (Unsigned_32 (newPages));
      debugPrint (" pages" & LF);

      ret := syscall (SYSCALL_SBRK, Unsigned_64 (extra) *
                      Unsigned_64 (PAGE_SIZE));
      if ret = Unsigned_64'Last then
         debugPrint ("procmgr: sbrk grow failed" & LF);
         return;
      end if;

      --  Revoke old grant and create a larger one
      revokeGrant (fsGrantId);

      createGrant (
         grantee   => fsPID,
         localAddr => elfBuf,
         numPages  => newPages,
         readWrite => True,
         grantId   => fsGrantId,
         success   => ok);

      if not ok then
         debugPrint ("procmgr: re-grant failed" & LF);
         return;
      end if;

      bufPages    := newPages;
      bufCapacity := newPages * PAGE_SIZE;
   end ensureBuffer;

   ---------------------------------------------------------------------------
   --  readFileFromFS - open and read an entire file via FS IPC
   --  Reads into elfBuf. Uses ioBuf as the grant-backed I/O buffer.
   --  Returns number of bytes read, or 0 on failure.
   ---------------------------------------------------------------------------
   function readFileFromFS (name : String) return Unsigned_64
   is
      msg       : Message;
      tag       : MessageTag;
      handle    : Unsigned_64;
      fileSize  : Unsigned_64;
      totalRead : Unsigned_64 := 0;
      chunkRead : Unsigned_64;
   begin
      --  Write filename into elfBuf (grant buffer); overwritten by OP_READ
      declare
         grantBuf : array (0 .. name'Length - 1) of Unsigned_8 with
            Import, Address => elfBuf;
      begin
         for i in 0 .. name'Length - 1 loop
            grantBuf (i) := Unsigned_8 (
               Character'Pos (name (name'First + i)));
         end loop;
      end;

      --  OP_OPEN: words(0)=grant_id, words(1)=path_length, words(2)=flags
      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_OPEN,
                  length => 3,
                  flags  => 0,
                  badge  => 0);
      msg.words (0) := fsGrantId;
      msg.words (1) := Unsigned_64 (name'Length);
      msg.words (2) := 0;
      tag := capCall (CAP_SLOT_FS_LOCAL, msg);

      if tag.label /= REPLY_OK then
         debugPrint ("procmgr: OP_OPEN failed" & LF);
         return 0;
      end if;

      handle   := msg.words (0);
      fileSize := msg.words (1);

      --  Grow buffer if file is larger than current capacity
      if fileSize > 0 then
         ensureBuffer (fileSize);
      end if;

      --  Read entire file via grant (zero-copy). Loop handles partial reads.
      loop
         declare
            remaining : constant Unsigned_64 :=
               Unsigned_64 (bufCapacity) - totalRead;
         begin
            exit when remaining = 0;

            msg := NULL_MESSAGE;
            msg.tag := (label  => OP_READ,
                        length => 3,
                        flags  => 0,
                        badge  => 0);
            msg.words (0) := handle;
            msg.words (1) := fsGrantId;
            msg.words (2) := remaining;
            tag := capCall (CAP_SLOT_FS_LOCAL, msg);
         end;

         if tag.label /= REPLY_OK then
            debugPrint ("procmgr: OP_READ failed" & LF);
            exit;
         end if;

         chunkRead := msg.words (0);
         exit when chunkRead = 0;

         totalRead := totalRead + chunkRead;
      end loop;

      --  Close file handle
      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_CLOSE,
                  length => 1,
                  flags  => 0,
                  badge  => 0);
      msg.words (0) := handle;
      tag := capCall (CAP_SLOT_FS_LOCAL, msg);

      return totalRead;
   end readFileFromFS;

   ---------------------------------------------------------------------------
   --  Manifest constants
   ---------------------------------------------------------------------------
   MANIFEST_MAGIC   : constant Unsigned_32 := 16#43424954#;  -- "CBIT" LE

   --  Manifest request types
   REQ_FRAMEBUFFER  : constant Unsigned_8 := 1;
   REQ_SERVICE      : constant Unsigned_8 := 2;

   --  ELF section header type for PROGBITS
   SHT_PROGBITS     : constant Unsigned_32 := 1;

   --  Capability type position values (must match kernel CapabilityType enum)
   CAP_TYPE_ENDPOINT      : constant Unsigned_64 := 1;
   CAP_TYPE_NOTIFICATION  : constant Unsigned_64 := 2;
   CAP_TYPE_DEVICE_MEM    : constant Unsigned_64 := 7;

   --  Well-known slots for input focus capabilities
   CAP_SLOT_KBD_FOCUS     : constant Unsigned_64 := 13;
   CAP_SLOT_MOUSE_FOCUS   : constant Unsigned_64 := 16;

   ---------------------------------------------------------------------------
   --  readU16 - read a little-endian Unsigned_16 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU16 (offset : Unsigned_64) return Unsigned_16 is
      val : Unsigned_16 with
         Import, Address => elfBuf + Storage_Offset (offset);
   begin
      return val;
   end readU16;

   ---------------------------------------------------------------------------
   --  readU32 - read a little-endian Unsigned_32 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU32 (offset : Unsigned_64) return Unsigned_32 is
      val : Unsigned_32 with
         Import, Address => elfBuf + Storage_Offset (offset);
   begin
      return val;
   end readU32;

   ---------------------------------------------------------------------------
   --  readU64 - read a little-endian Unsigned_64 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU64 (offset : Unsigned_64) return Unsigned_64 is
      val : Unsigned_64 with
         Import, Address => elfBuf + Storage_Offset (offset);
   begin
      return val;
   end readU64;

   ---------------------------------------------------------------------------
   --  readU8 - read a Unsigned_8 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU8 (offset : Unsigned_64) return Unsigned_8 is
      val : Unsigned_8 with
         Import, Address => elfBuf + Storage_Offset (offset);
   begin
      return val;
   end readU8;

   ---------------------------------------------------------------------------
   --  parseAndGrantManifest
   --  Parse the .cubit.caps section from the ELF in elfBuf and mint
   --  capabilities into the child process via SYSCALL_MINT_CAP.
   ---------------------------------------------------------------------------
   procedure parseAndGrantManifest
     (childPID : Unsigned_64;
      elfSize  : Unsigned_64)
   is
      --  ELF64 header field offsets
      e_shoff_off     : constant := 40;  -- Section header table offset
      e_shentsize_off : constant := 58;  -- Section header entry size
      e_shnum_off     : constant := 60;  -- Number of section headers

      e_shoff     : Unsigned_64;
      e_shentsize : Unsigned_16;
      e_shnum     : Unsigned_16;
   begin
      if elfSize < 64 then
         return;
      end if;

      e_shoff     := readU64 (e_shoff_off);
      e_shentsize := readU16 (e_shentsize_off);
      e_shnum     := readU16 (e_shnum_off);

      --  Validate section header table
      if e_shentsize /= 64 then
         debugPrint ("procmgr: unexpected shentsize" & LF);
         return;
      end if;

      if e_shoff = 0 or e_shnum = 0 then
         return;
      end if;

      if e_shoff + Unsigned_64 (e_shnum) * 64 > elfSize then
         debugPrint ("procmgr: section headers beyond ELF" & LF);
         return;
      end if;

      --  Scan section headers for .cubit.caps (PROGBITS with magic match)
      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase  : constant Unsigned_64 :=
               e_shoff + Unsigned_64 (i) * 64;
            sh_type : constant Unsigned_32 := readU32 (shBase + 4);
            sh_offset : Unsigned_64;
            sh_size   : Unsigned_64;
         begin
            if sh_type = SHT_PROGBITS then
               sh_offset := readU64 (shBase + 24);
               sh_size   := readU64 (shBase + 32);

               --  Need at least 8 bytes for header (magic + version + count)
               if sh_size >= 8 and then
                  sh_offset + sh_size <= elfSize and then
                  readU32 (sh_offset) = MANIFEST_MAGIC
               then
                  --  Found manifest section
                  declare
                     version : constant Unsigned_16 :=
                        readU16 (sh_offset + 4);
                     count   : constant Unsigned_16 :=
                        readU16 (sh_offset + 6);
                     entryBase  : Unsigned_64;
                     reqType    : Unsigned_8;
                     rights     : Unsigned_8;
                     slotNum    : Unsigned_8;
                     param0     : Unsigned_32;
                     rightsMask : Unsigned_64;
                     ignore     : Unsigned_64;
                  begin
                     if version /= 1 then
                        debugPrint ("procmgr: unknown manifest v" & LF);
                        return;
                     end if;

                     --  Validate all entries fit
                     if sh_size < 8 + Unsigned_64 (count) * 16 then
                        debugPrint ("procmgr: manifest truncated" & LF);
                        return;
                     end if;

                     debugPrint ("procmgr: manifest has ");
                     printDec (Unsigned_32 (count));
                     debugPrint (" cap entries" & LF);

                     for j in 0 .. Unsigned_16'(count - 1) loop
                        entryBase := sh_offset + 8 +
                           Unsigned_64 (j) * 16;
                        reqType := readU8 (entryBase);
                        rights  := readU8 (entryBase + 1);
                        slotNum := readU8 (entryBase + 2);
                        param0  := readU32 (entryBase + 4);

                        rightsMask := Unsigned_64 (rights);

                        case reqType is
                           when REQ_FRAMEBUFFER =>
                              --  CAP_DEVICE_MEM, ref=0, param=0x1000_0000
                              ignore := syscall (
                                 SYSCALL_MINT_CAP,
                                 childPID,
                                 CAP_TYPE_DEVICE_MEM,
                                 0,
                                 16#1000_0000#,
                                 rightsMask,
                                 Unsigned_64 (slotNum));
                              debugPrint ("procmgr: minted FB cap" & LF);

                           when REQ_SERVICE =>
                              --  Look up driver PID via sysinfo,
                              --  retrying briefly if not yet registered.
                              declare
                                 driverPID : Unsigned_64 := 0;
                                 MAX_RETRIES : constant := 20;
                              begin
                                 for attempt in 1 .. MAX_RETRIES loop
                                    driverPID := getInfo (
                                       SYSINFO_REGISTERED_DRIVER,
                                       Unsigned_64 (param0));
                                    exit when driverPID /= 0;
                                    ignore := syscall (
                                       SYSCALL_SLEEP, 50);
                                 end loop;

                                 if driverPID /= 0 then
                                    ignore := syscall (
                                       SYSCALL_MINT_CAP,
                                       childPID,
                                       CAP_TYPE_ENDPOINT,
                                       driverPID,
                                       0,
                                       rightsMask,
                                       Unsigned_64 (slotNum));
                                    debugPrint (
                                       "procmgr: minted svc cap" & LF);
                                 else
                                    debugPrint (
                                       "procmgr: driver not found" & LF);
                                 end if;
                              end;

                           when others =>
                              debugPrint (
                                 "procmgr: unknown req type" & LF);
                        end case;
                     end loop;
                  end;

                  --  Only process first matching manifest section
                  return;
               end if;
            end if;
         end;
      end loop;
   end parseAndGrantManifest;

   ---------------------------------------------------------------------------
   --  spawnByName
   --  Read ELF from filesystem, spawn suspended, grant manifest caps, resume.
   --  Returns PID on success, 0 on failure.
   ---------------------------------------------------------------------------
   function spawnByName
     (name      : String;
      priority  : Unsigned_64;
      requester : Unsigned_64 := 0) return Unsigned_64
   is
      elfSize : Unsigned_64;
      newPID  : Unsigned_64;
      pri     : Unsigned_64 := priority;
      t0, t1  : Unsigned_64;
   begin
      debugPrint ("procmgr: spawn: ");
      debugPrint (name);
      debugPrint ("" & LF);

      t0 := syscall (SYSCALL_GETTIME);
      elfSize := readFileFromFS (name);
      t1 := syscall (SYSCALL_GETTIME);

      if elfSize = 0 then
         debugPrint ("procmgr: file read failed" & LF);
         return 0;
      end if;

      debugPrint ("procmgr: read ");
      printDec (Unsigned_32 (elfSize));
      debugPrint (" bytes in ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);

      if pri = 0 or pri > 10 then
         pri := 5;
      end if;

      t0 := syscall (SYSCALL_GETTIME);
      declare
         function toNum is new Ada.Unchecked_Conversion
            (System.Address, Unsigned_64);
      begin
         newPID := syscall (SYSCALL_SPAWN,
                            toNum (elfBuf),
                            elfSize,
                            pri,
                            SPAWN_SUSPENDED,
                            0,          -- arg4: auto-assign PID
                            requester); -- arg5: ppid = who asked for spawn
      end;
      t1 := syscall (SYSCALL_GETTIME);

      if newPID = Unsigned_64'Last then
         debugPrint ("procmgr: spawn syscall failed" & LF);
         return 0;
      end if;

      debugPrint ("procmgr: SYSCALL_SPAWN took ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);

      t0 := syscall (SYSCALL_GETTIME);
      parseAndGrantManifest (newPID, elfSize);
      t1 := syscall (SYSCALL_GETTIME);

      debugPrint ("procmgr: manifest took ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);

      --  Mint input focus capabilities (CAP_NOTIFICATION) so the child
      --  can call registerDriver(DRIVER_KEYBOARD) and registerDriver(DRIVER_MOUSE).
      declare
         ignore : Unsigned_64;
      begin
         ignore := syscall (SYSCALL_MINT_CAP,
                            newPID,
                            CAP_TYPE_NOTIFICATION,
                            DRIVER_KEYBOARD,  -- ref
                            0,                -- param
                            2,                -- rights = RIGHT_WRITE
                            CAP_SLOT_KBD_FOCUS);
         ignore := syscall (SYSCALL_MINT_CAP,
                            newPID,
                            CAP_TYPE_NOTIFICATION,
                            DRIVER_MOUSE,     -- ref
                            0,                -- param
                            2,                -- rights = RIGHT_WRITE
                            CAP_SLOT_MOUSE_FOCUS);
      end;

      declare
         ignore : Unsigned_64;
      begin
         ignore := syscall (SYSCALL_RESUME, newPID);
      end;

      debugPrint ("procmgr: resumed PID ");
      printDec (Unsigned_32 (newPID));
      debugPrint ("" & LF);

      return newPID;
   end spawnByName;

   ---------------------------------------------------------------------------
   --  handleSpawn
   --  Request: tag.label=OP_SPAWN, words(0)=grant_id (filename in grant buf),
   --           tag.length=filename length, words(1)=priority
   ---------------------------------------------------------------------------
   procedure handleSpawn (sender : ProcessID; msg : Message) is
      nameLen   : constant Natural := Natural (msg.tag.length);
      grantId   : constant Unsigned_64 := msg.words (0);
      priority  : constant Unsigned_64 := msg.words (1);
      grantAddr : constant System.Address :=
         To_Address (Integer_Address (
            GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE));
      newPID : Unsigned_64;
   begin
      if nameLen = 0 or nameLen > 255 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      declare
         name : String (1 .. nameLen) with
            Import, Address => grantAddr;
      begin
         newPID := spawnByName (name, priority, Unsigned_64 (sender));
      end;

      if newPID = 0 then
         sendReply (sender, REPLY_ERR, 0);
      else
         sendReply (sender, REPLY_OK, newPID);
      end if;
   end handleSpawn;

   ---------------------------------------------------------------------------
   --  processInitConf
   --  Read init.conf from the filesystem, parse entries, and spawn each.
   --  Format: one filename per line, optional "pri=N" suffix.
   --  Lines starting with '#' and blank lines are skipped.
   ---------------------------------------------------------------------------
   procedure processInitConf is
      MAX_ENTRIES   : constant := 16;
      MAX_NAME_LEN  : constant := 64;

      type InitEntry is record
         name     : String (1 .. MAX_NAME_LEN);
         nameLen  : Natural;
         priority : Unsigned_64;
      end record;

      entries   : array (0 .. MAX_ENTRIES - 1) of InitEntry;
      numEntries : Natural := 0;

      confSize : Unsigned_64;
   begin
      --  Device manager ensures disk drivers are ready before starting
      --  procmgr, so no retry needed.
      debugPrint ("procmgr: reading init.conf..." & LF);
      confSize := readFileFromFS ("init.conf");
      if confSize = 0 then
         debugPrint ("procmgr: init.conf not found, skipping" & LF);
         return;
      end if;

      --  Parse the config from elfBuf (readFileFromFS stores data there)
      --  We must parse all entries before spawning, because spawnByName
      --  overwrites elfBuf.
      declare
         conf : array (0 .. Natural (confSize) - 1) of Character with
            Import, Address => elfBuf;
         pos  : Natural := 0;
      begin
         while pos < Natural (confSize) and numEntries < MAX_ENTRIES loop
            --  Skip leading whitespace
            while pos < Natural (confSize) and then
                  (conf (pos) = ' ' or conf (pos) = ASCII.HT)
            loop
               pos := pos + 1;
            end loop;

            --  End of buffer?
            exit when pos >= Natural (confSize);

            --  Skip blank lines
            if conf (pos) = ASCII.LF or conf (pos) = ASCII.CR then
               pos := pos + 1;

            --  Skip comment lines
            elsif conf (pos) = '#' then
               while pos < Natural (confSize) and then
                     conf (pos) /= ASCII.LF
               loop
                  pos := pos + 1;
               end loop;

            else
               --  Parse filename (up to space or newline)
               declare
                  nameStart : constant Natural := pos;
                  nameEnd   : Natural := pos;
                  pri       : Unsigned_64 := 5;
               begin
                  while nameEnd < Natural (confSize) and then
                        conf (nameEnd) /= ' ' and then
                        conf (nameEnd) /= ASCII.HT and then
                        conf (nameEnd) /= ASCII.LF and then
                        conf (nameEnd) /= ASCII.CR
                  loop
                     nameEnd := nameEnd + 1;
                  end loop;

                  declare
                     nLen : constant Natural := nameEnd - nameStart;
                  begin
                     if nLen > 0 and nLen <= MAX_NAME_LEN then
                        entries (numEntries).nameLen := nLen;
                        for i in 0 .. nLen - 1 loop
                           entries (numEntries).name (i + 1) :=
                              conf (nameStart + i);
                        end loop;

                        --  Check for "pri=N" after filename
                        pos := nameEnd;
                        while pos < Natural (confSize) and then
                              (conf (pos) = ' ' or conf (pos) = ASCII.HT)
                        loop
                           pos := pos + 1;
                        end loop;

                        if pos + 3 < Natural (confSize) and then
                           conf (pos)     = 'p' and then
                           conf (pos + 1) = 'r' and then
                           conf (pos + 2) = 'i' and then
                           conf (pos + 3) = '='
                        then
                           pos := pos + 4;
                           if pos < Natural (confSize) and then
                              conf (pos) in '0' .. '9'
                           then
                              pri := Unsigned_64 (
                                 Character'Pos (conf (pos)) -
                                 Character'Pos ('0'));
                              pos := pos + 1;
                              --  Handle two-digit priority (e.g., "10")
                              if pos < Natural (confSize) and then
                                 conf (pos) in '0' .. '9'
                              then
                                 pri := pri * 10 + Unsigned_64 (
                                    Character'Pos (conf (pos)) -
                                    Character'Pos ('0'));
                                 pos := pos + 1;
                              end if;
                           end if;
                        end if;

                        entries (numEntries).priority := pri;
                        numEntries := numEntries + 1;
                     end if;
                  end;

                  --  Skip to end of line
                  while pos < Natural (confSize) and then
                        conf (pos) /= ASCII.LF
                  loop
                     pos := pos + 1;
                  end loop;
               end;
            end if;
         end loop;
      end;

      debugPrint ("procmgr: init.conf: ");
      printDec (Unsigned_32 (numEntries));
      debugPrint (" entries" & LF);

      --  Now spawn each entry (this overwrites elfBuf each time)
      for i in 0 .. numEntries - 1 loop
         declare
            n : String renames
               entries (i).name (1 .. entries (i).nameLen);
            pid : Unsigned_64;
         begin
            debugPrint ("procmgr: init spawn: ");
            debugPrint (n);
            debugPrint ("" & LF);
            pid := spawnByName (n, entries (i).priority);
            if pid = 0 then
               debugPrint ("procmgr: init spawn failed: ");
               debugPrint (n);
               debugPrint ("" & LF);
            end if;
         end;
      end loop;
   end processInitConf;

   ---------------------------------------------------------------------------
   --  Main variables
   ---------------------------------------------------------------------------
   sender : ProcessID;
   msg    : Message;

begin
   debugPrint ("procmgr: starting..." & LF);

   --  Register as DRIVER_PROCMGR
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_PROCMGR);
   end;

   debugPrint ("procmgr: registered as driver" & LF);

   --  Allocate initial ELF buffer via sbrk (grows dynamically as needed)
   declare
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_SBRK,
                      Unsigned_64 (INITIAL_BUF_PAGES) *
                      Unsigned_64 (PAGE_SIZE));
      if ret = Unsigned_64'Last then
         debugPrint ("procmgr: sbrk failed" & LF);
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_EXIT, 1);
         end;
         return;
      end if;
      elfBuf := To_Address (Integer_Address (ret));
   end;

   --  Grant elfBuf directly to FS server for zero-copy reads.
   --  FS is at well-known PID 10 (Config.SERVICE_FILESYSTEM_PID).
   fsPID := 10;

   declare
      ok : Boolean;
   begin
      createGrant (
         grantee   => fsPID,
         localAddr => elfBuf,
         numPages  => INITIAL_BUF_PAGES,
         readWrite => True,
         grantId   => fsGrantId,
         success   => ok);

      if not ok then
         debugPrint ("procmgr: createGrant to FS failed" & LF);
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_EXIT, 1);
         end;
         return;
      end if;
   end;

   --  Process init.conf to spawn Stage 2 programs
   processInitConf;

   debugPrint ("procmgr: ready, entering receive loop" & LF);

   --  Main service loop
   loop
      receive (sender, msg);

      case msg.tag.label is
         when OP_SPAWN =>
            handleSpawn (sender, msg);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end loop;
end main;
