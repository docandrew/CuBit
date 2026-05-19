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
   REPLY_OK     : constant Unsigned_32 := 16#F000#;
   REPLY_ERR    : constant Unsigned_32 := 16#F001#;
   OP_SET_ACL   : constant Unsigned_32 := 16#0080#;

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096; -- 16 MiB

   --  ELF buffer — starts small, grows dynamically via sbrk when needed.
   INITIAL_BUF_PAGES : constant := 16;  --  64 KB starting size
   PAGE_SIZE         : constant := 4096;
   bufPages    : Natural := INITIAL_BUF_PAGES;
   bufCapacity : Natural := INITIAL_BUF_PAGES * PAGE_SIZE;

   --  FS capability slot
   CAP_SLOT_FS_LOCAL     : constant Unsigned_64 := 1;
   CAP_SLOT_CONFIG_LOCAL : constant Unsigned_64 := 2;

   --  Service routing identifiers
   SERVICE_FS     : constant Unsigned_8 := 0;
   SERVICE_CONFIG : constant Unsigned_8 := 1;

   --  ELF buffer (also serves as grant buffer to FS for zero-copy reads)
   elfBuf : System.Address := System.Null_Address;

   --  Grant to FS server covering elfBuf
   fsGrantId : Unsigned_64 := 0;

   --  Grant to config service covering elfBuf
   configGrantId : Unsigned_64 := 0;

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

      createGrantViaCap (
         slot      => CAP_SLOT_FS_LOCAL,
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
   ID_MAGIC         : constant Unsigned_32 := 16#44494243#;  -- "CBID" LE
   STREAMS_MAGIC    : constant Unsigned_32 := 16#54534243#;  -- "CBST" LE

   --  Manifest request types
   REQ_FRAMEBUFFER  : constant Unsigned_8 := 1;
   REQ_SERVICE      : constant Unsigned_8 := 2;
   REQ_IOPORT       : constant Unsigned_8 := 3;
   REQ_STREAM       : constant Unsigned_8 := 8;
   REQ_RESOURCE     : constant Unsigned_8 := 9;

   --  Stream notification label
   OP_STREAM_AVAILABLE : constant Unsigned_32 := 16#0706#;

   --  Config store IPC label
   OP_CONFIG_GET : constant Unsigned_32 := 16#0600#;

   --  Well-known slot for config-based resource quota
   RESOURCE_CAP_SLOT : constant Unsigned_64 := 62;

   --  ELF section header type for PROGBITS
   SHT_PROGBITS     : constant Unsigned_32 := 1;

   --  Capability type position values (must match kernel CapabilityType enum)
   CAP_TYPE_ENDPOINT      : constant Unsigned_64 := 1;
   CAP_TYPE_NOTIFICATION  : constant Unsigned_64 := 2;
   CAP_TYPE_IOPORT        : constant Unsigned_64 := 4;
   CAP_TYPE_PROCESS       : constant Unsigned_64 := 6;
   CAP_TYPE_DEVICE_MEM    : constant Unsigned_64 := 7;
   CAP_TYPE_RESOURCE      : constant Unsigned_64 := 9;

   --  Well-known slots for input focus capabilities
   CAP_SLOT_KBD_FOCUS     : constant Unsigned_64 := 13;
   CAP_SLOT_MOUSE_FOCUS   : constant Unsigned_64 := 16;
   CAP_SLOT_PROCESS_MGMT  : constant Unsigned_64 := 5;
   CAP_SLOT_SERVICE_REG   : constant Unsigned_64 := 7;

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
   --  parseIdSection
   --  Parse the .cubit.id section from the ELF in elfBuf. Searches for
   --  the "id" key and copies its value into pkgId.
   ---------------------------------------------------------------------------
   procedure parseIdSection
     (elfSize  : Unsigned_64;
      pkgId    : out String;
      pkgIdLen : out Natural)
   is
      e_shoff     : Unsigned_64;
      e_shnum     : Unsigned_16;
   begin
      pkgIdLen := 0;

      if elfSize < 64 then
         return;
      end if;

      e_shoff := readU64 (40);
      e_shnum := readU16 (60);

      if readU16 (58) /= 64 or e_shoff = 0 or e_shnum = 0 then
         return;
      end if;

      if e_shoff + Unsigned_64 (e_shnum) * 64 > elfSize then
         return;
      end if;

      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 :=
               e_shoff + Unsigned_64 (i) * 64;
            sh_type   : constant Unsigned_32 := readU32 (shBase + 4);
            sh_offset : Unsigned_64;
            sh_size   : Unsigned_64;
         begin
            if sh_type = SHT_PROGBITS then
               sh_offset := readU64 (shBase + 24);
               sh_size   := readU64 (shBase + 32);

               if sh_size >= 8 and then
                  sh_offset + sh_size <= elfSize and then
                  readU32 (sh_offset) = ID_MAGIC
               then
                  declare
                     version : constant Unsigned_16 :=
                        readU16 (sh_offset + 4);
                     count   : constant Unsigned_16 :=
                        readU16 (sh_offset + 6);
                     pos     : Unsigned_64 := sh_offset + 8;
                  begin
                     if version /= 1 then
                        return;
                     end if;

                     for j in 0 .. Unsigned_16'(count - 1) loop
                        exit when pos + 3 > sh_offset + sh_size;

                        declare
                           kLen : constant Natural :=
                              Natural (readU8 (pos));
                           vLen : constant Natural :=
                              Natural (readU16 (pos + 1));
                        begin
                           pos := pos + 3;

                           exit when pos + Unsigned_64 (kLen) +
                                     Unsigned_64 (vLen) >
                                     sh_offset + sh_size;

                           --  Check if key is "id" (2 bytes)
                           if kLen = 2 and then
                              readU8 (pos) =
                                 Unsigned_8 (Character'Pos ('i'))
                              and then
                              readU8 (pos + 1) =
                                 Unsigned_8 (Character'Pos ('d'))
                           then
                              pkgIdLen := Natural'Min (
                                 vLen, pkgId'Length);
                              for c in 0 .. pkgIdLen - 1 loop
                                 pkgId (pkgId'First + c) :=
                                    Character'Val (Natural (readU8 (
                                       pos + Unsigned_64 (kLen) +
                                       Unsigned_64 (c))));
                              end loop;

                              debugPrint ("procmgr: pkg id=");
                              debugPrint (
                                 pkgId (pkgId'First ..
                                    pkgId'First + pkgIdLen - 1));
                              debugPrint ("" & LF);
                              return;
                           end if;

                           pos := pos + Unsigned_64 (kLen) +
                                  Unsigned_64 (vLen);
                        end;
                     end loop;

                     return;  -- only process first matching section
                  end;
               end if;
            end if;
         end;
      end loop;
   end parseIdSection;

   ---------------------------------------------------------------------------
   --  parseStreamsSection
   --  Parse the .cubit.streams section from the ELF in elfBuf. Builds a
   --  64-bit bitmask where bit N means stream ID N is declared.
   ---------------------------------------------------------------------------
   procedure parseStreamsSection
     (elfSize       : Unsigned_64;
      streamBitmask : out Unsigned_64)
   is
      e_shoff : Unsigned_64;
      e_shnum : Unsigned_16;
   begin
      streamBitmask := 0;

      if elfSize < 64 then
         return;
      end if;

      e_shoff := readU64 (40);
      e_shnum := readU16 (60);

      if readU16 (58) /= 64 or e_shoff = 0 or e_shnum = 0 then
         return;
      end if;

      if e_shoff + Unsigned_64 (e_shnum) * 64 > elfSize then
         return;
      end if;

      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 :=
               e_shoff + Unsigned_64 (i) * 64;
            sh_type   : constant Unsigned_32 := readU32 (shBase + 4);
            sh_offset : Unsigned_64;
            sh_size   : Unsigned_64;
         begin
            if sh_type = SHT_PROGBITS then
               sh_offset := readU64 (shBase + 24);
               sh_size   := readU64 (shBase + 32);

               if sh_size >= 8 and then
                  sh_offset + sh_size <= elfSize and then
                  readU32 (sh_offset) = STREAMS_MAGIC
               then
                  declare
                     version : constant Unsigned_16 :=
                        readU16 (sh_offset + 4);
                     count   : constant Unsigned_16 :=
                        readU16 (sh_offset + 6);
                  begin
                     if version /= 1 then
                        return;
                     end if;

                     if sh_size < 8 + Unsigned_64 (count) * 8 then
                        return;
                     end if;

                     for j in 0 .. Unsigned_16'(count - 1) loop
                        declare
                           entBase  : constant Unsigned_64 :=
                              sh_offset + 8 + Unsigned_64 (j) * 8;
                           streamId : constant Unsigned_16 :=
                              readU16 (entBase);
                        begin
                           if Unsigned_64 (streamId) < 64 then
                              streamBitmask := streamBitmask or
                                 Shift_Left (Unsigned_64'(1),
                                    Natural (streamId));
                           end if;
                           debugPrint ("procmgr: stream decl id=");
                           printDec (Unsigned_32 (streamId));
                           debugPrint ("" & LF);
                        end;
                     end loop;

                     return;
                  end;
               end if;
            end if;
         end;
      end loop;
   end parseStreamsSection;

   ---------------------------------------------------------------------------
   --  parseAndGrantManifest
   --  Parse the .cubit.caps section from the ELF in elfBuf and mint
   --  capabilities into the child process via SYSCALL_MINT_CAP.
   ---------------------------------------------------------------------------
   procedure parseAndGrantManifest
     (childPID      : Unsigned_64;
      elfSize       : Unsigned_64;
      streamBitmask : in out Unsigned_64)
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
                     entryBase     : Unsigned_64;
                     reqType       : Unsigned_8;
                     rights        : Unsigned_8;
                     slotNum       : Unsigned_8;
                     param0        : Unsigned_32;
                     param1        : Unsigned_64;
                     rightsMask    : Unsigned_64;
                     ignore        : Unsigned_64;
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
                        param1  := readU64 (entryBase + 8);

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

                           when REQ_IOPORT =>
                              --  CAP_IOPORT. param0 is the base I/O port.
                              --  param1 low 16 bits optionally gives the port
                              --  count; older manifests leave it zero and get
                              --  the original one-port grant.
                              if param1 = 0 then
                                 param1 := 1;
                              end if;
                              debugPrint ("procmgr: i/o port request base=");
                              printDec (param0 and 16#FFFF#);
                              debugPrint (" count=");
                              printDec (Unsigned_32 (param1 and 16#FFFF#));
                              debugPrint (" slot=");
                              printDec (Unsigned_32 (slotNum));
                              debugPrint ("" & LF);
                              ignore := syscall (
                                 SYSCALL_MINT_CAP,
                                 childPID,
                                 CAP_TYPE_IOPORT,
                                 Unsigned_64 (param0 and 16#FFFF#),
                                 param1 and 16#FFFF#,
                                 rightsMask,
                                 Unsigned_64 (slotNum));
                              if ignore = Unsigned_64'Last then
                                 debugPrint ("procmgr: i/o port cap mint failed" & LF);
                              else
                                 debugPrint ("procmgr: minted i/o port cap" & LF);
                              end if;

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

                           when REQ_STREAM =>
                              --  Fallback: only use .cubit.caps stream
                              --  entries if no .cubit.streams section found
                              if streamBitmask = 0 then
                                 declare
                                    sid : constant Unsigned_64 :=
                                       Unsigned_64 (
                                          param0 and 16#FFFF#);
                                 begin
                                    if sid < 64 then
                                       streamBitmask :=
                                          streamBitmask or
                                          Shift_Left (
                                             Unsigned_64'(1),
                                             Natural (sid));
                                    end if;
                                    debugPrint (
                                       "procmgr: stream id=");
                                    printDec (Unsigned_32 (sid));
                                    debugPrint ("" & LF);
                                 end;
                              end if;

                           when REQ_RESOURCE =>
                              --  param0 = maxFrames
                              --  param1 = cpuQuotaUs(lo32)|cpuPeriodUs(hi32)
                              declare
                                 param1 : constant Unsigned_64 :=
                                    readU64 (entryBase + 8);
                              begin
                                 ignore := syscall (
                                    SYSCALL_MINT_CAP,
                                    childPID,
                                    CAP_TYPE_RESOURCE,
                                    Unsigned_64 (param0),
                                    param1,
                                    rightsMask,
                                    Unsigned_64 (slotNum));
                                 debugPrint (
                                    "procmgr: minted resource cap" &
                                    LF);
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
   --  Access manifest constants
   ---------------------------------------------------------------------------
   ACCESS_MAGIC : constant Unsigned_32 := 16#43434143#;  -- "CACC" LE
   MAX_ACL      : constant := 16;

   SANDBOX_NONE       : constant Unsigned_8 := 0;
   SANDBOX_RUN_FOLDER : constant Unsigned_8 := 1;
   SANDBOX_APP_FOLDER : constant Unsigned_8 := 2;

   ---------------------------------------------------------------------------
   --  parseAndSendACL
   --  Parse .cubit.access section from the ELF in elfBuf and send
   --  OP_SET_ACL to the FS server. Falls back to wildcard if no section.
   --  sandboxOverride / cwd / binaryName enable RUN_FOLDER / APP_FOLDER
   --  prefix rewriting for wildcard FS entries.
   ---------------------------------------------------------------------------
   procedure parseAndSendACL
     (childPID        : Unsigned_64;
      elfSize         : Unsigned_64;
      sandboxOverride : Unsigned_8 := SANDBOX_NONE;
      cwd             : String := "";
      binaryName      : String := "")
   is
      e_shoff     : Unsigned_64;
      e_shentsize : Unsigned_16;
      e_shnum     : Unsigned_16;
   begin
      if elfSize < 64 then
         goto Send_Wildcard;
      end if;

      e_shoff     := readU64 (40);
      e_shentsize := readU16 (58);
      e_shnum     := readU16 (60);

      if e_shentsize /= 64 or e_shoff = 0 or e_shnum = 0 then
         goto Send_Wildcard;
      end if;

      if e_shoff + Unsigned_64 (e_shnum) * 64 > elfSize then
         goto Send_Wildcard;
      end if;

      --  Scan section headers for .cubit.access (PROGBITS with CACC magic)
      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 :=
               e_shoff + Unsigned_64 (i) * 64;
            sh_type   : constant Unsigned_32 := readU32 (shBase + 4);
            sh_offset : Unsigned_64;
            sh_size   : Unsigned_64;
         begin
            if sh_type = SHT_PROGBITS then
               sh_offset := readU64 (shBase + 24);
               sh_size   := readU64 (shBase + 32);

               --  Need at least 16 bytes for access header
               if sh_size >= 16 and then
                  sh_offset + sh_size <= elfSize and then
                  readU32 (sh_offset) = ACCESS_MAGIC
               then
                  declare
                     version : constant Unsigned_16 :=
                        readU16 (sh_offset + 4);
                     count   : constant Unsigned_16 :=
                        readU16 (sh_offset + 6);
                  begin
                     if version /= 1 then
                        debugPrint ("procmgr: unknown access v" & LF);
                        goto Send_Wildcard;
                     end if;

                     if count = 0 or count > MAX_ACL then
                        goto Send_Wildcard;
                     end if;

                     --  Validate entries fit: 16 + 80*count
                     if sh_size < 16 + Unsigned_64 (count) * 80 then
                        debugPrint ("procmgr: access truncated" & LF);
                        goto Send_Wildcard;
                     end if;

                     debugPrint ("procmgr: access has ");
                     printDec (Unsigned_32 (count));
                     debugPrint (" ACL entries" & LF);

                     --  Read sandbox mode from header byte 13
                     declare
                        manifestSandbox : Unsigned_8 :=
                           readU8 (sh_offset + 13);
                        effectiveSandbox : Unsigned_8;
                        sandboxPrefix    : String (1 .. 64) :=
                           (others => ' ');
                        sandboxPrefixLen : Natural := 0;
                     begin
                        --  Clamp to valid range
                        if manifestSandbox > SANDBOX_APP_FOLDER then
                           manifestSandbox := SANDBOX_NONE;
                        end if;

                        --  Most restrictive wins (higher = more
                        --  restrictive)
                        if sandboxOverride > manifestSandbox then
                           effectiveSandbox := sandboxOverride;
                        else
                           effectiveSandbox := manifestSandbox;
                        end if;

                        --  Compute sandbox prefix
                        if effectiveSandbox = SANDBOX_RUN_FOLDER then
                           --  Use cwd from spawner
                           sandboxPrefixLen := cwd'Length;
                           if sandboxPrefixLen > 64 then
                              sandboxPrefixLen := 64;
                           end if;
                           for c in 0 .. sandboxPrefixLen - 1 loop
                              sandboxPrefix (1 + c) :=
                                 cwd (cwd'First + c);
                           end loop;
                        elsif effectiveSandbox = SANDBOX_APP_FOLDER
                        then
                           --  dirname(binaryName): find last '/'
                           declare
                              lastSlash : Natural := 0;
                           begin
                              for c in 0 .. binaryName'Length - 1 loop
                                 if binaryName (
                                    binaryName'First + c) = '/'
                                 then
                                    lastSlash := c + 1;
                                 end if;
                              end loop;
                              sandboxPrefixLen := lastSlash;
                              if sandboxPrefixLen > 64 then
                                 sandboxPrefixLen := 64;
                              end if;
                              for c in 0 ..
                                 sandboxPrefixLen - 1
                              loop
                                 sandboxPrefix (1 + c) :=
                                    binaryName (
                                       binaryName'First + c);
                              end loop;
                           end;
                        end if;

                        if effectiveSandbox /= SANDBOX_NONE
                           and sandboxPrefixLen > 0
                        then
                           debugPrint ("procmgr: sandbox prefix=");
                           debugPrint (
                              sandboxPrefix (1 .. sandboxPrefixLen));
                           debugPrint ("" & LF);
                        end if;

                     --  Copy entries to stack-local array before
                     --  overwriting elfBuf with grant format.
                     --  Per entry: 1 byte rights + 1 byte prefixLen +
                     --  1 byte service + 64 bytes prefix.
                     declare
                        type LocalEntry is record
                           rights    : Unsigned_8;
                           prefixLen : Unsigned_8;
                           service   : Unsigned_8;
                           prefix    : String (1 .. 64);
                        end record;
                        locals : array (0 .. Natural (count) - 1)
                           of LocalEntry;
                        entBase : Unsigned_64;
                        pLen    : Natural;
                        fsCount     : Natural := 0;
                        configCount : Natural := 0;
                     begin
                        for j in 0 .. Natural (count) - 1 loop
                           entBase := sh_offset + 16 +
                              Unsigned_64 (j) * 80;
                           locals (j).rights :=
                              readU8 (entBase);
                           locals (j).prefixLen :=
                              readU8 (entBase + 1);
                           locals (j).service :=
                              readU8 (entBase + 2);
                           pLen := Natural (locals (j).prefixLen);
                           if pLen > 64 then
                              pLen := 64;
                              locals (j).prefixLen := 64;
                           end if;
                           for c in 0 .. pLen - 1 loop
                              locals (j).prefix (1 + c) :=
                                 Character'Val (
                                    Natural (readU8 (entBase + 8 +
                                       Unsigned_64 (c))));
                           end loop;
                        end loop;

                        --  Sandbox rewrite: set prefix on wildcard FS
                        --  entries (prefixLen=0, service=FS)
                        if effectiveSandbox /= SANDBOX_NONE
                           and sandboxPrefixLen > 0
                        then
                           for j in 0 .. Natural (count) - 1 loop
                              if locals (j).service = SERVICE_FS
                                 and locals (j).prefixLen = 0
                              then
                                 locals (j).prefixLen :=
                                    Unsigned_8 (sandboxPrefixLen);
                                 for c in 0 ..
                                    sandboxPrefixLen - 1
                                 loop
                                    locals (j).prefix (1 + c) :=
                                       sandboxPrefix (1 + c);
                                 end loop;
                              end if;
                           end loop;
                        end if;

                        --  Count FS vs CONFIG entries
                        for j in 0 .. Natural (count) - 1 loop
                           if locals (j).service = SERVICE_CONFIG then
                              configCount := configCount + 1;
                           else
                              fsCount := fsCount + 1;
                           end if;
                        end loop;

                        --  Pass 1: Write FS entries to elfBuf and send
                        if fsCount > 0 then
                           declare
                              grantBuf : array (0 .. fsCount * 72 - 1)
                                 of Unsigned_8 with
                                 Import, Address => elfBuf;
                              base : Natural;
                              idx  : Natural := 0;
                           begin
                              for b in grantBuf'Range loop
                                 grantBuf (b) := 0;
                              end loop;

                              for j in 0 .. Natural (count) - 1 loop
                                 if locals (j).service /= SERVICE_CONFIG
                                 then
                                    base := idx * 72;
                                    grantBuf (base) := locals (j).rights;
                                    grantBuf (base + 1) :=
                                       locals (j).prefixLen;
                                    pLen :=
                                       Natural (locals (j).prefixLen);
                                    for c in 0 .. pLen - 1 loop
                                       grantBuf (base + 8 + c) :=
                                          Unsigned_8 (Character'Pos (
                                             locals (j).prefix (1 + c)));
                                    end loop;
                                    idx := idx + 1;
                                 end if;
                              end loop;
                           end;

                           declare
                              aclMsg : Message := NULL_MESSAGE;
                              aclTag : MessageTag;
                           begin
                              aclMsg.tag := (label  => OP_SET_ACL,
                                             length => 4,
                                             flags  => 0,
                                             badge  => 0);
                              aclMsg.words := (0 => childPID,
                                               1 => Unsigned_64 (fsCount),
                                               2 => fsGrantId,
                                               3 => 0);
                              aclTag := capCall (
                                 CAP_SLOT_FS_LOCAL, aclMsg);
                           end;
                        end if;

                        --  Pass 2: Write CONFIG entries and send
                        if configCount > 0 and configGrantId /= 0 then
                           declare
                              grantBuf : array
                                 (0 .. configCount * 72 - 1)
                                 of Unsigned_8 with
                                 Import, Address => elfBuf;
                              base : Natural;
                              idx  : Natural := 0;
                           begin
                              for b in grantBuf'Range loop
                                 grantBuf (b) := 0;
                              end loop;

                              for j in 0 .. Natural (count) - 1 loop
                                 if locals (j).service = SERVICE_CONFIG
                                 then
                                    base := idx * 72;
                                    grantBuf (base) := locals (j).rights;
                                    grantBuf (base + 1) :=
                                       locals (j).prefixLen;
                                    pLen :=
                                       Natural (locals (j).prefixLen);
                                    for c in 0 .. pLen - 1 loop
                                       grantBuf (base + 8 + c) :=
                                          Unsigned_8 (Character'Pos (
                                             locals (j).prefix (1 + c)));
                                    end loop;
                                    idx := idx + 1;
                                 end if;
                              end loop;
                           end;

                           declare
                              aclMsg : Message := NULL_MESSAGE;
                              aclTag : MessageTag;
                           begin
                              aclMsg.tag := (label  => OP_SET_ACL,
                                             length => 4,
                                             flags  => 0,
                                             badge  => 0);
                              aclMsg.words := (
                                 0 => childPID,
                                 1 => Unsigned_64 (configCount),
                                 2 => configGrantId,
                                 3 => 0);
                              aclTag := capCall (
                                 CAP_SLOT_CONFIG_LOCAL, aclMsg);
                           end;
                        end if;

                        return;  -- done, skip wildcard
                     end;
                     end;  -- sandbox declare
                  end;
               end if;
            end if;
         end;
      end loop;

   <<Send_Wildcard>>
      --  No .cubit.access section: deny-by-default.
      --  FS server denies access for processes with no ACL profile,
      --  so we simply don't send OP_SET_ACL.
      debugPrint ("procmgr: no .cubit.access, deny-by-default" & LF);
   end parseAndSendACL;

   ---------------------------------------------------------------------------
   --  queryConfigQuota
   --  Query the config store for resource quotas keyed by package ID.
   --  Uses configGrantId (which maps elfBuf). Safe to call after all ELF
   --  section parsing is done, since this overwrites elfBuf.
   ---------------------------------------------------------------------------
   procedure queryConfigQuota
     (pkgId       : String;
      maxFrames   : out Unsigned_32;
      cpuQuotaUs  : out Unsigned_32;
      cpuPeriodUs : out Unsigned_32)
   is
      procedure queryOne (suffix : String; result : out Unsigned_32) is
         prefix   : constant String := "resource/";
         totalLen : constant Natural :=
            prefix'Length + pkgId'Length + 1 + suffix'Length;
         cfgMsg   : Message;
      begin
         result := 0;

         if configGrantId = 0 or totalLen > PAGE_SIZE then
            return;
         end if;

         --  Write key to elfBuf (grant buffer for config)
         declare
            keyBuf : array (0 .. totalLen - 1) of Unsigned_8 with
               Import, Address => elfBuf;
            pos : Natural := 0;
         begin
            for c in prefix'Range loop
               keyBuf (pos) :=
                  Unsigned_8 (Character'Pos (prefix (c)));
               pos := pos + 1;
            end loop;
            for c in pkgId'Range loop
               keyBuf (pos) :=
                  Unsigned_8 (Character'Pos (pkgId (c)));
               pos := pos + 1;
            end loop;
            keyBuf (pos) := Unsigned_8 (Character'Pos ('/'));
            pos := pos + 1;
            for c in suffix'Range loop
               keyBuf (pos) :=
                  Unsigned_8 (Character'Pos (suffix (c)));
               pos := pos + 1;
            end loop;
         end;

         cfgMsg := NULL_MESSAGE;
         cfgMsg.tag := (label  => OP_CONFIG_GET,
                        length => 2,
                        flags  => 0,
                        badge  => 0);
         cfgMsg.words (0) := configGrantId;
         cfgMsg.words (1) := Unsigned_64 (totalLen);
         cfgMsg.tag := capCall (CAP_SLOT_CONFIG_LOCAL, cfgMsg);

         if cfgMsg.tag.label /= REPLY_OK then
            return;
         end if;

         --  Parse decimal value from grant buffer
         declare
            valLen : constant Natural :=
               Natural (cfgMsg.words (0));
         begin
            if valLen > 0 and valLen <= PAGE_SIZE then
               declare
                  valBuf : array (0 .. valLen - 1) of Unsigned_8
                     with Import, Address => elfBuf;
                  acc : Unsigned_32 := 0;
                  ch  : Unsigned_8;
               begin
                  for v in 0 .. valLen - 1 loop
                     ch := valBuf (v);
                     exit when ch < Unsigned_8 (Character'Pos ('0'))
                        or ch > Unsigned_8 (Character'Pos ('9'));
                     acc := acc * 10 +
                        Unsigned_32 (ch -
                           Unsigned_8 (Character'Pos ('0')));
                  end loop;
                  result := acc;
               end;
            end if;
         end;
      end queryOne;
   begin
      queryOne ("max-frames", maxFrames);
      queryOne ("cpu-quota-us", cpuQuotaUs);
      queryOne ("cpu-period-us", cpuPeriodUs);
   end queryConfigQuota;

   ---------------------------------------------------------------------------
   --  spawnByName
   --  Read ELF from filesystem, spawn suspended, grant manifest caps, resume.
   --  Returns PID on success, 0 on failure.
   ---------------------------------------------------------------------------
   function spawnByName
     (name        : String;
      priority    : Unsigned_64;
      requester   : Unsigned_64 := 0;
      sandboxMode : Unsigned_8 := SANDBOX_NONE;
      cwd         : String := "") return Unsigned_64
   is
      elfSize       : Unsigned_64;
      newPID        : Unsigned_64;
      pri           : Unsigned_64 := priority;
      t0, t1        : Unsigned_64;
      pkgId         : String (1 .. 128);
      pkgIdLen      : Natural := 0;
      streamBitmask : Unsigned_64 := 0;
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
         --  NUL-terminated copy of name for kernel to read
         nameBuf : String (1 .. 17) := (others => Character'Val (0));
         nameLen : Natural := name'Length;
      begin
         if nameLen > 16 then
            nameLen := 16;
         end if;
         for i in 0 .. nameLen - 1 loop
            nameBuf (i + 1) := name (name'First + i);
         end loop;

         newPID := syscall (SYSCALL_SPAWN,
                            toNum (elfBuf),
                            elfSize,
                            pri,
                            toNum (nameBuf'Address),
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

      --  Parse .cubit.id section for package identity
      parseIdSection (elfSize, pkgId, pkgIdLen);

      --  Parse .cubit.streams section for stream declarations
      parseStreamsSection (elfSize, streamBitmask);

      --  Parse .cubit.caps manifest (streams fallback if no .cubit.streams)
      t0 := syscall (SYSCALL_GETTIME);
      parseAndGrantManifest (newPID, elfSize, streamBitmask);
      t1 := syscall (SYSCALL_GETTIME);

      debugPrint ("procmgr: manifest took ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);

      --  Mint input focus capabilities (CAP_NOTIFICATION) so the child
      --  can call registerDriver(DRIVER_KEYBOARD) and
      --  registerDriver(DRIVER_MOUSE).
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
         --  Mint CAP_PROCESS(ref=0 wildcard, RIGHT_READ+RIGHT_WRITE)
         --  so spawned apps can kill child processes.
         ignore := syscall (SYSCALL_MINT_CAP,
                            newPID,
                            CAP_TYPE_PROCESS,
                            0,                -- ref = 0 (wildcard)
                            0,                -- param
                            3,                -- rights = READ+WRITE
                            CAP_SLOT_PROCESS_MGMT);
      end;

      --  Mint CAP_NOTIFICATION for logstore driver registration
      if pkgIdLen = 18 then
         declare
            LOGSTORE_ID : constant String := "com.cubit.logstore";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            for c in 0 .. 17 loop
               if pkgId (1 + c) /= LOGSTORE_ID (1 + c) then
                  match := False;
                  exit;
               end if;
            end loop;
            if match then
               ignore := syscall (SYSCALL_MINT_CAP,
                                  newPID,
                                  CAP_TYPE_NOTIFICATION,
                                  DRIVER_LOGSTORE,  -- ref = 13
                                  0,                -- param
                                  2,                -- rights = RIGHT_WRITE
                                  7);               -- slot 7
               debugPrint ("procmgr: minted logstore ntf cap" & LF);
            end if;
         end;
      end if;

      --  Mint CAP_NOTIFICATION for desktop.svc registration.
      --  The desktop endpoint is intentionally not granted to every process:
      --  only the process whose package identity is com.cubit.desktop should
      --  be able to claim DRIVER_DESKTOP via registerDriver().
      if pkgIdLen = 17 then
         declare
            DESKTOP_ID : constant String := "com.cubit.desktop";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            for c in 0 .. 16 loop
               if pkgId (1 + c) /= DESKTOP_ID (1 + c) then
                  match := False;
                  exit;
               end if;
            end loop;
            if match then
               ignore := syscall (SYSCALL_MINT_CAP,
                                  newPID,
                                  CAP_TYPE_NOTIFICATION,
                                  DRIVER_DESKTOP,
                                  0,
                                  2,
                                  CAP_SLOT_SERVICE_REG);
               debugPrint ("procmgr: minted desktop ntf cap" & LF);
            end if;
         end;
      end if;

      --  Mint CAP_NOTIFICATION for display.svc registration. display.svc is
      --  the sole userspace owner of scanout framebuffer authority.
      if pkgIdLen = 17 then
         declare
            DISPLAY_ID : constant String := "com.cubit.display";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            for c in 0 .. 16 loop
               if pkgId (1 + c) /= DISPLAY_ID (1 + c) then
                  match := False;
                  exit;
               end if;
            end loop;
            if match then
               ignore := syscall (SYSCALL_MINT_CAP,
                                  newPID,
                                  CAP_TYPE_NOTIFICATION,
                                  DRIVER_DISPLAY,
                                  0,
                                  2,
                                  CAP_SLOT_SERVICE_REG);
               debugPrint ("procmgr: minted display ntf cap" & LF);
            end if;
         end;
      end if;

      --  Mint CAP_NOTIFICATION for the headless IPC regression server.
      if pkgIdLen = 24 then
         declare
            IPCTEST_SERVER_ID : constant String :=
               "com.cubit.ipctest.server";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            for c in 0 .. 23 loop
               if pkgId (1 + c) /= IPCTEST_SERVER_ID (1 + c) then
                  match := False;
                  exit;
               end if;
            end loop;
            if match then
               ignore := syscall (SYSCALL_MINT_CAP,
                                  newPID,
                                  CAP_TYPE_NOTIFICATION,
                                  DRIVER_IPCTEST,
                                  0,
                                  2,
                                  7);
               debugPrint ("procmgr: minted ipctest ntf cap" & LF);
            end if;
         end;
      end if;

      --  Parse .cubit.access and send ACLs to FS server (or wildcard)
      parseAndSendACL (newPID, elfSize,
                       sandboxMode, cwd, name);

      --  Query config store for resource quotas (overwrites elfBuf)
      declare
         maxFrames   : Unsigned_32 := 0;
         cpuQuotaUs  : Unsigned_32 := 0;
         cpuPeriodUs : Unsigned_32 := 0;
         ignore      : Unsigned_64;
      begin
         if pkgIdLen > 0 then
            queryConfigQuota (
               pkgId (1 .. pkgIdLen),
               maxFrames, cpuQuotaUs, cpuPeriodUs);
         else
            queryConfigQuota (
               name, maxFrames, cpuQuotaUs, cpuPeriodUs);
         end if;

         if maxFrames /= 0 or cpuQuotaUs /= 0 then
            declare
               param1 : constant Unsigned_64 :=
                  Unsigned_64 (cpuQuotaUs) or
                  Shift_Left (Unsigned_64 (cpuPeriodUs), 32);
            begin
               ignore := syscall (SYSCALL_MINT_CAP,
                  newPID,
                  CAP_TYPE_RESOURCE,
                  Unsigned_64 (maxFrames),
                  param1,
                  1,   -- rights = RIGHT_READ
                  RESOURCE_CAP_SLOT);
               debugPrint (
                  "procmgr: minted config resource cap" & LF);
            end;
         end if;
      end;

      declare
         ignore : Unsigned_64;
      begin
         ignore := syscall (SYSCALL_RESUME, newPID);
      end;

      debugPrint ("procmgr: resumed PID ");
      printDec (Unsigned_32 (newPID));
      debugPrint ("" & LF);

      --  Notify requester and logstore about declared streams (after resume)
      if streamBitmask /= 0 then
         declare
            evMsg : Message := NULL_MESSAGE;
         begin
            evMsg.tag := (
               label  => OP_STREAM_AVAILABLE,
               length => 2,
               flags  => 0,
               badge  => 0);
            evMsg.words (0) := newPID;
            evMsg.words (1) := streamBitmask;

            if requester /= 0 then
               sendEvent (ProcessID (requester), evMsg);
               debugPrint ("procmgr: sent stream available" & LF);
            end if;

            --  Also notify log store (if registered) via submit
            --  (sendEvent requires CAP_ENDPOINT; submit does not)
            declare
               logPID : constant Unsigned_64 :=
                  getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_LOGSTORE);
               ignore : Boolean;
            begin
               if logPID /= 0 and logPID /= Unsigned_64'Last then
                  ignore := submit (ProcessID (logPID), evMsg,
                                    Unsigned_64'Last);
                  debugPrint ("procmgr: notified logstore" & LF);
               end if;
            end;
         end;
      end if;

      return newPID;
   end spawnByName;

   ---------------------------------------------------------------------------
   --  handleSpawn
   --  Request: tag.label=OP_SPAWN, words(0)=grant_id (filename in grant buf),
   --           tag.length=filename length, words(1)=priority,
   --           words(2)=spawnFlags (low 2 bits = sandbox override),
   --           words(3)=cwdLen (cwd bytes follow filename in grant buf)
   ---------------------------------------------------------------------------
   procedure handleSpawn (sender : ProcessID; msg : Message) is
      nameLen     : constant Natural := Natural (msg.tag.length);
      grantId     : constant Unsigned_64 := msg.words (0);
      priority    : constant Unsigned_64 := msg.words (1);
      spawnFlags  : constant Unsigned_64 := msg.words (2);
      cwdLen      : Natural := Natural (msg.words (3) and 16#FF#);
      sandboxMode : constant Unsigned_8 :=
         Unsigned_8 (spawnFlags and 16#03#);
      grantAddr   : constant System.Address :=
         To_Address (Integer_Address (
            GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE));
      newPID : Unsigned_64;
   begin
      if nameLen = 0 or nameLen > 255 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      --  Clamp cwdLen to fit in grant buffer after filename
      if cwdLen > 128 then
         cwdLen := 128;
      end if;

      declare
         name : String (1 .. nameLen) with
            Import, Address => grantAddr;
      begin
         if cwdLen > 0 then
            declare
               cwdAddr : constant System.Address :=
                  To_Address (Integer_Address (
                     GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE +
                     Unsigned_64 (nameLen)));
               cwdStr : String (1 .. cwdLen) with
                  Import, Address => cwdAddr;
            begin
               newPID := spawnByName (name, priority,
                                      Unsigned_64 (sender),
                                      sandboxMode, cwdStr);
            end;
         else
            newPID := spawnByName (name, priority,
                                   Unsigned_64 (sender),
                                   sandboxMode);
         end if;
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
      --  Retry init.conf read: the ATA driver may still be registering
      --  when procmgr starts, so the FS server might not have a disk
      --  backend ready yet.
      debugPrint ("procmgr: reading init.conf..." & LF);
      for attempt in 1 .. 10 loop
         confSize := readFileFromFS ("init.conf");
         exit when confSize > 0;
         if attempt < 10 then
            declare
               ignore : Unsigned_64;
            begin
               ignore := syscall (SYSCALL_SLEEP, 100);
            end;
         end if;
      end loop;
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

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      ignore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, badge => 0),
          capBadge => 0,
          words    => (others => 0)));
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
   --  Use createGrantViaCap with our FS endpoint cap (slot 1) to avoid
   --  hardcoding the FS server PID.
   declare
      ok : Boolean;
   begin
      createGrantViaCap (
         slot      => CAP_SLOT_FS_LOCAL,
         localAddr => elfBuf,
         numPages  => INITIAL_BUF_PAGES,
         readWrite => True,
         grantId   => fsGrantId,
         success   => ok);

      if not ok then
         debugPrint ("procmgr: createGrantViaCap to FS failed" & LF);
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_EXIT, 1);
         end;
         return;
      end if;
   end;

   --  Create grant to config service (if present) for ACL delivery.
   --  Config PID discovered via sysinfo; grant covers same elfBuf.
   declare
      configPID : Unsigned_64;
      ok : Boolean;
   begin
      configPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_CONFIG);
      if configPID /= 0 and configPID /= Unsigned_64'Last then
         createGrant (
            grantee   => configPID,
            localAddr => elfBuf,
            numPages  => INITIAL_BUF_PAGES,
            readWrite => True,
            grantId   => configGrantId,
            success   => ok);

         if not ok then
            debugPrint ("procmgr: config grant failed" & LF);
            configGrantId := 0;
         end if;
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
