------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Log Store Service
--
--  Subscribes to LOG streams (stream ID 4) from spawned processes and
--  stores entries in a circular FIFO buffer. Clients can query recent
--  log entries via IPC.
--
--  Discovery: registers as DRIVER_LOGSTORE (13) via sysinfo.
--  Procmgr sends OP_STREAM_AVAILABLE events when processes are spawned.
--  Logstore checks for the LOG bit (bit 4) and subscribes only to LOG
--  streams.
--
--  No capabilities required -- uses raw PID-based IPC + sysinfo discovery.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Streams;

procedure main is
   use ASCII;

   --  IPC operation labels
   OP_LOG_QUERY : constant Unsigned_32 := 16#0800#;
   OP_LOG_CLEAR : constant Unsigned_32 := 16#0801#;
   REPLY_OK     : constant Unsigned_32 := 16#F000#;
   REPLY_ERR    : constant Unsigned_32 := 16#F001#;

   --  Stream bitmask: LOG stream is ID 4 => bit 4
   LOG_BIT : constant Unsigned_64 := 16#10#;

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 256 * 4096;  --  1 MiB

   PAGE_SIZE : constant := 4096;

   ---------------------------------------------------------------------------
   --  Producer tracking (active stream subscriptions)
   ---------------------------------------------------------------------------

   MAX_PRODUCERS : constant := 16;

   type ProducerInfo is record
      active     : Boolean     := False;
      pendingSub : Boolean     := False;  --  awaiting subscribe retry
      pid        : Unsigned_64 := 0;
      grantBase  : Unsigned_64 := 0;
      cursor     : CuBit.Streams.CursorSlot := 0;
      cursorIdx  : Unsigned_32 := 0;
      capacity   : Unsigned_32 := 0;
   end record;

   producers : array (0 .. MAX_PRODUCERS - 1) of ProducerInfo;

   --  Pending subscribe tokens: token = 100 + producer slot index
   TOKEN_BASE : constant Unsigned_64 := 100;

   ---------------------------------------------------------------------------
   --  FIFO buffer (circular, allocated via sbrk)
   ---------------------------------------------------------------------------

   FIFO_PAGES : constant := 32;
   FIFO_SIZE  : constant := FIFO_PAGES * PAGE_SIZE;  --  128 KB

   type FifoArray is array (0 .. FIFO_SIZE - 1) of Unsigned_8;
   type FifoPtr is access all FifoArray;

   function toFifoPtr is new Ada.Unchecked_Conversion
     (System.Address, FifoPtr);

   fifo     : FifoPtr := null;
   fifoHead : Natural := 0;  --  next write position
   fifoTail : Natural := 0;  --  oldest entry start
   fifoUsed : Natural := 0;  --  bytes currently used
   fifoCount : Natural := 0; --  number of entries

   --  FIFO entry format:
   --    [totalLen:U16][pid:U16][timestamp:U32][data bytes]
   --  totalLen includes the 8-byte header.
   FIFO_ENTRY_HEADER : constant := 8;

   --  Query response buffer (1 page, owned by logstore, granted to callers)
   queryBuf  : System.Address := System.Null_Address;

   --  Track previous query grant so we can revoke it at start of next query
   --  (not immediately after reply, to give the caller time to read).
   lastQueryGrant : Unsigned_64 := Unsigned_64'Last;

   ---------------------------------------------------------------------------
   --  Conversion helpers
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  printDec - print a small unsigned number in decimal (serial debug)
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
      word0 : Unsigned_64;
      word1 : Unsigned_64 := 0)
   is
      replyMsg : Message := NULL_MESSAGE;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 2,
                       flags  => 0,
                       badge  => 0);
      replyMsg.words := (0 => word0, 1 => word1, others => 0);
      ignore := reply (dest, replyMsg);
   end sendReply;

   ---------------------------------------------------------------------------
   --  FIFO helpers
   ---------------------------------------------------------------------------

   --  Write a byte to the FIFO at the head position and advance
   procedure fifoPut (b : Unsigned_8) is
   begin
      fifo (fifoHead) := b;
      fifoHead := (fifoHead + 1) mod FIFO_SIZE;
      fifoUsed := fifoUsed + 1;
   end fifoPut;

   --  Read a U16 from fifo at absolute position
   function fifoReadU16 (pos : Natural) return Unsigned_16 is
      lo : constant Unsigned_8 := fifo (pos mod FIFO_SIZE);
      hi : constant Unsigned_8 := fifo ((pos + 1) mod FIFO_SIZE);
   begin
      return Unsigned_16 (lo) or Shift_Left (Unsigned_16 (hi), 8);
   end fifoReadU16;

   --  Read a U32 from fifo at absolute position
   function fifoReadU32 (pos : Natural) return Unsigned_32 is
      b0 : constant Unsigned_8 := fifo (pos mod FIFO_SIZE);
      b1 : constant Unsigned_8 := fifo ((pos + 1) mod FIFO_SIZE);
      b2 : constant Unsigned_8 := fifo ((pos + 2) mod FIFO_SIZE);
      b3 : constant Unsigned_8 := fifo ((pos + 3) mod FIFO_SIZE);
   begin
      return Unsigned_32 (b0) or
             Shift_Left (Unsigned_32 (b1), 8) or
             Shift_Left (Unsigned_32 (b2), 16) or
             Shift_Left (Unsigned_32 (b3), 24);
   end fifoReadU32;

   --  Discard the oldest FIFO entry (advance tail)
   procedure fifoDiscardOldest is
      entLen : Unsigned_16;
   begin
      if fifoCount = 0 then
         return;
      end if;
      entLen := fifoReadU16 (fifoTail);
      if Natural (entLen) > fifoUsed or entLen < FIFO_ENTRY_HEADER then
         --  Corrupt; reset
         fifoHead := 0;
         fifoTail := 0;
         fifoUsed := 0;
         fifoCount := 0;
         return;
      end if;
      fifoTail := (fifoTail + Natural (entLen)) mod FIFO_SIZE;
      fifoUsed := fifoUsed - Natural (entLen);
      fifoCount := fifoCount - 1;
   end fifoDiscardOldest;

   --  Append a log entry to the FIFO
   procedure appendToFifo
     (pid       : Unsigned_16;
      timestamp : Unsigned_32;
      data      : System.Address;
      dataLen   : Natural)
   is
      totalLen : constant Natural := FIFO_ENTRY_HEADER + dataLen;
      tl16     : constant Unsigned_16 := Unsigned_16 (totalLen);
      type ByteArr is array (0 .. dataLen - 1) of Unsigned_8
        with Convention => C;
      src : ByteArr with Import, Address => data;
   begin
      if totalLen > FIFO_SIZE then
         return;  --  entry too large
      end if;

      --  Evict oldest entries to make room
      while fifoUsed + totalLen > FIFO_SIZE loop
         fifoDiscardOldest;
         exit when fifoCount = 0;
      end loop;

      --  Write header: totalLen (U16 LE)
      fifoPut (Unsigned_8 (tl16 and 16#FF#));
      fifoPut (Unsigned_8 (Shift_Right (tl16, 8) and 16#FF#));

      --  pid (U16 LE)
      fifoPut (Unsigned_8 (pid and 16#FF#));
      fifoPut (Unsigned_8 (Shift_Right (pid, 8) and 16#FF#));

      --  timestamp (U32 LE)
      fifoPut (Unsigned_8 (timestamp and 16#FF#));
      fifoPut (Unsigned_8 (Shift_Right (timestamp, 8) and 16#FF#));
      fifoPut (Unsigned_8 (Shift_Right (timestamp, 16) and 16#FF#));
      fifoPut (Unsigned_8 (Shift_Right (timestamp, 24) and 16#FF#));

      --  data bytes
      for i in 0 .. dataLen - 1 loop
         fifoPut (src (i));
      end loop;

      fifoCount := fifoCount + 1;
   end appendToFifo;

   ---------------------------------------------------------------------------
   --  handleQuery
   --  Request: words(0) = max entries, words(1) = filter PID (0 = all)
   --  Logstore creates a temporary grant to the sender, writes entries into
   --  it, replies with the grant ID, then revokes the grant.
   --  Grant buffer layout:
   --    [count:U32][pad:U32] header (8 bytes)
   --    then packed [pid:U16][dataLen:U16][timestamp:U32][data] entries
   --  Reply: words(0) = entries written, words(1) = total in FIFO,
   --         words(2) = grant ID (so caller can read from grant region)
   ---------------------------------------------------------------------------
   procedure handleQuery (sender : ProcessID; msg : Message) is
      maxEntries : constant Natural := Natural (msg.words (0) and 16#FFFF#);
      filterPID : constant Unsigned_16 :=
        Unsigned_16 (msg.words (1) and 16#FFFF#);

      gid     : Unsigned_64;
      ok      : Boolean;

      writeOff : Natural := 8;  --  skip count header
      written  : Natural := 0;
      pos      : Natural := fifoTail;
      remaining : Natural := fifoCount;
      grantMax  : constant Natural := PAGE_SIZE;
   begin
      --  Revoke the previous query grant (from a prior call)
      if lastQueryGrant /= Unsigned_64'Last then
         revokeGrant (lastQueryGrant);
         lastQueryGrant := Unsigned_64'Last;
      end if;

      if maxEntries = 0 then
         sendReply (sender, REPLY_OK, 0, Unsigned_64 (fifoCount));
         return;
      end if;

      --  Create a temporary grant to the sender (uses reply cap in slot 63)
      createGrant (
         grantee   => sender,
         localAddr => queryBuf,
         numPages  => 1,
         readWrite => False,
         grantId   => gid,
         success   => ok);

      if not ok then
         debugPrint ("logstore: grant to sender failed" & LF);
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      --  Write entries into our local queryBuf
      declare
         outBuf : array (0 .. grantMax - 1) of Unsigned_8
           with Import, Address => queryBuf;
      begin
         --  Scan FIFO entries from oldest to newest
         while remaining > 0 and written < maxEntries loop
            declare
               entLen : constant Unsigned_16 := fifoReadU16 (pos);
               entPID : constant Unsigned_16 :=
                 fifoReadU16 ((pos + 2) mod FIFO_SIZE);
               entTS  : constant Unsigned_32 :=
                 fifoReadU32 ((pos + 4) mod FIFO_SIZE);
               dataLen : Natural;
            begin
               if Natural (entLen) < FIFO_ENTRY_HEADER then
                  exit;  --  corrupt
               end if;

               dataLen := Natural (entLen) - FIFO_ENTRY_HEADER;

               --  Apply PID filter
               if filterPID = 0 or entPID = filterPID then
                  declare
                     outSize : constant Natural := 8 + dataLen;
                  begin
                     if writeOff + outSize > grantMax then
                        exit;  --  buffer full
                     end if;

                     --  pid (U16 LE)
                     outBuf (writeOff) :=
                       Unsigned_8 (entPID and 16#FF#);
                     outBuf (writeOff + 1) :=
                       Unsigned_8 (Shift_Right (entPID, 8) and 16#FF#);
                     --  dataLen (U16 LE)
                     declare
                        dl16 : constant Unsigned_16 :=
                          Unsigned_16 (dataLen);
                     begin
                        outBuf (writeOff + 2) :=
                          Unsigned_8 (dl16 and 16#FF#);
                        outBuf (writeOff + 3) :=
                          Unsigned_8 (Shift_Right (dl16, 8) and 16#FF#);
                     end;
                     --  timestamp (U32 LE)
                     outBuf (writeOff + 4) :=
                       Unsigned_8 (entTS and 16#FF#);
                     outBuf (writeOff + 5) :=
                       Unsigned_8 (Shift_Right (entTS, 8) and 16#FF#);
                     outBuf (writeOff + 6) :=
                       Unsigned_8 (Shift_Right (entTS, 16) and 16#FF#);
                     outBuf (writeOff + 7) :=
                       Unsigned_8 (Shift_Right (entTS, 24) and 16#FF#);
                     --  data bytes
                     for i in 0 .. dataLen - 1 loop
                        outBuf (writeOff + 8 + i) := fifo (
                           (pos + FIFO_ENTRY_HEADER + i) mod FIFO_SIZE);
                     end loop;

                     writeOff := writeOff + outSize;
                     written := written + 1;
                  end;
               end if;

               pos := (pos + Natural (entLen)) mod FIFO_SIZE;
               remaining := remaining - 1;
            end;
         end loop;

         --  Write count header at offset 0
         declare
            cnt32 : constant Unsigned_32 := Unsigned_32 (written);
         begin
            outBuf (0) := Unsigned_8 (cnt32 and 16#FF#);
            outBuf (1) :=
              Unsigned_8 (Shift_Right (cnt32, 8) and 16#FF#);
            outBuf (2) :=
              Unsigned_8 (Shift_Right (cnt32, 16) and 16#FF#);
            outBuf (3) :=
              Unsigned_8 (Shift_Right (cnt32, 24) and 16#FF#);
            outBuf (4) := 0;
            outBuf (5) := 0;
            outBuf (6) := 0;
            outBuf (7) := 0;
         end;
      end;

      --  Reply with grant ID so caller can read from grant region
      declare
         replyMsg : Message := NULL_MESSAGE;
         ignore   : Unsigned_64;
      begin
         replyMsg.tag := (label  => REPLY_OK,
                          length => 3,
                          flags  => 0,
                          badge  => 0);
         replyMsg.words (0) := Unsigned_64 (written);
         replyMsg.words (1) := Unsigned_64 (fifoCount);
         replyMsg.words (2) := gid;
         ignore := reply (sender, replyMsg);
      end;

      --  Defer revocation to next query so caller has time to read
      lastQueryGrant := gid;
   end handleQuery;

   ---------------------------------------------------------------------------
   --  handleClear - clear the FIFO buffer
   ---------------------------------------------------------------------------
   procedure handleClear (sender : ProcessID) is
   begin
      fifoHead := 0;
      fifoTail := 0;
      fifoUsed := 0;
      fifoCount := 0;
      debugPrint ("logstore: FIFO cleared" & LF);
      sendReply (sender, REPLY_OK, 0);
   end handleClear;

   ---------------------------------------------------------------------------
   --  findFreeProducer - find a free producer slot
   ---------------------------------------------------------------------------
   function findFreeProducer return Integer is
   begin
      for i in producers'Range loop
         if not producers (i).active then
            return i;
         end if;
      end loop;
      return -1;
   end findFreeProducer;

   ---------------------------------------------------------------------------
   --  handleStreamAvail - process OP_STREAM_AVAILABLE from procmgr
   ---------------------------------------------------------------------------
   procedure handleStreamAvail (m : Message) is
      prodPID : constant Unsigned_64 := m.words (0);
      bitmask : constant Unsigned_64 := m.words (1);
      slot    : Integer;
      subMsg  : Message := NULL_MESSAGE;
      ok      : Boolean;
   begin
      debugPrint ("logstore: avail pid=");
      printDec (Unsigned_32 (prodPID));
      debugPrint (" mask=");
      printDec (Unsigned_32 (bitmask));
      debugPrint ("" & LF);

      if (bitmask and LOG_BIT) = 0 then
         return;
      end if;

      --  TEMPORARY: skip subscribe to debug DOOM crash
      debugPrint ("logstore: would subscribe (disabled)" & LF);
      return;

      slot := findFreeProducer;
      if slot < 0 then
         debugPrint ("logstore: no free slots" & LF);
         return;
      end if;

      subMsg.tag := (
         label  => CuBit.Streams.OP_STREAM_SUBSCRIBE,
         length => 1,
         flags  => 0,
         badge  => 0);
      subMsg.words (0) := 16#0004#;

      producers (slot).pid := prodPID;
      ok := submit (
         ProcessID (prodPID), subMsg,
         TOKEN_BASE + Unsigned_64 (slot));

      if ok then
         debugPrint ("logstore: subscribe sent pid=");
         printDec (Unsigned_32 (prodPID));
         debugPrint ("" & LF);
      else
         producers (slot).pendingSub := True;
         debugPrint ("logstore: subscribe deferred pid=");
         printDec (Unsigned_32 (prodPID));
         debugPrint ("" & LF);
      end if;
   end handleStreamAvail;

   ---------------------------------------------------------------------------
   --  Stream read buffer (shared across all producer drain)
   ---------------------------------------------------------------------------
   streamBuf : array (0 .. 511) of Unsigned_8;

   ---------------------------------------------------------------------------
   --  drainProducers - poll all active producer ring buffers into FIFO
   ---------------------------------------------------------------------------
   procedure drainProducers is
      timestamp : constant Unsigned_32 := Unsigned_32 (
        syscall (SYSCALL_GETTIME) and 16#FFFF_FFFF#);
   begin
      for i in producers'Range loop
         if producers (i).active then
            declare
               sub : CuBit.Streams.SubInfo := (
                  active    => True,
                  grantBase => producers (i).grantBase,
                  cursor    => producers (i).cursor,
                  cursorIdx => producers (i).cursorIdx,
                  capacity  => producers (i).capacity);
               tt  : CuBit.Streams.TypeTag;
               n   : Unsigned_32;
            begin
               loop
                  n := CuBit.Streams.streamRead (
                     sub, streamBuf'Address, 512, tt);
                  exit when n = 0;

                  appendToFifo (
                     pid       => Unsigned_16 (
                        producers (i).pid and 16#FFFF#),
                     timestamp => timestamp,
                     data      => streamBuf'Address,
                     dataLen   => Natural (n));
               end loop;

               --  Update cursor state
               producers (i).cursor    := sub.cursor;
               producers (i).cursorIdx := sub.cursorIdx;
            end;
         end if;
      end loop;
   end drainProducers;

   ---------------------------------------------------------------------------
   --  Main loop variables
   ---------------------------------------------------------------------------
   sender   : ProcessID;
   msg      : Message;
   found    : Boolean;
   eventMsg : Message;
begin
   debugPrint ("logstore: starting..." & LF);

   --  Register as DRIVER_LOGSTORE
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_LOGSTORE);
   end;

   --  Allocate FIFO buffer + query buffer via sbrk
   declare
      rawAddr : Unsigned_64;
      aligned : Unsigned_64;
   begin
      --  FIFO: 32 pages + 1 for alignment slack
      rawAddr := syscall (SYSCALL_SBRK, Unsigned_64 (FIFO_PAGES + 1) *
                          PAGE_SIZE);
      if rawAddr = Unsigned_64'Last then
         debugPrint ("logstore: sbrk failed for FIFO" & LF);
         return;
      end if;
      aligned := (rawAddr + PAGE_SIZE - 1) and
                 not Unsigned_64 (PAGE_SIZE - 1);
      fifo := toFifoPtr (To_Address (Integer_Address (aligned)));

      --  Query response buffer: 1 page + 1 for alignment slack
      rawAddr := syscall (SYSCALL_SBRK, 2 * PAGE_SIZE);
      if rawAddr = Unsigned_64'Last then
         debugPrint ("logstore: sbrk failed for query buf" & LF);
         return;
      end if;
      aligned := (rawAddr + PAGE_SIZE - 1) and
                 not Unsigned_64 (PAGE_SIZE - 1);
      queryBuf := To_Address (Integer_Address (aligned));
   end;

   debugPrint ("logstore: FIFO allocated, entering main loop" & LF);

   --  Main polling loop
   loop
      --  1. Handle service requests and stream subscription traffic.
      --     Poll_Service_Request is deliberately narrow: it will not consume
      --     event-lane traffic while logstore is checking for client work.
      Poll_Service_Request (sender, msg, found);
      if found then
         case msg.tag.label is
            when OP_LOG_QUERY =>
               handleQuery (sender, msg);
            when OP_LOG_CLEAR =>
               handleClear (sender);
            when CuBit.Streams.OP_STREAM_AVAILABLE =>
               --  Stream notification from procmgr (via submit)
               handleStreamAvail (msg);
            when others =>
               sendReply (sender, REPLY_ERR, 0);
         end case;
      end if;

      --  2. Drain event ring. Events are unsolicited notifications from
      --     producers and supervisors, not request/reply work.
      found := Poll_Event (eventMsg);

      --  2b. Retry pending subscribes (target mailbox was full earlier)
      for i in producers'Range loop
         if producers (i).pendingSub then
            declare
               subMsg : constant Message := (
                  tag => (
                     label  => CuBit.Streams.OP_STREAM_SUBSCRIBE,
                     length => 1,
                     flags  => 0,
                     badge  => 0),
                  capBadge => 0,
                  words    => (0 => 16#0004#, others => 0));
               ok : Boolean;
            begin
               ok := submit (
                  ProcessID (producers (i).pid),
                  subMsg,
                  TOKEN_BASE + Unsigned_64 (i));
               if ok then
                  producers (i).pendingSub := False;
                  debugPrint ("logstore: subscribe sent pid=");
                  printDec (Unsigned_32 (producers (i).pid));
                  debugPrint ("" & LF);
               end if;
            end;
         end if;
      end loop;

      --  3. Check subscription completions
      declare
         comp : CompletionEntry;
         ret  : Unsigned_64;
      begin
         ret := Poll_Completion (comp'Address);
         if ret = 1 then
            if comp.token >= TOKEN_BASE and then
               comp.token < TOKEN_BASE + Unsigned_64 (MAX_PRODUCERS)
            then
               declare
                  slot : constant Natural :=
                    Natural (comp.token - TOKEN_BASE);
               begin
                  if comp.msg.tag.label = REPLY_OK then
                     producers (slot).active := True;
                     producers (slot).grantBase :=
                       GRANT_REGION_BASE +
                       comp.msg.words (0) * GRANT_SLOT_SIZE;
                     producers (slot).cursor :=
                       CuBit.Streams.CursorSlot (comp.msg.words (1));
                     producers (slot).cursorIdx :=
                       Unsigned_32 (comp.msg.words (3));
                     producers (slot).capacity :=
                       Unsigned_32 (comp.msg.words (2));

                     debugPrint ("logstore: subscribed pid=");
                     printDec (Unsigned_32 (producers (slot).pid));
                     debugPrint ("" & LF);
                  else
                     debugPrint ("logstore: subscribe rejected" & LF);
                     producers (slot).pid := 0;
                  end if;
               end;
            end if;
         end if;
      end;

      --  4. Drain all active producer ring buffers into FIFO
      drainProducers;

      --  5. Brief sleep (10 ms)
      declare
         ignore : Unsigned_64;
      begin
         ignore := syscall (SYSCALL_SLEEP, 10);
      end;
   end loop;
end main;
