------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Named I/O Streams implementation
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

package body CuBit.Streams is

   REPLY_OK  : constant Unsigned_32 := 16#F000#;
   REPLY_ERR : constant Unsigned_32 := 16#F001#;

   MAGIC : constant Unsigned_32 := 16#53545249#;  -- "STRI"

   ---------------------------------------------------------------------------
   --  Internal stream state (producer side)
   ---------------------------------------------------------------------------

   type GrantIdArray is
      array (CursorSlot) of Unsigned_64;

   type StreamState is record
      active   : Boolean      := False;
      id       : StreamId     := 0;
      pages    : Natural      := 0;
      baseAddr : Unsigned_64  := 0;  -- start of ring buffer (header + data)
      capacity : Unsigned_32  := 0;  -- data area size in bytes
      grantIds : GrantIdArray := (others => 0);  -- per-subscriber grant IDs
   end record;

   subtype StreamIndex is Natural range 0 .. MAX_STREAMS - 1;
   streamTab : array (StreamIndex) of StreamState;

   ---------------------------------------------------------------------------
   --  Volatile memory access helpers
   ---------------------------------------------------------------------------

   procedure writeU32 (addr : Unsigned_64; val : Unsigned_32);
   function readU32 (addr : Unsigned_64) return Unsigned_32;
   procedure writeU16 (addr : Unsigned_64; val : Unsigned_16);
   function readU16 (addr : Unsigned_64) return Unsigned_16;
   procedure writeU8 (addr : Unsigned_64; val : Unsigned_8);
   function readU8 (addr : Unsigned_64) return Unsigned_8;

   function findStream (id : StreamId) return Integer;
   function alignUp8 (v : Unsigned_32) return Unsigned_32;
   function getMinCursor (base : Unsigned_64;
                           subCnt : Unsigned_8;
                           pIdx : Unsigned_32) return Unsigned_32;

   procedure writeU32 (addr : Unsigned_64; val : Unsigned_32) is
      mem : Unsigned_32
         with Import, Address => To_Address (Integer_Address (addr)),
              Volatile;
   begin
      mem := val;
   end writeU32;

   function readU32 (addr : Unsigned_64) return Unsigned_32 is
      mem : Unsigned_32
         with Import, Address => To_Address (Integer_Address (addr)),
              Volatile;
   begin
      return mem;
   end readU32;

   procedure writeU16 (addr : Unsigned_64; val : Unsigned_16) is
      mem : Unsigned_16
         with Import, Address => To_Address (Integer_Address (addr)),
              Volatile;
   begin
      mem := val;
   end writeU16;

   function readU16 (addr : Unsigned_64) return Unsigned_16 is
      mem : Unsigned_16
         with Import, Address => To_Address (Integer_Address (addr)),
              Volatile;
   begin
      return mem;
   end readU16;

   procedure writeU8 (addr : Unsigned_64; val : Unsigned_8) is
      mem : Unsigned_8
         with Import, Address => To_Address (Integer_Address (addr)),
              Volatile;
   begin
      mem := val;
   end writeU8;

   function readU8 (addr : Unsigned_64) return Unsigned_8 is
      mem : Unsigned_8
         with Import, Address => To_Address (Integer_Address (addr)),
              Volatile;
   begin
      return mem;
   end readU8;

   ---------------------------------------------------------------------------
   --  Header field offsets
   ---------------------------------------------------------------------------
   HDR_MAGIC            : constant := 16#00#;
   HDR_VERSION          : constant := 16#04#;
   HDR_FLAGS            : constant := 16#06#;
   HDR_SUBSCRIBER_COUNT : constant := 16#08#;
   HDR_PRODUCER_IDX     : constant := 16#0C#;
   HDR_CAPACITY         : constant := 16#10#;
   HDR_DEFAULT_TYPE_TAG : constant := 16#14#;
   HDR_OVERFLOW_POLICY  : constant := 16#16#;
   HDR_STREAM_ID        : constant := 16#18#;

   --  Subscriber table starts at offset 0x20 (32 bytes into header)
   SUBSCRIBER_TABLE_OFF : constant := 16#20#;
   --  Each subscriber entry: pid(u32) + cursorIdx(u32) + flags(u16) +
   --  reserved(u16) = 12 bytes
   SUBSCRIBER_ENTRY_SIZE : constant := 12;

   SUB_OFF_PID       : constant := 0;
   SUB_OFF_CURSOR    : constant := 4;
   SUB_OFF_FLAGS     : constant := 8;

   ---------------------------------------------------------------------------
   --  findStream - find stream by ID in local table
   ---------------------------------------------------------------------------
   function findStream (id : StreamId) return Integer is
   begin
      for i in StreamIndex loop
         if streamTab (i).active and then streamTab (i).id = id then
            return i;
         end if;
      end loop;
      return -1;
   end findStream;

   ---------------------------------------------------------------------------
   --  alignUp8 - round up to 8-byte boundary
   ---------------------------------------------------------------------------
   function alignUp8 (v : Unsigned_32) return Unsigned_32 is
   begin
      return (v + 7) and not 7;
   end alignUp8;

   ---------------------------------------------------------------------------
   --  getMinCursor
   --
   --  Scan subscriber table and return the smallest cursorIdx across all
   --  active subscribers. Returns producerIdx if there are no subscribers.
   ---------------------------------------------------------------------------
   function getMinCursor (base : Unsigned_64;
                           subCnt : Unsigned_8;
                           pIdx : Unsigned_32) return Unsigned_32
   is
      minCur  : Unsigned_32 := pIdx;
      slotOff : Unsigned_64;
      pid     : Unsigned_32;
      cur     : Unsigned_32;
   begin
      if subCnt = 0 then
         return pIdx;
      end if;

      for i in 0 .. Natural (subCnt) - 1 loop
         slotOff := base + SUBSCRIBER_TABLE_OFF +
            Unsigned_64 (i) * SUBSCRIBER_ENTRY_SIZE;
         pid := readU32 (slotOff + SUB_OFF_PID);
         if pid /= 0 then
            cur := readU32 (slotOff + SUB_OFF_CURSOR);
            if i = 0 or else cur < minCur then
               minCur := cur;
            end if;
         end if;
      end loop;

      return minCur;
   end getMinCursor;

   ---------------------------------------------------------------------------
   --  streamHandleSubscription
   --
   --  Public. Drains 1 pending service request and handles
   --  subscribe or unsubscribe requests. Returns True if handled.
   ---------------------------------------------------------------------------
   function streamHandleSubscription return Boolean is
      from  : ProcessID;
      msg   : Message;
      found : Boolean;
   begin
      Poll_Service_Request (from, msg, found);

      if not found then
         return False;
      end if;

      if msg.tag.label = OP_STREAM_SUBSCRIBE then
         declare
            reqId : constant StreamId :=
               StreamId (msg.words (0) and 16#FFFF#);
            idx   : constant Integer := findStream (reqId);
         begin
            if idx >= 0 then
               declare
                  s       : StreamState renames streamTab (idx);
                  base    : constant Unsigned_64 := s.baseAddr;
                  subCnt  : Unsigned_8;
                  slot    : CursorSlot;
                  slotOff : Unsigned_64;
                  gid     : Unsigned_64;
                  ok      : Boolean;
                  pIdx    : Unsigned_32;
                  replyMsg : Message;
                  ignore   : Unsigned_64;
               begin
                  subCnt := readU8 (base + HDR_SUBSCRIBER_COUNT);

                  if Natural (subCnt) >= MAX_SUBSCRIBERS then
                     --  No room: reply with error
                     replyMsg := NULL_MESSAGE;
                     replyMsg.tag.label := REPLY_ERR;
                     ignore := reply (from, replyMsg);
                  else
                     slot := CursorSlot (subCnt);
                     slotOff := base + SUBSCRIBER_TABLE_OFF +
                        Unsigned_64 (slot) * SUBSCRIBER_ENTRY_SIZE;

                     --  Create read-only grant to the ring buffer
                     createGrant (
                        grantee   => from,
                        localAddr => To_Address (
                           Integer_Address (s.baseAddr)),
                        numPages  => s.pages,
                        readWrite => False,
                        grantId   => gid,
                        success   => ok);

                     if ok then
                        --  Store grant ID for later revocation
                        s.grantIds (slot) := gid;

                        --  Fill subscriber entry
                        pIdx := readU32 (base + HDR_PRODUCER_IDX);
                        writeU32 (slotOff + SUB_OFF_PID,
                           Unsigned_32 (from));
                        writeU32 (slotOff + SUB_OFF_CURSOR, pIdx);
                        writeU16 (slotOff + SUB_OFF_FLAGS, 0);

                        --  Increment subscriber count
                        writeU8 (base + HDR_SUBSCRIBER_COUNT,
                           subCnt + 1);

                        --  Reply with grant info
                        replyMsg := NULL_MESSAGE;
                        replyMsg.tag.label := REPLY_OK;
                        replyMsg.words (0) := gid;
                        replyMsg.words (1) := Unsigned_64 (slot);
                        replyMsg.words (2) := Unsigned_64 (s.capacity);
                        replyMsg.words (3) := Unsigned_64 (pIdx);
                        ignore := reply (from, replyMsg);
                     else
                        replyMsg := NULL_MESSAGE;
                        replyMsg.tag.label := REPLY_ERR;
                        ignore := reply (from, replyMsg);
                     end if;
                  end if;
               end;
            else
               --  Stream not found: reply with error
               declare
                  replyMsg : Message;
                  ignore   : Unsigned_64;
               begin
                  replyMsg := NULL_MESSAGE;
                  replyMsg.tag.label := REPLY_ERR;
                  ignore := reply (from, replyMsg);
               end;
            end if;
         end;

         return True;

      elsif msg.tag.label = OP_STREAM_LIST then
         --  Return bitmask of active streams and count
         declare
            bitmask     : Unsigned_64 := 0;
            activeCount : Unsigned_32 := 0;
            replyMsg    : Message := NULL_MESSAGE;
            ignore      : Unsigned_64;
         begin
            for i in StreamIndex loop
               if streamTab (i).active then
                  bitmask := bitmask or
                     Shift_Left (Unsigned_64'(1),
                        Natural (streamTab (i).id));
                  activeCount := activeCount + 1;
               end if;
            end loop;
            replyMsg.tag.label := REPLY_OK;
            replyMsg.words (0) := bitmask;
            replyMsg.words (1) := Unsigned_64 (activeCount);
            ignore := reply (from, replyMsg);
         end;
         return True;

      elsif msg.tag.label = OP_STREAM_UNSUBSCRIBE then
         declare
            reqId   : constant StreamId :=
               StreamId (msg.words (0) and 16#FFFF#);
            reqSlot : constant Unsigned_64 := msg.words (1);
            idx     : constant Integer := findStream (reqId);
            replyMsg : Message;
            ignore   : Unsigned_64;
         begin
            if idx >= 0 and then reqSlot <= Unsigned_64 (CursorSlot'Last)
            then
               declare
                  s       : StreamState renames streamTab (idx);
                  base    : constant Unsigned_64 := s.baseAddr;
                  slot    : constant CursorSlot := CursorSlot (reqSlot);
                  slotOff : constant Unsigned_64 := base +
                     SUBSCRIBER_TABLE_OFF +
                     Unsigned_64 (slot) * SUBSCRIBER_ENTRY_SIZE;
                  subPid  : constant Unsigned_32 :=
                     readU32 (slotOff + SUB_OFF_PID);
                  subCnt  : Unsigned_8;
               begin
                  --  Validate the slot belongs to the requesting PID
                  if subPid = Unsigned_32 (from) then
                     --  Revoke the grant
                     if s.grantIds (slot) /= 0 then
                        revokeGrant (s.grantIds (slot));
                        s.grantIds (slot) := 0;
                     end if;

                     --  Clear subscriber entry
                     writeU32 (slotOff + SUB_OFF_PID, 0);
                     writeU32 (slotOff + SUB_OFF_CURSOR, 0);
                     writeU16 (slotOff + SUB_OFF_FLAGS, 0);

                     --  Decrement subscriber count
                     subCnt := readU8 (base + HDR_SUBSCRIBER_COUNT);
                     if subCnt > 0 then
                        writeU8 (base + HDR_SUBSCRIBER_COUNT,
                           subCnt - 1);
                     end if;

                     replyMsg := NULL_MESSAGE;
                     replyMsg.tag.label := REPLY_OK;
                     ignore := reply (from, replyMsg);
                  else
                     --  PID mismatch: deny
                     replyMsg := NULL_MESSAGE;
                     replyMsg.tag.label := REPLY_ERR;
                     ignore := reply (from, replyMsg);
                  end if;
               end;
            else
               replyMsg := NULL_MESSAGE;
               replyMsg.tag.label := REPLY_ERR;
               ignore := reply (from, replyMsg);
            end if;
         end;

         return True;
      end if;

      --  Unknown message type: drop silently
      return False;
   end streamHandleSubscription;

   ---------------------------------------------------------------------------
   --  streamCreate
   ---------------------------------------------------------------------------
   procedure streamCreate (id        : StreamId;
                            pages     : Natural;
                            entryType : TypeTag) is
      totalBytes : constant Unsigned_64 := Unsigned_64 (pages) * 4096;
      ret        : Unsigned_64;
      idx        : Integer := -1;
   begin
      --  Find a free slot
      for i in StreamIndex loop
         if not streamTab (i).active then
            idx := i;
            exit;
         end if;
      end loop;

      if idx < 0 then
         return;  -- no free slots
      end if;

      --  Allocate memory via sbrk
      ret := syscall (SYSCALL_SBRK, totalBytes);
      if ret = Unsigned_64'Last then
         return;  -- sbrk failed
      end if;

      declare
         base : constant Unsigned_64 := ret;
         cap  : constant Unsigned_32 :=
            Unsigned_32 (totalBytes) - Unsigned_32 (HEADER_SIZE);
      begin
         --  Zero the header area
         for i in 0 .. HEADER_SIZE - 1 loop
            writeU8 (base + Unsigned_64 (i), 0);
         end loop;

         --  Initialize header fields
         writeU32 (base + HDR_MAGIC, MAGIC);
         writeU16 (base + HDR_VERSION, 1);
         writeU16 (base + HDR_FLAGS, 0);
         writeU8  (base + HDR_SUBSCRIBER_COUNT, 0);
         writeU32 (base + HDR_PRODUCER_IDX, 0);
         writeU32 (base + HDR_CAPACITY, cap);
         writeU16 (base + HDR_DEFAULT_TYPE_TAG, Unsigned_16 (entryType));
         writeU8  (base + HDR_OVERFLOW_POLICY, 1);  -- DROP_OLDEST
         writeU16 (base + HDR_STREAM_ID, Unsigned_16 (id));

         --  Store in local table
         streamTab (idx) := (
            active   => True,
            id       => id,
            pages    => pages,
            baseAddr => base,
            capacity => cap,
            grantIds => (others => 0));
      end;
   end streamCreate;

   ---------------------------------------------------------------------------
   --  streamWrite
   --
   --  Ring buffer write with sentinel wraparound and flow control.
   --  When an entry would straddle the buffer boundary, a sentinel
   --  (length=0xFFFF) is written at the current position and the
   --  producer index advances to the start. This ensures entry headers
   --  and payloads are always contiguous.
   --
   --  Flow control: before writing, check the slowest subscriber's
   --  cursor. Under DROP_OLDEST policy, advance lagging subscribers
   --  to make room.
   ---------------------------------------------------------------------------
   function streamWrite (id        : StreamId;
                          data      : System.Address;
                          len       : Unsigned_32;
                          entryType : TypeTag) return Unsigned_32
   is
      idx : constant Integer := findStream (id);
   begin
      if idx < 0 or len = 0 then
         return 0;
      end if;

      --  Handle pending subscription requests before writing
      declare
         handled : Boolean;
         pragma Unreferenced (handled);
      begin
         handled := streamHandleSubscription;
      end;

      declare
         s        : StreamState renames streamTab (idx);
         base     : constant Unsigned_64 := s.baseAddr;
         dataBase : constant Unsigned_64 := base + DATA_OFFSET;
         cap      : constant Unsigned_32 := s.capacity;
         pIdx     : Unsigned_32;

         --  Ring entry: [len:u16][entryType:u16][payload][pad-to-8B]
         entryLen : constant Unsigned_32 :=
            alignUp8 (ENTRY_HEADER_SIZE + len);

         type ByteArray is array (0 .. Natural (len) - 1) of Unsigned_8
            with Convention => C;
         src : ByteArray with Import, Address => data;

         writeOff   : Unsigned_32;
         subCnt     : Unsigned_8;
         minCur     : Unsigned_32;
         used       : Unsigned_32;
         avail      : Unsigned_32;
         needed     : Unsigned_32;
      begin
         if entryLen > cap then
            return 0;  -- entry too large for ring
         end if;

         pIdx := readU32 (base + HDR_PRODUCER_IDX);

         --  Check if entry would straddle the boundary; write sentinel
         writeOff := pIdx mod cap;
         if writeOff + entryLen > cap then
            --  Write sentinel marker
            writeU16 (dataBase + Unsigned_64 (writeOff), 16#FFFF#);
            --  Advance past the remainder of the buffer
            pIdx := pIdx + (cap - writeOff);
            writeOff := 0;
         end if;

         --  Flow control: check slowest subscriber
         subCnt := readU8 (base + HDR_SUBSCRIBER_COUNT);
         if subCnt > 0 then
            minCur := getMinCursor (base, subCnt, pIdx);
            used := pIdx - minCur;
            if used > cap then
               --  Subscribers fell too far behind; treat as no space used
               used := cap;
            end if;
            avail := cap - used;

            --  Need entryLen space (sentinel already accounted for above)
            needed := entryLen;
            if avail < needed then
               --  DROP_OLDEST: advance lagging subscribers
               declare
                  advance : constant Unsigned_32 := needed - avail;
                  slotOff : Unsigned_64;
                  pid     : Unsigned_32;
                  cur     : Unsigned_32;
               begin
                  for i in 0 .. Natural (subCnt) - 1 loop
                     slotOff := base + SUBSCRIBER_TABLE_OFF +
                        Unsigned_64 (i) * SUBSCRIBER_ENTRY_SIZE;
                     pid := readU32 (slotOff + SUB_OFF_PID);
                     if pid /= 0 then
                        cur := readU32 (slotOff + SUB_OFF_CURSOR);
                        if (pIdx - cur) + advance > cap then
                           --  This subscriber needs advancing
                           writeU32 (slotOff + SUB_OFF_CURSOR,
                              cur + advance);
                        end if;
                     end if;
                  end loop;
               end;
            end if;
         end if;

         --  Write entry header: length (u16) + entryType (u16)
         writeU16 (dataBase + Unsigned_64 (writeOff),
            Unsigned_16 (len));
         writeU16 (dataBase + Unsigned_64 (writeOff + 2),
            Unsigned_16 (entryType));

         --  Write payload (contiguous, no wraparound needed with sentinel)
         for i in 0 .. Natural (len) - 1 loop
            writeU8 (dataBase + Unsigned_64 (writeOff) +
               Unsigned_64 (ENTRY_HEADER_SIZE) + Unsigned_64 (i),
               src (i));
         end loop;

         --  Advance producer index by aligned entry size
         writeU32 (base + HDR_PRODUCER_IDX, pIdx + entryLen);

         return len;
      end;
   end streamWrite;

   ---------------------------------------------------------------------------
   --  streamPrint
   ---------------------------------------------------------------------------
   procedure streamPrint (id  : StreamId;
                           msg : String)
   is
      ignore : Unsigned_32;
   begin
      if msg'Length = 0 then
         return;
      end if;

      declare
         type ByteArray is array (0 .. msg'Length - 1) of Unsigned_8
            with Convention => C;
         buf : ByteArray;
      begin
         for i in 0 .. msg'Length - 1 loop
            buf (i) := Unsigned_8 (Character'Pos (msg (msg'First + i)));
         end loop;
         ignore := streamWrite (id, buf'Address,
            Unsigned_32 (msg'Length), TYPE_TEXT_LINE);
      end;
   end streamPrint;

   ---------------------------------------------------------------------------
   --  streamFlush
   --
   --  Best-effort: poll subscriber cursors until all match producerIdx
   --  or timeout after ~100 iterations with 1ms sleeps.
   ---------------------------------------------------------------------------
   procedure streamFlush (id : StreamId) is
      idx : constant Integer := findStream (id);
      ignore : Unsigned_64;
   begin
      if idx < 0 then
         return;
      end if;

      declare
         s      : StreamState renames streamTab (idx);
         base   : constant Unsigned_64 := s.baseAddr;
         pIdx   : Unsigned_32;
         subCnt : Unsigned_8;
         allCaughtUp : Boolean;
         slotOff : Unsigned_64;
         pid     : Unsigned_32;
         cur     : Unsigned_32;
      begin
         for attempt in 1 .. 100 loop
            pIdx := readU32 (base + HDR_PRODUCER_IDX);
            subCnt := readU8 (base + HDR_SUBSCRIBER_COUNT);

            if subCnt = 0 then
               return;  -- no subscribers, nothing to flush
            end if;

            allCaughtUp := True;
            for i in 0 .. Natural (subCnt) - 1 loop
               slotOff := base + SUBSCRIBER_TABLE_OFF +
                  Unsigned_64 (i) * SUBSCRIBER_ENTRY_SIZE;
               pid := readU32 (slotOff + SUB_OFF_PID);
               if pid /= 0 then
                  cur := readU32 (slotOff + SUB_OFF_CURSOR);
                  if cur /= pIdx then
                     allCaughtUp := False;
                     exit;
                  end if;
               end if;
            end loop;

            if allCaughtUp then
               return;
            end if;

            --  Sleep 1ms and retry
            ignore := syscall (SYSCALL_SLEEP, 1);
         end loop;
      end;
   end streamFlush;

   ---------------------------------------------------------------------------
   --  streamRead (subscriber side)
   --
   --  Reads one entry from a subscribed stream. Handles sentinel entries
   --  by advancing the cursor to offset 0 and re-reading.
   ---------------------------------------------------------------------------
   function streamRead (sub       : in out SubInfo;
                         buf       : System.Address;
                         maxLen    : Unsigned_32;
                         entryType : out TypeTag) return Unsigned_32
   is
   begin
      entryType := TYPE_RAW_BYTES;

      if not sub.active or sub.capacity = 0 then
         return 0;
      end if;

      declare
         base     : constant Unsigned_64 := sub.grantBase;
         dataBase : constant Unsigned_64 := base + DATA_OFFSET;
         cap      : constant Unsigned_32 := sub.capacity;

         pIdx : constant Unsigned_32 :=
            readU32 (base + HDR_PRODUCER_IDX);

         cIdx : Unsigned_32 := sub.cursorIdx;
      begin
         --  Nothing to read?
         if pIdx = cIdx then
            return 0;
         end if;

         --  Read entry header
         declare
            readOff  : Unsigned_32 := cIdx mod cap;
            entryLen : Unsigned_16 :=
               readU16 (dataBase + Unsigned_64 (readOff));
         begin
            --  Handle sentinel: skip to offset 0
            if entryLen = SENTINEL_LENGTH then
               cIdx := cIdx + (cap - readOff);
               readOff := 0;

               --  Re-check: anything left?
               if pIdx = cIdx then
                  sub.cursorIdx := cIdx;
                  return 0;
               end if;

               entryLen := readU16 (dataBase);
            end if;

            if entryLen = 0 then
               return 0;
            end if;

            declare
               entryTag : constant Unsigned_16 :=
                  readU16 (dataBase + Unsigned_64 (readOff + 2));
               copyLen  : Unsigned_32;
               aligned  : Unsigned_32;

               type ByteArray is
                  array (0 .. Natural (maxLen) - 1) of Unsigned_8
                  with Convention => C;
               dst : ByteArray with Import, Address => buf;
            begin
               entryType := TypeTag (entryTag);

               copyLen := Unsigned_32 (entryLen);
               if copyLen > maxLen then
                  copyLen := maxLen;
               end if;

               --  Copy payload (contiguous with sentinel scheme)
               for i in 0 .. Natural (copyLen) - 1 loop
                  dst (i) := readU8 (dataBase +
                     Unsigned_64 (readOff) +
                     Unsigned_64 (ENTRY_HEADER_SIZE) +
                     Unsigned_64 (i));
               end loop;

               --  Advance local cursor by full aligned entry size
               aligned := alignUp8 (ENTRY_HEADER_SIZE +
                  Unsigned_32 (entryLen));
               cIdx := cIdx + aligned;
               sub.cursorIdx := cIdx;

               return copyLen;
            end;
         end;
      end;
   end streamRead;

   ---------------------------------------------------------------------------
   --  streamAvailable
   ---------------------------------------------------------------------------
   function streamAvailable (sub : SubInfo) return Unsigned_32 is
   begin
      if not sub.active or sub.capacity = 0 then
         return 0;
      end if;

      declare
         base : constant Unsigned_64 := sub.grantBase;
         pIdx : constant Unsigned_32 :=
            readU32 (base + HDR_PRODUCER_IDX);
      begin
         return pIdx - sub.cursorIdx;
      end;
   end streamAvailable;

end CuBit.Streams;
