------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Named I/O Streams
--
--  @description
--  Producer-owned ring buffer model for structured process output.
--  Producers create streams (e.g. stdout) and handle subscription
--  requests opportunistically inside streamPrint via Poll_Service_Request.
--  Subscribers send an async subscribe, receive a read-only grant
--  to the ring buffer, then poll for new data.
--
--  Ring buffer layout:
--    Offset 0x000: StreamHeader (128 bytes)
--      +0x00  magic           : U32  ("STRI" = 0x53545249)
--      +0x04  version         : U16  (1)
--      +0x06  flags           : U16
--      +0x08  subscriberCount : U8
--      +0x0C  producerIdx     : U32  (producer advances after writing)
--      +0x10  capacity        : U32  (ring data area bytes)
--      +0x14  defaultTypeTag  : U16
--      +0x16  overflowPolicy  : U8   (1 = DROP_OLDEST)
--      +0x18  streamId        : U16
--      +0x20  subscribers     : SubscriberTable (8 x 12 bytes = 96 bytes)
--    Offset 0x080: Ring data (capacity bytes)
--
--  Ring entry format:
--    [length:u16][typeTag:u16][payload bytes][pad to 8-byte alignment]
--
--  Sentinel entry:
--    length=0xFFFF means "skip to offset 0". Used when an entry would
--    straddle the buffer boundary. The reader advances its cursor to
--    the next capacity-aligned position and re-reads from offset 0.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package CuBit.Streams is

   type StreamId   is new Unsigned_16;
   type TypeTag    is new Unsigned_16;
   type CursorSlot is range 0 .. 7;

   STREAM_STDOUT : constant StreamId := 16#02#;
   STREAM_STDERR : constant StreamId := 16#03#;

   TYPE_RAW_BYTES : constant TypeTag := 16#0000#;
   TYPE_TEXT_LINE : constant TypeTag := 16#0001#;

   --  IPC labels
   OP_STREAM_SUBSCRIBE   : constant Unsigned_32 := 16#0700#;
   OP_STREAM_UNSUBSCRIBE : constant Unsigned_32 := 16#0701#;
   OP_STREAM_NOTIFY      : constant Unsigned_32 := 16#0702#;
   OP_STREAM_WAKEUP      : constant Unsigned_32 := 16#0704#;
   OP_STREAM_LIST        : constant Unsigned_32 := 16#0705#;
   OP_STREAM_AVAILABLE   : constant Unsigned_32 := 16#0706#;
   OP_STREAM_GONE        : constant Unsigned_32 := 16#0707#;

   MAX_STREAMS     : constant := 4;
   MAX_SUBSCRIBERS : constant := 8;

   HEADER_SIZE : constant := 128;
   DATA_OFFSET : constant := 128;

   ENTRY_HEADER_SIZE : constant := 4;  -- 2 bytes length + 2 bytes typeTag

   SENTINEL_LENGTH : constant Unsigned_16 := 16#FFFF#;

   ---------------------------------------------------------------------------
   --  Producer API
   ---------------------------------------------------------------------------

   --  Create a named stream. Allocates pages via sbrk and initializes the
   --  ring buffer header. Must be called before streamWrite/streamPrint.
   procedure streamCreate (id        : StreamId;
                            pages     : Natural;
                            entryType : TypeTag);

   --  Write raw bytes to a stream. Returns bytes actually written.
   function streamWrite (id        : StreamId;
                          data      : System.Address;
                          len       : Unsigned_32;
                          entryType : TypeTag) return Unsigned_32;

   --  Write a text line to a stream (convenience wrapper around streamWrite).
   procedure streamPrint (id  : StreamId;
                           msg : String);

   --  Handle one pending subscription/unsubscription service request.
   --  Returns True if a request was handled. Call this periodically in
   --  processes that produce streams but may not call streamWrite frequently.
   function streamHandleSubscription return Boolean;

   --  Best-effort flush: poll until all subscribers have caught up with
   --  the producer, or timeout after ~100 iterations with 1ms sleeps.
   procedure streamFlush (id : StreamId);

   ---------------------------------------------------------------------------
   --  Subscriber API (used by shell)
   ---------------------------------------------------------------------------

   type SubInfo is record
      active    : Boolean     := False;
      grantBase : Unsigned_64 := 0;
      cursor    : CursorSlot  := 0;
      cursorIdx : Unsigned_32 := 0;  -- local read cursor (avoids RO fault)
      capacity  : Unsigned_32 := 0;
   end record;

   --  Non-blocking: read one entry from a subscribed stream.
   --  Returns bytes read (0 if no data available).
   function streamRead (sub       : in out SubInfo;
                         buf       : System.Address;
                         maxLen    : Unsigned_32;
                         entryType : out TypeTag) return Unsigned_32;

   --  Check how many bytes are available to read.
   function streamAvailable (sub : SubInfo) return Unsigned_32;

end CuBit.Streams;
