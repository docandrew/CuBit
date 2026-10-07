------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Named I/O Streams
--
--  @description
--  Producer-owned rings for a process's outlets (docs/ccl-launch-parameters.md,
--  "Connectors, not stdio"). A program opens each connector its manifest
--  declares by its qualified name (Open_Outlet); the ring's id is the
--  connector's position plus one. CuBit has no stdout.
--
--  Each ring is a CuBit.Stream_Rings region (docs/ccl-streams.md, "The ring
--  underneath"): a control page and a power-of-two data ring of the
--  connector's declared pages. One producer, any number of readers; the
--  producer never waits, and a slow reader loses the oldest records.
--  A reader opens a channel on the outlet's connector (CuBit.Outlet_Channels),
--  receives its own read-only grant of the region, and reads with its own
--  cursor. Producers answer those requests opportunistically inside
--  streamWrite (or streamHandleSubscription).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with CuBit.Protocols;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Messages;
with CuBit.Stream_Rings;

package CuBit.Streams is

   type StreamId   is new Unsigned_16;
   type TypeTag    is new Unsigned_16;
   type CursorSlot is range 0 .. 7;

   --  No stream: a connector the program's manifest does not declare.
   NO_STREAM : constant StreamId := 0;

   TYPE_RAW_BYTES : constant TypeTag := CuBit.Stream_Rings.ELEMENT_RAW_BYTES;
   TYPE_TEXT_LINE : constant TypeTag := CuBit.Stream_Rings.ELEMENT_TEXT_LINE;

   --  IPC labels. Reading an outlet is a channel (CuBit.Outlet_Channels):
   --  these are the queries around it.
   --  Which streams a process has (reply: word 0 a bitmask of ids, 1 count).
   OP_STREAM_LIST        : constant Unsigned_32 := 16#0705#;
   --  procmgr to a launcher: a child's declared streams (word 0 the PID,
   --  1 the bitmask).
   OP_STREAM_AVAILABLE   : constant Unsigned_32 := 16#0706#;

   MAX_STREAMS     : constant := 4;
   MAX_SUBSCRIBERS : constant := 8;

   ---------------------------------------------------------------------------
   --  Producer API
   ---------------------------------------------------------------------------

   --  Open the outlet Name (fully qualified, as the manifest declares
   --  it), from the description procmgr attached to the launch block: map
   --  the ring its launcher lent it when the block lists one, else create
   --  one with the declared pages and element type. NO_STREAM when the
   --  program declares no such outlet.
   function Open_Outlet (Name : String) return StreamId;

   ---------------------------------------------------------------------------
   --  Launcher-owned rings (docs/ccl-launch-parameters.md, "Launcher-owned
   --  outlet rings"): the launcher makes a ring in its own memory, lends it
   --  to the child, and reads it in place.
   ---------------------------------------------------------------------------

   --  A grant revoke event (CuBit.Control_Events) for a ring a launcher lent
   --  this process: the outlet is closed and its mapping returned. False
   --  when no outlet holds that grant. CuBit.Process_Events calls this.
   function Return_Revoked (Slot, Generation : Unsigned_64) return Boolean;

   --  Create a named stream. Allocates pages via sbrk and initializes the
   --  ring buffer header. Must be called before streamWrite/streamPrint.
   procedure streamCreate (id        : StreamId;
                            pages     : Natural;
                            entryType : TypeTag);

   --  Create a stream whose entries all conform to a stable wire schema.
   --  Schema identity is retained by the producer and will be negotiated by
   --  the typed subscription protocol; it is not an authority identifier.
   procedure streamCreateTyped
     (id        : StreamId;
      pages     : Natural;
      entryType : TypeTag;
      schema    : CuBit.Protocols.Schema_Contract);

   --  Write raw bytes to a stream. Returns bytes actually written.
   function streamWrite (id        : StreamId;
                          data      : System.Address;
                          len       : Unsigned_32;
                          entryType : TypeTag) return Unsigned_32;

   --  Fail closed if schema does not exactly match the stream declaration.
   function streamWriteTyped
     (id        : StreamId;
      data      : System.Address;
      len       : Unsigned_32;
      entryType : TypeTag;
      schema    : CuBit.Protocols.Schema_Contract) return Unsigned_32;

   --  Write a text line to a stream (convenience wrapper around streamWrite).
   procedure streamPrint (id  : StreamId;
                           msg : String);

   --  A reader's grant came back (CuBit.Control_Events): its slot is freed.
   --  False when it was not one of this process's readers.
   --  CuBit.Process_Events calls this.
   function Forget_Returned (Event : CuBit.Control_Events.Event) return Boolean;

   --  Handle one pending open, close or list request.
   --  Returns True if a request was handled. Call this periodically in
   --  processes that produce streams but may not call streamWrite frequently.
   function streamHandleSubscription return Boolean;

   ---------------------------------------------------------------------------
   --  Reading another process's outlet: a channel opened on its connector
   ---------------------------------------------------------------------------

   type SubInfo is record
      Link : CuBit.Channels.Channel;
   end record;

   --  Ask the process behind Endpoint to let this one read its outlet
   --  Stream, of Element records, without waiting: the completion that
   --  carries Token goes to Subscribe_Finish.
   procedure Subscribe_Begin
     (Endpoint : CuBit.Messages.CapabilitySlot; Stream : StreamId;
      Element : CuBit.Protocols.Schema_Contract; Token : Unsigned_64;
      Sub : out SubInfo; Submitted : out Boolean);
   procedure Subscribe_Finish
     (Sub : in out SubInfo; Reply : CuBit.Messages.Message; Subscribed : out Boolean);

   --  Non-blocking: read one entry from a subscribed stream.
   --  Returns bytes read (0 if no data available).
   function streamRead (sub       : in out SubInfo;
                         buf       : System.Address;
                         maxLen    : Unsigned_32;
                         entryType : out TypeTag) return Unsigned_32;

   --  How many bytes are available to read.
   function streamAvailable (sub : SubInfo) return Unsigned_32;

   procedure Unsubscribe (Sub : in out SubInfo);

end CuBit.Streams;
