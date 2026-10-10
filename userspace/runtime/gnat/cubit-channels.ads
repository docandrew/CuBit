------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Channels (docs/data-plane.md): one side opens through an endpoint
--  capability (Open), the other answers the request in its service loop
--  (Accept_Open), and both then move typed elements through shared memory
--  with no IPC per element (Put, Take).
--
--  @description
--  The producer owns the data region and grants it read-only, with grant
--  events, to the consumer. When the policy needs it (Lossless,
--  Shed_Newest), the consumer owns an index page and grants it read-only
--  to the producer. Each side checks every word the other writes, through
--  the proved rings (CuBit.Channel_Rings, CuBit.Datagram_Rings,
--  CuBit.Stream_Rings); the ordering of those words is this package's.
--
--  Wakes: a side that would wait arms its waiting word, checks again, and
--  then sleeps; the other side kicks it after making progress. Only the
--  opener can kick (it holds the endpoint), so a side that accepted the
--  channel is kicked and the opener polls or waits by its own means.
--
--  A channel ends when either side closes it or dies. Close revokes this
--  side's grant and returns the peer's mapping; a peer's grant events
--  (CuBit.Process_Events) say the other side did (Ended).
--
--  Queues carry elements (bounded, or fixed-size: exactly their size);
--  arenas are regions of buffers the lending side owns and both sides write
--  (Buffer_Address), handed over by a companion queue; a duplex channel is
--  one region a side (Own_Base, Peer_Base), each written by its owner only,
--  in a layout the element names (a request ring one way, answers back).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Channel_Rings;
with CuBit.Control_Events;
with CuBit.Memory_Grants;
with CuBit.Messages;

package CuBit.Channels is

   package CC renames CuBit.Channel_Contracts;
   package CP renames CuBit.Channel_Protocol;
   use type CC.Channel_Kind;

   type Side is (Producing, Consuming);

   --  Words the consumer publishes to the producer in its index page, by
   --  agreement of the channel's type (logging: the least severe record
   --  kept).
   Consumer_Words : constant := 8;
   subtype Consumer_Word_Index is Natural range 0 .. Consumer_Words - 1;

   type Channel is record
      Active    : Boolean := False;
      Item      : CC.Contract;
      This_Side : Side := Producing;
      Opener    : Boolean := False;
      --  The opener's way to the peer, or the acceptor's peer.
      Endpoint  : CuBit.Messages.CapabilitySlot := 0;
      Peer      : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      --  The channel's number at the peer (an opener's OP_KICK and
      --  OP_CLOSE carry it).
      Peer_Number : Unsigned_64 := 0;
      --  This side's region (the data region, or the index page) and its
      --  grant; the peer's, as mapped here.
      Own_Base    : Unsigned_64 := 0;
      Own_Bytes   : Unsigned_64 := 0;
      Own_Grant   : CuBit.Memory_Grants.Grant_Reference := (slot => 0, generation => 1);
      Has_Own     : Boolean := False;
      Peer_Base   : Unsigned_64 := 0;
      Peer_Grant  : CuBit.Memory_Grants.Grant_Reference := (slot => 0, generation => 1);
      Has_Peer    : Boolean := False;
      Writer      : CuBit.Channel_Rings.Producer;
      Reader      : CuBit.Channel_Rings.Consumer;
      Cursor      : CuBit.Channel_Rings.Index := 0;   --  drop-oldest readers
      --  This side's region belongs to something else (a broadcast
      --  outlet's ring, Accept_Shared): closing revokes only the grant.
      Shares_Region : Boolean := False;
   end record;

   type Open_Result is (Opened, Refused, Failed);

   --  Open a channel to the service behind Endpoint, this side being
   --  This_Side, on the peer's Connector (an outlet or inlet number; 0 when
   --  the peer has one kind of channel). Refused: the peer said why
   --  (Refusal). The peer may answer with a different queue capacity (a
   --  consumer then uses the producer's).
   procedure Open
     (Endpoint : CuBit.Messages.CapabilitySlot; Item : CC.Contract; This_Side : Side;
      C : out Channel; Result : out Open_Result; Refusal : out CP.Open_Refusal;
      Connector : Unsigned_16 := 0)
   with Pre => CC.Valid (Item);

   --  The same without waiting: the request goes out as a submission whose
   --  completion carries Token; pass the completion's message to
   --  Finish_Open. Submitted False: nothing was sent (C is inactive).
   procedure Begin_Open
     (Endpoint : CuBit.Messages.CapabilitySlot; Item : CC.Contract; This_Side : Side;
      Token : Unsigned_64; C : out Channel; Submitted : out Boolean;
      Connector : Unsigned_16 := 0)
   with Pre => CC.Valid (Item);
   procedure Finish_Open
     (C : in out Channel; Reply : CuBit.Messages.Message;
      Result : out Open_Result; Refusal : out CP.Open_Refusal);

   --  Whether Request is an open request; its contract, the side the
   --  opener takes and the connector it names when it is (Valid: the
   --  contract decoded).
   procedure Decode_Open
     (Request : CuBit.Messages.Message; Is_Open : out Boolean; Valid : out Boolean;
      Item : out CC.Contract; Opener_Side : out Side; Connector : out Unsigned_16);

   --  Answer an open request from From (Decode_Open said Valid), as this
   --  service's channel Number. Reply is the reply to send: REPLY_OK, or
   --  REPLY_ERR with a refusal when the opener's grant cannot be used or
   --  memory runs out (C is then inactive).
   procedure Accept_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Number : Unsigned_64; C : out Channel; Reply : out CuBit.Messages.Message);

   --  Answer a consuming open (Drop_Oldest) by granting a region this
   --  process already produces into: a broadcast outlet's control page and
   --  ring of Pages, at Base. Every reader gets its own grant of the same
   --  pages; closing C revokes that grant only.
   procedure Accept_Shared
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Number : Unsigned_64; Base : Unsigned_64; Pages : CC.Ring_Pages;
      C : out Channel; Reply : out CuBit.Messages.Message);

   --  The refusal reply to an open request this service will not take.
   function Refusal_Reply (Why : CP.Open_Refusal) return CuBit.Messages.Message;

   type Put_Result is (Put, Full, Shed, Too_Large, Closed);

   --  One element of Length bytes at Data (producers). Full: lossless and
   --  no room; the producer's waiting word is armed, and the consumer's
   --  next Take kicks it if this side opened the channel. Shed: dropped
   --  and counted (Shed_Newest).
   procedure Put
     (C : in out Channel; Data : System.Address; Length : Natural; Result : out Put_Result);

   type Take_Result is (Taken, Empty, Malformed, Closed);

   --  The next element into Into (at most Maximum bytes; the rest of a
   --  longer one is dropped), consumers. Copied, then the caller validates
   --  it (Copy_Then_Validate). Hold: the producer is not told yet; Release
   --  tells it about everything taken so far (a consumer that must finish
   --  with what it took before the producer counts it done, as logstore
   --  stores records before their publisher's Flush may return).
   procedure Take
     (C : in out Channel; Into : System.Address; Maximum : Natural;
      Length : out Natural; Result : out Take_Result; Hold : Boolean := False);

   --  Tell the producer how far this side has taken (after Take with Hold).
   procedure Release (C : in out Channel);

   --  Before sleeping for more (consumers): arm the waiting word. False
   --  when an element arrived meanwhile (do not sleep; Take again).
   function Arm (C : Channel) return Boolean;
   procedure Disarm (C : Channel);

   --  Ask the peer to look now (an opener only; the acceptor is told).
   procedure Kick (C : Channel);

   --  Producers of a channel with a consumer index: whether the consumer
   --  has taken everything put so far.
   function All_Taken (C : in out Channel) return Boolean;

   --  Producers: the ring bytes free now, as far as the consumer's index
   --  says (a producer that must not lose an element checks before taking
   --  it from elsewhere).
   function Free_Bytes (C : in out Channel) return Natural;

   --  An arena's buffer Buffer (0 .. Buffers - 1): Pages pages, writable
   --  by both sides; who uses it when is the companion queue's business.
   function Buffer_Address (C : Channel; Buffer : Natural) return System.Address
   with Pre => C.Active and then C.Item.Kind = CC.Arena and then Buffer < C.Item.Buffers;

   --  The peer's OP_KICK or OP_CLOSE carries this side's channel number
   --  (word 0); the acceptor dispatches on it.
   function Number_Of (Request : CuBit.Messages.Message) return Unsigned_64;

   --  Whether Request, From's OP_CLOSE, names C: its peer, and one of its
   --  grants (not an earlier channel's with the same number).
   function Closed_By
     (C : Channel; From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message)
     return Boolean;

   --  Records the producer shed so far (Shed_Newest), as it published.
   function Shed_Count (C : Channel) return Unsigned_64;

   --  The consumer's published words (consumers write, producers read).
   procedure Set_Consumer_Word (C : Channel; Index : Consumer_Word_Index; Value : Unsigned_64);
   function Consumer_Word (C : Channel; Index : Consumer_Word_Index) return Unsigned_64;

   --  Whether Event (from CuBit.Process_Events) ends C: the peer revoked
   --  its grant or died, or took back what this side granted. The kernel
   --  tells both sides, always (docs/ipc-delivery.md).
   function Ended (C : Channel; Event : CuBit.Control_Events.Event) return Boolean;

   --  End C from this side: the peer's mapping is returned, this side's
   --  grant revoked (the kernel tells the peer), and an opener tells the
   --  peer too (OP_CLOSE). Its memory is
   --  released when the kernel confirms the grant retired (the next Close
   --  of the same channel record, or Release_Retired).
   procedure Close (C : in out Channel);

   --  Release the memory of closed channels whose grants have retired
   --  since (Open does this too), and say how many regions still wait for
   --  their peer to let go. A process that wants to leave nothing behind
   --  (a diagnostic counting its own memory) waits for zero.
   function Retiring_Regions return Natural;

end CuBit.Channels;
