------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The control plane's messages and the data plane's region layouts
--  (docs/data-plane.md, "The control plane"). Shared by the Ada runtime
--  (CuBit.Channels) and the libc.
--
--  @description
--  Opening (a call through an endpoint capability):
--    OP_OPEN_PRODUCING: the opener will produce. Words 0 .. 2: the
--      contract (CuBit.Channel_Contracts); 3: the opener's data grant
--      (wire form), made with the notify flag.
--    OP_OPEN_CONSUMING: the opener will consume. Words 0 .. 2: the
--      contract; 3: the opener's index grant when the policy needs one
--      (Lossless, Shed_Newest), else 0.
--    The message tag's reserved field names the peer's connector (an
--    outlet or inlet number; 0 when it has one kind of channel).
--    Reply REPLY_OK: word 0 the channel's number at the peer, 1 the peer's
--      grant (its index page when the opener produces and the policy
--      needs one; its data region when the opener consumes; else 0), 2 the
--      queue's pages as the peer accepted them (a broadcast outlet answers
--      with its ring's).
--      REPLY_ERR: word 0 an Open_Refusal.
--  OP_CLOSE (one-way): word 0 the channel's number at the receiver.
--  OP_KICK (one-way): word 0 the channel's number at the receiver; sent
--    only while the receiver's waiting word is set.
--
--  Data region (producer-owned, granted read-only to the consumer): a
--  control page, then the ring (queues; CuBit.Stream_Rings' layout, which
--  this extends) or the buffers (arenas). Index page (consumer-owned,
--  granted read-only to the producer): the consumer's index and waiting
--  word. Every word written by a peer is checked before use.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Contracts; use CuBit.Channel_Contracts;
with CuBit.Stream_Rings;

package CuBit.Channel_Protocol with Pure, SPARK_Mode is

   OP_OPEN_PRODUCING : constant := 16#0E00#;
   OP_OPEN_CONSUMING : constant := 16#0E01#;
   OP_CLOSE          : constant := 16#0E02#;
   OP_KICK           : constant := 16#0E03#;
   Open_Words  : constant := Contract_Words + 1;
   Reply_Words : constant := 3;

   type Open_Refusal is
     (Unknown_Type,      --  the peer has no such channel to offer
      Unsupported,       --  the kind, policy or size is not one it takes
      No_Room,           --  it has no free channel slot
      Bad_Grant);        --  the opener's grant could not be acquired
   for Open_Refusal use (Unknown_Type => 1, Unsupported => 2, No_Room => 3, Bad_Grant => 4);

   --  The producer's control page (Stream_Rings' words, then these).
   PRODUCED_OFFSET  : constant := CuBit.Stream_Rings.PRODUCED_OFFSET;
   OLDEST_OFFSET    : constant := CuBit.Stream_Rings.OLDEST_OFFSET;
   ENDED_OFFSET     : constant := CuBit.Stream_Rings.ENDED_OFFSET;
   ELEMENT_OFFSET   : constant := CuBit.Stream_Rings.ELEMENT_OFFSET;
   --  The producer waits for room (lossless): the consumer kicks it after
   --  moving its index.
   PRODUCER_WAITING_OFFSET : constant := 320;
   --  Records the producer dropped because the ring was full (Shed_Newest).
   SHED_OFFSET : constant := 384;
   CONTROL_BYTES : constant := CuBit.Stream_Rings.CONTROL_BYTES;

   --  The consumer's index page.
   CONSUMED_OFFSET : constant := 0;
   --  The consumer waits for data: the producer kicks it after publishing.
   CONSUMER_WAITING_OFFSET : constant := 64;
   INDEX_BYTES : constant := 4_096;

   --  Which grants a policy uses (docs/data-plane.md, "Policies"): the
   --  producer's data region always; the consumer's index page when the
   --  producer must see how far the consumer has read.
   function Needs_Index (Policy : Channel_Policy) return Boolean is
     (Policy in Lossless | Shed_Newest);

   --  A side's region pages: the control page and the ring, or the arena's
   --  buffers; a duplex side's region is its pages alone (its layout is
   --  the element's), the opener's or the acceptor's.
   function Region_Pages (Item : Contract) return Positive is
     (case Item.Kind is
        when Queue  => 1 + Item.Pages,
        when Arena  => 1 + Item.Pages * Item.Buffers,
        when Duplex => Item.Pages)
   with Pre => Valid (Item);
   function Acceptor_Region_Pages (Item : Contract) return Positive is
     (if Item.Kind = Duplex then Item.Buffers else Region_Pages (Item))
   with Pre => Valid (Item);

end CuBit.Channel_Protocol;
