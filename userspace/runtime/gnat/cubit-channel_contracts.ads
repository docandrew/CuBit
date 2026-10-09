------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What two peers agree on when they open a channel (docs/data-plane.md,
--  "The control plane"): the element type, the channel kind, the policy and
--  the capacity, and its encoding in an IPC message's words.
--
--  @description
--  The element type is a CuBit.Protocols.Schema_Contract. Its sizing picks
--  the framing: fixed-size elements travel as CuBit.Slot_Rings slots,
--  bounded ones as CuBit.Datagram_Rings records. A queue carries elements;
--  an arena is a region of Buffers buffers of Pages pages each, handed over
--  by a companion queue; a duplex channel is two regions, one owned by each
--  side (Pages the opener's, Buffers the acceptor's).
--
--  Wire form (three words; a fourth carries a grant reference):
--    0  the schema's identity
--    1  bits 0 .. 31 the element's wire size, 32 sizing (1 bounded),
--       48 .. 63 the schema version
--    2  bits 0 .. 7 kind, 8 .. 15 policy, 16 .. 23 ring pages (queues) or
--       buffer pages (arenas), 24 .. 39 buffers (arenas), 40 .. 47 the read
--       rule
--
--  Proved (tests/channel-contracts): Decode accepts exactly the words
--  Encode produces for valid contracts, and every contract it accepts is
--  valid.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Protocols; use CuBit.Protocols;

package CuBit.Channel_Contracts with Pure, SPARK_Mode is

   --  Duplex: each side owns a region and grants it, read-only, to the
   --  other, which maps it; each writes only its own (a request ring and
   --  its indices one way, an answer ring and its indices the other). The
   --  opener's region is Pages pages, the acceptor's Buffers pages. The
   --  element names the layout both follow.
   type Channel_Kind is (Queue, Arena, Duplex);
   for Channel_Kind use (Queue => 1, Arena => 2, Duplex => 3);

   --  docs/data-plane.md, "Policies".
   type Channel_Policy is (Lossless, Drop_Oldest, Shed_Newest);
   for Channel_Policy use (Lossless => 1, Drop_Oldest => 2, Shed_Newest => 3);

   --  docs/data-plane.md, "Reading in place": how a consumer may use an
   --  element (copy then validate, or in place).
   type Read_Rule is (Copy_Then_Validate, In_Place);
   for Read_Rule use (Copy_Then_Validate => 1, In_Place => 2);

   --  A queue's ring, or one arena buffer, in pages; an arena's buffers.
   subtype Ring_Pages is Positive range 1 .. 128;
   subtype Buffer_Count is Positive range 1 .. 4_096;
   Page_Bytes : constant := 4_096;
   --  The most pages one grant covers (the kernel's Memory_Grants
   --  Maximum_Page_Count): a valid contract's regions always fit one, so a
   --  contract that could never be granted is refused when decoded.
   Maximum_Region_Pages : constant := 4_096;
   Largest_Element : constant := 65_535;

   type Contract is record
      --  No schema (inline: the libc links no CuBit.Protocols objects).
      Element : Schema_Contract := (others => <>);
      Kind    : Channel_Kind := Queue;
      Policy  : Channel_Policy := Lossless;
      Pages   : Ring_Pages := 1;
      Buffers : Buffer_Count := 1;      --  arenas only; 1 for queues
      Rule    : Read_Rule := Copy_Then_Validate;
   end record;

   --  A queue's ring holds two elements at least (a record is at most half
   --  of it); an arena is lossless (buffers change hands, nothing is
   --  dropped); a queue's ring is a power of two of pages.
   function Power_Of_Two (Pages : Ring_Pages) return Boolean is
     (Pages in 1 | 2 | 4 | 8 | 16 | 32 | 64 | 128);

   function Valid (Item : Contract) return Boolean is
     (Valid (Item.Element)
      and then Item.Element.Wire_Size <= Largest_Element
      and then (case Item.Kind is
                  when Queue =>
                     Item.Buffers = 1 and then Power_Of_Two (Item.Pages)
                     and then Natural (Item.Element.Wire_Size) + 4
                                <= Item.Pages * Page_Bytes / 2,
                  when Arena =>
                     Item.Policy = Lossless
                     and then Natural (Item.Element.Wire_Size) <= Item.Pages * Page_Bytes
                     --  The control page and the buffers.
                     and then Item.Buffers <= (Maximum_Region_Pages - 1) / Item.Pages,
                  when Duplex =>
                     Item.Policy = Lossless and then Item.Buffers <= Ring_Pages'Last
                     and then Natural (Item.Element.Wire_Size) <= Item.Pages * Page_Bytes));

   Contract_Words : constant := 3;
   type Words is array (0 .. Contract_Words - 1) of Unsigned_64;

   function Encode (Item : Contract) return Words
   with Pre => Valid (Item);

   procedure Decode (Item : Words; Result : out Contract; Accepted : out Boolean)
   with Post => (if Accepted then Valid (Result) and then Encode (Result) = Item);

end CuBit.Channel_Contracts;
