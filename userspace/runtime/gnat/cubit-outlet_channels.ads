------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A program outlet as a channel (docs/data-plane.md; docs/ccl-streams.md,
--  "The ring underneath"): a broadcast Drop_Oldest queue whose region the
--  producer owns and grants, read-only, to each reader that opens it
--  (consuming, on the outlet's connector: its ring id). Shared by the Ada
--  runtime, the libc and readers, so all compute the same contract.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Contracts; use CuBit.Channel_Contracts;
with CuBit.Protocols;
with CuBit.Stream_Rings;

package CuBit.Outlet_Channels with Pure, SPARK_Mode is

   --  An outlet of Element records whose connector declares Declared pages.
   function Contract
     (Element : CuBit.Protocols.Schema_Contract;
      Declared : CuBit.Stream_Rings.Declared_Pages) return CuBit.Channel_Contracts.Contract
   is ((Element => Element, Kind => Queue, Policy => Drop_Oldest,
        Pages => CuBit.Stream_Rings.Ring_Pages (Declared), Buffers => 1,
        Rule => Copy_Then_Validate));

   --  What a reader offers before it knows the producer's ring: the
   --  producer answers with its own (CuBit.Channels.Accept_Shared).
   function Offer (Element : CuBit.Protocols.Schema_Contract) return CuBit.Channel_Contracts.Contract is
     (Contract (Element, 1));

end CuBit.Outlet_Channels;
