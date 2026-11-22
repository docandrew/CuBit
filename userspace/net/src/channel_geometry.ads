------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where a stream or datagram channel's rings lie in the grant a client
--  lends netstack (CuBit.Net_Channel_Layout): the header, then the send
--  ring, then the receive ring. netstack reads the ring sizes once, at
--  open, and accepts them only if both rings fit the acquired grant.
--
--  Proved (tests/net-tcp): for accepted sizes, every ring position a
--  CuBit.Channel_Rings slice can name (below its ring's size) is an
--  offset inside the grant, so no access through a channel leaves the
--  memory netstack acquired.
------------------------------------------------------------------------------
package Channel_Geometry with SPARK_Mode, Pure is

   Header_Bytes : constant := 4_096;
   Maximum_Ring : constant := 1_048_576;
   Maximum_Grant : constant := Header_Bytes + 2 * Maximum_Ring;

   subtype Grant_Bytes is Natural range 0 .. Maximum_Grant;
   subtype Ring_Bytes is Natural range 0 .. Maximum_Ring;

   --  Both rings (non-empty) fit after the header.
   function Fits (Grant : Grant_Bytes; Tx, Rx : Ring_Bytes) return Boolean is
     (Tx > 0 and then Rx > 0 and then Grant >= Header_Bytes and then
      Tx + Rx <= Grant - Header_Bytes);

   --  The grant offset of position Pos of the send ring.
   function Send_Offset (Grant : Grant_Bytes; Tx, Rx : Ring_Bytes; Pos : Natural)
     return Natural
   is (Header_Bytes + Pos)
   with Pre  => Fits (Grant, Tx, Rx) and then Pos < Tx,
        Post => Send_Offset'Result < Grant;

   --  The grant offset of position Pos of the receive ring.
   function Receive_Offset (Grant : Grant_Bytes; Tx, Rx : Ring_Bytes; Pos : Natural)
     return Natural
   is (Header_Bytes + Tx + Pos)
   with Pre  => Fits (Grant, Tx, Rx) and then Pos < Rx,
        Post => Receive_Offset'Result < Grant and then
                Receive_Offset'Result >= Header_Bytes + Tx;

   --  A run of Length bytes from position Pos of a ring of Size bytes stays
   --  in the ring (what Channel_Rings' slices promise).
   function In_Ring (Pos, Length, Size : Natural) return Boolean is
     (Pos <= Size and then Length <= Size - Pos);

end Channel_Geometry;
