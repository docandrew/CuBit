------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The boot medium's read-only Block.Device.V1 endpoint: one deferred
--  read at a time, through private USB DMA (Storage_Requests routes its
--  requests here).
------------------------------------------------------------------------------
with CuBit.Messages;

package Optical_Service is
   --  Answers the deferred read once its data arrived.
   procedure Complete (Progressed : out Boolean);
   --  A read is in flight.
   function Busy return Boolean;
   --  Takes one request from the filesystem (already received).
   procedure Serve (Sender : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message)
     with Pre => not Busy;
end Optical_Service;
