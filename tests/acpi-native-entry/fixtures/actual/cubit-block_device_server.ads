------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A synchronous Block.Device.V1 server over one driver's transfer and flush
--  primitives (HW-001 step 1). The driver keeps its receive loop and replies
--  with the Response this builds.
--
--  The driver is only ever asked for a transfer that Decode accepted
--  (Valid_Transfer, as the formal's precondition) with the caller's grant
--  held for exactly Bytes (Item), and only for a flush it advertised.
--  Drivers that complete later use CuBit.Block_Requests directly: Decode,
--  Admit, then Settle when the device finishes.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages;
with CuBit.Block_Requests; use CuBit.Block_Requests;

generic
   --  Moves Item.Count blocks between the device and Buffer, which maps
   --  Bytes (Item) of the caller's grant, and reports the bytes moved.
   with procedure Transfer
     (State  : Device_State;
      Item   : Transfer_Request;
      Buffer : System.Address;
      Moved  : out Unsigned_64)
     with Pre => Valid_Transfer (State, Item);
   --  Makes every completed write durable.
   with procedure Flush (State : Device_State; Durable : out Boolean)
     with Pre => Flushes (State);
package CuBit.Block_Device_Server with SPARK_Mode is
   procedure Handle
     (State    : Device_State;
      Sender   : CuBit.Messages.Process_ID;
      Request  : CuBit.Messages.Message;
      Response : out CuBit.Messages.Message)
     with Post => Response.tag.length in 1 | 4;
end CuBit.Block_Device_Server;
