------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  CuBit.Block_Driver for a synchronous device: one slot, every command
--  finishes inside Transfer or Flush, so each request costs one receive and
--  one reply (DEV-002, docs/device-classes.md).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.Block_Work; use CuBit.Block_Work;
with CuBit.Device_Identities;

generic
   --  Whether writes need the write grant (no default: each driver says).
   Writes : Write_Admission;

   --  Moves Item between the device and Buffer (Bytes (Item) long, the
   --  caller's grant). Moved: bytes moved; anything short is a failure.
   with procedure Transfer
     (Item : Transfer_Request; Buffer : System.Address; Moved : out Unsigned_64);

   --  Commits the device's volatile write cache (only called when the
   --  device offers FEATURE_FLUSH).
   with procedure Flush (Durable : out Boolean);
package CuBit.Block_Sync_Driver is

   --  As CuBit.Block_Driver.Start.
   procedure Start
     (State    : Device_State;
      Identity : CuBit.Device_Identities.Identity)
     with Pre =>
       CuBit.Device_Identities.Well_Formed (Identity.Model)
       and then CuBit.Device_Identities.Well_Formed (Identity.Serial);

   --  Blocks until a request arrives and answers it.
   procedure Serve_One;
end CuBit.Block_Sync_Driver;
