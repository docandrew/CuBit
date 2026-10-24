------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Block.Device.V1 for the identified unit, asynchronous: a request takes
--  a free transfer slot (CuBit.Block_Requests checks it and holds the
--  caller's grant), its reply is saved, and it is answered when the slot
--  completes or times out. New requests are taken only while a slot is
--  free, so the kernel queue holds the rest (no request is dropped).
--
--  Read only: the device is offered with FEATURE_READ_ONLY, so writes are
--  refused before any command is built (the laptop's disk holds Windows;
--  writing waits for the user's consent, HW-001).
------------------------------------------------------------------------------
with UFS_Identify;

package UFS_Service is
   procedure Start (Device : UFS_Identify.Identity);

   --  Completes finished slots, expires late ones, takes new requests.
   procedure Poll (Progressed : out Boolean);

   --  Some request is in flight (the caller should not sleep for long).
   function Outstanding return Boolean;
end UFS_Service;
