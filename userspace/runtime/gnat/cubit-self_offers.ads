------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The offerable self endpoint (Request.Offerable_Self, UI-020).
--
--  @description
--  The kernel gives every process an endpoint to itself ("self") that is
--  read-write and cannot be passed on (no RIGHT_GRANT). A program that must
--  let another process deliver grants to it (Desktop: display.svc derives
--  scanout loans for it) declares Request.Offerable_Self; the launcher
--  installs, at spawn and only then, a copy of its endpoint to the child
--  with Read and Grant and nothing else. Read without Write cannot call
--  (a call needs READ_WRITE): a holder can only derive or create grants
--  for the program. Grant lets the program offer one Read-only copy
--  (CuBit.Handles.IPC.Call_Offering). A launched child may declare it only
--  if its launcher holds one (attenuation).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
package CuBit.Self_Offers with Pure, SPARK_Mode is
   --  Its .cubit.caps request kind (after CuBit.Publication_Grants' 14).
   Manifest_Request : constant Unsigned_8 := 15;
   --  Its rights field in .cubit.caps: Read_Only (the encoding's 1); the
   --  launcher adds Grant and nothing else.
   Manifest_Rights : constant Unsigned_8 := 1;
end CuBit.Self_Offers;
