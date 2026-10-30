------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Each refusal logged once, naming who was refused and which declaration
--  fell short (CAP-003, docs/declaration-checking.md section 5).
--
--  @description
--  A service that refuses a request (config.svc a key outside the caller's
--  scopes, logstore a publication without the publisher badge, procmgr a
--  startup entry it cannot grant) calls Warn_Once. The first refusal with a
--  given Key becomes one Warning record (CuBit.Log: the console, and
--  logstore when the service has it), always spelled
--  "<service>: Warning: refused <what> (CAP-003)", which the startup-health
--  test looks for; repeats are only counted. Key names the refused party
--  and the declaration, for example "41 read gpu.power.waitboost". The
--  table is bounded: past Capacity distinct keys, refusals are counted,
--  and that is said once.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Refusal_Notes is

   --  Distinct refusals remembered, and the bytes of each Key compared.
   Capacity  : constant := 64;
   Key_Bytes : constant := 96;

   --  Service: who refuses ("config"). What: who was refused and what
   --  for, and why, in words.
   procedure Warn_Once (Service : String; Key : String; What : String);

   --  Refusals not logged because they were seen before (or the table was
   --  full).
   function Repeated return Unsigned_64;

end CuBit.Refusal_Notes;
