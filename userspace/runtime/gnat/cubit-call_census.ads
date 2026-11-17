------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  This process's system calls, counted by call number (timing builds):
--  the census a loop takes at its phase marks to say which calls each phase
--  makes (IPC-009's call inventory).
--
--  @description
--  Off unless the process turns it on (Desktop's and the probe's timing
--  builds, display.svc's statistics); off, a call costs one load and a
--  branch more. Counts are per process and not atomic: a diagnostic, never
--  a decision.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Call_Census with Preelaborate is

   --  Call numbers counted (higher ones share the last slot).
   Last_Number : constant := 255;
   subtype Number is Natural range 0 .. Last_Number;
   type Counts is array (Number) of Unsigned_64;

   Enabled : Boolean := False;
   Census : Counts := [others => 0];

   procedure Note (Call : Unsigned_64) with Inline_Always;

end CuBit.Call_Census;
