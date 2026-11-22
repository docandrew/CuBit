------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The machine's raw copy speeds next to the frame numbers (timing builds):
--  once, when the first scanout buffer is lent, 1 MiB copied private to
--  private and private to the lent buffer, and one read pass over the lent
--  buffer, each as ns per byte (best of a few runs, TSC timed).
--
--  @description
--  "DESKTOP-MEMORY-BENCH: bytes=N private_to_private=X.XXX
--  private_to_lent=X.XXX lent_read=X.XXX (ns/byte)". The lent buffer
--  written is one not on screen; its contents are drawn over before it is
--  ever shown (a fresh buffer is fully repainted). Off outside timing
--  builds.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;

package Desktop_Memory_Bench is

   Bench_Bytes : constant := 1_048_576;
   Runs : constant := 8;

   --  Lent: a lent scanout buffer not on screen, Lent_Bytes long.
   procedure Run_Once (Lent : System.Address; Lent_Bytes : Natural);

end Desktop_Memory_Bench;
