------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The reader's side of the clock publication's seqlock: copy the
--  parameters between two counter loads, keep them only if Stable, and
--  give up after Read_Attempts tries (the caller then uses its system
--  call). Each runtime instantiates it with its page loads and RDTSC;
--  tests/fast-clock instantiates it against a racing writer.
--
--  The actuals must keep program order: Sequence and Fields are volatile
--  loads, and Counter is ordered after the loads before it and before the
--  load after it (LFENCE; RDTSC; LFENCE with a memory clobber).
------------------------------------------------------------------------------
pragma Ada_2022;

generic
   with function Sequence return Unsigned_64;
   with function Fields return Parameters;
   with function Counter return Unsigned_64;
procedure Clock_Publication.Sample
  (Nanoseconds : out Unsigned_64; Success : out Boolean)
with SPARK_Mode, Post => (if not Success then Nanoseconds = 0);
