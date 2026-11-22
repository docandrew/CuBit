------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One CPU's record read under its seqlock (CPU_Times_Publication): a copy
--  the kernel finished, with the interval in progress added at the
--  counter sampled between the two sequence loads.
--
--  @description
--  At most Read_Attempts tries; a writer that keeps the record busy (or a
--  page that is not published) gives Unavailable, so a reader never spins.
--  The loads are the instance's: volatile reads of the mapped page, and the
--  counter (RDTSC) fenced between them.
------------------------------------------------------------------------------
pragma Ada_2022;

generic
   with function Sequence (CPU : Processor) return Unsigned_64;
   with function Values (CPU : Processor) return Fields;
   with function Counter return Unsigned_64;
procedure CPU_Times_Publication.Sample (CPU : Processor; Result : out CPU_Reading)
with SPARK_Mode;
