------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The reader's step over the kernel log ring (Kernel_Log_Publication): the
--  records after Seen, oldest first, and an exact account of those it
--  could not take.
--
--  @description
--  Takes at most Budget records per call (More: some are left), so a reader
--  that starts late does not stop its own loop to catch up. Each number
--  after Seen up to the last one taken is either delivered (its slot held
--  it before and after the copy) or counted in a dropped run, reported as
--  Dropped (First, Last) when the run ends: overwritten by the kernel
--  before the reader got to it, or while it copied it. Nothing is counted
--  twice and nothing is skipped: Accounted, the delivered and dropped
--  records of this call, is exactly how far Seen moved.
--
--  The loads are the instance's: volatile reads of the mapped area.
------------------------------------------------------------------------------
pragma Ada_2022;

generic
   with function Load_Header return Header;
   with function Load_Sequence (Where : Slot) return Unsigned_64;
   with function Load_Record (Where : Slot) return Log_Record;
   with procedure Deliver (Number : Record_Number; Item : Log_Record);
   with procedure Dropped (First, Last : Record_Number);
procedure Kernel_Log_Publication.Drain
  (Seen      : in out Unsigned_64;
   Budget    : Record_Capacity;
   Accounted : out Unsigned_64;
   More      : out Boolean)
with SPARK_Mode,
     Post => Seen >= Seen'Old and then Accounted = Seen - Seen'Old;
