------------------------------------------------------------------------------
--  A writer task updating one CPU record on another CPU while the reader
--  generic reads it (main.adb). Every version the writer finishes keeps
--  Thread = 2 * Idle, Scheduler = 3 * Idle and Dispatches = Idle, so a torn
--  read shows up as a broken ratio. Run_Unprotected reads the same record
--  without the seqlock (a mutated reader): it must see torn reads, or the
--  test could not tell.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Racing_Writer is
   procedure Run (Torn : out Unsigned_64; Stable : out Natural);
   procedure Run_Unprotected (Torn : out Unsigned_64);
end Racing_Writer;
