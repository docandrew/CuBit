--  A queue pair with 4 submission and 2 completion slots, so that the
--  completion ring is the limit and wrap-around is cheap to exercise.
pragma SPARK_Mode;
with Interfaces;
with CuBit.Submission_Queues;
package Queue_Small is new CuBit.Submission_Queues
  (Request => Interfaces.Unsigned_32, Result => Interfaces.Unsigned_32,
   Submission_Bits => 2, Completion_Bits => 1);
