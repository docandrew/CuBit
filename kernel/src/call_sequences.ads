-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Call sequences (docs/ipc-fastpath.md, "Call deadlines"): which answer a
-- caller accepts.
--
-- @description
-- Each thread counts its synchronous calls. A call's sequence travels with
-- the request and is stamped into its reply capability. A caller may time
-- out and start another call before the server answers, so an answer (a
-- reply, or the kernel failing the call because the server died) is
-- delivered only to the call it was stamped for, while its caller still
-- waits on it.
--
-- Rollover: a thread whose count reaches the maximum makes no more calls.
-- The name is retired, never wrapped (docs/process-objects.md, "A 64-bit
-- identity, with rollover handled"), so a stamp is never reused.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Call_Sequences with
    SPARK_Mode => On,
    Pure
is
    subtype Sequence is Unsigned_64;

    function Can_Begin (Current : Sequence) return Boolean is
      (Current < Sequence'Last);

    -- The sequence of the call a thread begins; every earlier call's
    -- sequence is below it.
    function Next (Current : Sequence) return Sequence is (Current + 1)
      with Pre  => Can_Begin (Current),
           Post => Next'Result > Current;

    -- An answer stamped Stamped is delivered to a caller whose latest call
    -- is Current only while the caller waits, and only if it answers that
    -- call.
    function Accepts (Waiting : Boolean; Current, Stamped : Sequence)
      return Boolean is (Waiting and then Current = Stamped);

    -- A late answer: once its caller has begun another call, an answer
    -- stamped for any earlier call is refused, whatever the caller does
    -- next (sequences only grow).
    procedure Earlier_Refused (Stamped, Current : Sequence; Waiting : Boolean)
      with Ghost,
           Pre  => Stamped < Current,
           Post => not Accepts (Waiting, Current, Stamped);

end Call_Sequences;
