-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Work-stealing eligibility policy (docs/threads.md)
--
-- Pure SPARK policy used by the kernel's ready-queue steal path. An idle CPU
-- may take a ready entry from another CPU's list only if it is ordinary work
-- (not an idle thread), unpinned, not being retired, not still executing
-- (switching out) on another CPU, and has waited at least the steal age.
-- The age keeps a freshly woken IPC partner on its CPU, preserving the
-- same-CPU direct IPC handoff.
--
-- Timestamps come from different CPUs' TSCs. A "now" earlier than the
-- queued stamp is treated as not aged, never as a wrapped, huge age.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Work_Stealing with Pure, SPARK_Mode => On is

    type Candidate is record
        Priority  : Integer;
        Pinned    : Boolean;
        Closing   : Boolean;
        Executing : Boolean;
        Queued_At : Unsigned_64;   -- TSC when the entry joined a ready list
    end record;

    -- Minimum age in TSC ticks, saturating instead of overflowing.
    function Min_Age (Ticks_Per_Microsecond : Unsigned_64;
                      Microseconds          : Unsigned_64) return Unsigned_64
    with
        Post =>
          (if Ticks_Per_Microsecond = 0 or else Microseconds = 0 then
               Min_Age'Result = 0
           elsif Microseconds <= Unsigned_64'Last / Ticks_Per_Microsecond then
               Min_Age'Result = Ticks_Per_Microsecond * Microseconds
           else
               Min_Age'Result = Unsigned_64'Last);

    -- True if Now is at least Min ticks after Queued_At. A Now before
    -- Queued_At (cross-CPU TSC skew) is never aged.
    function Aged (Queued_At, Now, Min : Unsigned_64) return Boolean is
      (Now >= Queued_At and then Now - Queued_At >= Min);

    -- The steal rule. Uncalibrated time (zero ticks per microsecond) makes
    -- nothing stealable.
    function Eligible (C                     : Candidate;
                       Now                   : Unsigned_64;
                       Ticks_Per_Microsecond : Unsigned_64;
                       Age_Microseconds      : Unsigned_64) return Boolean
    with
        Post =>
          Eligible'Result =
            (C.Priority >= 0 and then not C.Pinned and then
             not C.Closing and then not C.Executing and then
             Ticks_Per_Microsecond /= 0 and then
             Aged (C.Queued_At, Now,
                   Min_Age (Ticks_Per_Microsecond, Age_Microseconds)));

    ---------------------------------------------------------------------------
    -- Selection model. A ready list is ordered by descending priority. The
    -- kernel takes the first eligible entry in list order; this proves that
    -- entry has the highest priority among all eligible entries.
    ---------------------------------------------------------------------------
    subtype Index is Positive range 1 .. 512;
    type Priorities is array (Index range <>) of Integer;
    type Eligibility is array (Index range <>) of Boolean;

    function Sorted (P : Priorities) return Boolean is
      (for all I in P'Range =>
         (for all J in P'Range => (if I <= J then P (I) >= P (J))))
    with Ghost;

    -- First eligible index, or 0 if none.
    function First_Eligible (E : Eligibility) return Natural
    with
        Post =>
          (if First_Eligible'Result = 0 then
               (for all I in E'Range => not E (I))
           else
               First_Eligible'Result in E'Range and then
               E (First_Eligible'Result) and then
               (for all I in E'First .. First_Eligible'Result - 1 =>
                  not E (I)));

    procedure Prove_First_Is_Best (P : Priorities; E : Eligibility)
    with
        Ghost,
        Global => null,
        Pre    => P'First = E'First and then P'Last = E'Last and then
                  Sorted (P) and then First_Eligible (E) /= 0,
        Post   =>
          (for all I in E'Range =>
             (if E (I) then P (First_Eligible (E)) >= P (I)));

end Work_Stealing;
