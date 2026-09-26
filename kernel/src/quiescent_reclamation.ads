-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Grace periods for freeing memory that lock-free readers may use
--
-- Pure SPARK rules (docs/threads.md, "Page reclamation waits for concurrent
-- readers"). Process and thread table lookups are lock-free, so a page of
-- records removed from its directory may still be in use by a reader on
-- another CPU. The page is freed only after every online CPU has passed a
-- quiescent point (where it holds no record pointer) since the removal.
--
-- Each CPU advances its own counter at quiescent points (Next_Count). The
-- adapter keeps one counter per CPU with atomic components, written only by
-- that CPU, so CPUs never contend or overwrite each other. Retiring a page
-- records a snapshot of the counters, taken after the page is unlinked. The
-- page may be freed once every online CPU's counter has moved past its
-- snapshot value (Grace_Elapsed). Unlike TLB_Reclamation, nothing waits:
-- retired pages are freed later, when their grace period has elapsed.
--
-- A counter at its maximum stops advancing; pages retired after that are
-- never freed (they leak) rather than being freed early.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Config;

package Quiescent_Reclamation with SPARK_Mode => On is
    subtype CPU_Index is Natural range 0 .. Config.MAX_SMP_CPUS - 1;
    type Count is new Unsigned_64;
    type Counters is array (CPU_Index) of Count;
    type CPU_Set is array (CPU_Index) of Boolean;

    -- A CPU's counter after it passes a quiescent point.
    function Next_Count (C : Count) return Count is
      (if C < Count'Last then C + 1 else Count'Last);

    -- Every online CPU has passed a quiescent point since Retired_At was
    -- snapshotted. An offline CPU holds no pointers. A CPU whose counter was
    -- already at the maximum can no longer show progress, so memory retired
    -- then is never released.
    function Grace_Elapsed (Current, Retired_At : Counters; Online : CPU_Set)
      return Boolean
    is
      (for all C in CPU_Index =>
         (not Online (C) or else
          (Current (C) > Retired_At (C) and then Retired_At (C) < Count'Last)));

    -- Counters never decrease.
    function Not_Before (Current, Retired_At : Counters) return Boolean is
      (for all C in CPU_Index => Current (C) >= Retired_At (C))
    with Ghost;

    -- One CPU advancing its own counter.
    function Advanced (Current : Counters; CPU : CPU_Index) return Counters
    with
        Ghost,
        Post => Advanced'Result (CPU) = Next_Count (Current (CPU)) and then
                (for all C in CPU_Index =>
                   (if C /= CPU then Advanced'Result (C) = Current (C)));

    -- An online CPU that has not advanced since the snapshot blocks the
    -- grace period: freeing cannot happen while that CPU may still hold a
    -- pointer.
    procedure Prove_Idle_CPU_Blocks (Current : Counters; CPU : CPU_Index;
                                     Online : CPU_Set)
    with
        Ghost,
        Global => null,
        Pre    => Online (CPU),
        Post   => not Grace_Elapsed (Current, Current, Online);

    -- Advancing keeps every earlier snapshot not-before the counters.
    procedure Prove_Monotonic (Current, Retired_At : Counters; CPU : CPU_Index)
    with
        Ghost,
        Global => null,
        Pre    => Not_Before (Current, Retired_At),
        Post   => Not_Before (Advanced (Current, CPU), Retired_At);

    -- Once every online CPU has advanced at least once past the snapshot
    -- (without saturating), the grace period has elapsed.
    procedure Prove_Everyone_Advanced (Retired_At : Counters; Online : CPU_Set)
    with
        Ghost,
        Global => null,
        Pre    => (for all C in CPU_Index => Retired_At (C) < Count'Last);
end Quiescent_Reclamation;
