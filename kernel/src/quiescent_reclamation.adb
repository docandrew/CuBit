package body Quiescent_Reclamation with SPARK_Mode => On is

    function Advanced (Current : Counters; CPU : CPU_Index) return Counters is
        Result : Counters := Current;
    begin
        Result (CPU) := Next_Count (Current (CPU));
        return Result;
    end Advanced;

    procedure Prove_Idle_CPU_Blocks (Current : Counters; CPU : CPU_Index;
                                     Online : CPU_Set) is
    begin
        -- The snapshot equals the current counter, so it is not exceeded.
        pragma Assert (not (Current (CPU) > Current (CPU)));
    end Prove_Idle_CPU_Blocks;

    procedure Prove_Monotonic (Current, Retired_At : Counters; CPU : CPU_Index) is
    begin
        pragma Assert (Next_Count (Current (CPU)) >= Current (CPU));
    end Prove_Monotonic;

    procedure Prove_Everyone_Advanced (Retired_At : Counters; Online : CPU_Set) is
        Current : Counters := Retired_At;
    begin
        for C in CPU_Index loop
            pragma Loop_Invariant
              (for all D in CPU_Index =>
                 (if D < C then Current (D) = Next_Count (Retired_At (D))
                  else Current (D) = Retired_At (D)));
            Current := Advanced (Current, C);
        end loop;
        pragma Assert (Grace_Elapsed (Current, Retired_At, Online));
    end Prove_Everyone_Advanced;

end Quiescent_Reclamation;
