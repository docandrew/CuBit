-- Test-only SMP boot entry; never part of the normal kernel source tree.
package Spinlock_Benchmark is
    procedure Run (CPU : Natural; Participants : Natural) with No_Return;
end Spinlock_Benchmark;
