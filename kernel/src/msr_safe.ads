-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary MSR accesses that survive a refusal (interrupt_handlers.asm)
--
-- A CPU may advertise a feature in CPUID yet refuse its MSR with #GP (the
-- N150 refused an HWP read despite its CPUID bit: image 27). Read and Write
-- here resume after such a #GP and report it instead of panicking. Use them
-- for every MSR whose presence rests only on a feature bit.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package Msr_Safe is

    -- The value written, or False when the CPU refused it (#GP).
    function Write (Register : Unsigned_32; Value : Unsigned_64) return Boolean;

    -- The value read, or Ok False (and Value 0) when the CPU refused it.
    procedure Read (Register : Unsigned_32; Value : out Unsigned_64; Ok : out Boolean);

    -- Boot check: an MSR no CPU implements faults and is recovered. True
    -- when it faulted (and this code is still running).
    function Absent_Register_Recovered return Boolean;

private
    function cubitWrmsrSafe (Register : Unsigned_64; Value : Unsigned_64) return Unsigned_64
      with Import, Convention => C, External_Name => "cubitWrmsrSafe";
    function cubitRdmsrSafe (Register : Unsigned_64; Into : System.Address) return Unsigned_64
      with Import, Convention => C, External_Name => "cubitRdmsrSafe";
end Msr_Safe;
