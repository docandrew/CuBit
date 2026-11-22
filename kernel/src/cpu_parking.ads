-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Parking the other CPUs for a platform power transition (KERN-004).
--
-- @description
-- Park_Others asks every other CPU to stop and waits, boundedly, until each
-- online one has. A CPU stops where it next takes the reschedule
-- interrupt Park_Others broadcasts: the handler runs with interrupts off
-- and, since every spinlock (and the console's output lock) is taken with
-- interrupts off, it holds none. There it waits, interrupts off, until
-- Release. So the parking CPU never waits on a lock a
-- parked CPU holds, and a release resumes every CPU where it stopped.
--
-- One transition at a time: a second Park_Others while one is in progress
-- reports Busy and touches nothing.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Quiescent_Reclamation;

package CPU_Parking is
    subtype CPU_Index is Quiescent_Reclamation.CPU_Index;
    subtype CPU_Set is Quiescent_Reclamation.CPU_Set;

    type Outcome is (All_Parked, Some_Running, Busy);

    -- Parking is requested: the calling CPU must Hold.
    function Requested return Boolean with Inline;

    -- On a CPU other than the parking one, interrupts off: wait until
    -- Release. Returns at once on the parking CPU or when nothing is asked.
    procedure Hold (Self : CPU_Index);

    -- From the parking CPU (Self), interrupts off: ask every other CPU in
    -- Online to stop and wait until each has, at most Wait_Iterations polls.
    -- Some_Running: the bound passed first (the caller may go on; those CPUs
    -- still stop when they next can). Busy: another transition holds it.
    procedure Park_Others (Self : CPU_Index; Online : CPU_Set; Result : out Outcome);

    -- After Park_Others (not Busy): every parked CPU resumes.
    procedure Release;

    -- How many CPUs Park_Others saw stop (for the console line).
    function Parked_Count return Natural;

    Wait_Iterations : constant := 100_000_000;
end CPU_Parking;
