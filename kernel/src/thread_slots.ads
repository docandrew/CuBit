-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Which thread-count line (Readiness_Publication.Thread_Slot) each
-- of a process's live threads owns.
--
-- Pure SPARK rules (tests/thread-slots/README.md). A process's threads write
-- their system-call counts into lines of its thread-count pages, one line
-- per slot. The main thread holds Main_Thread_Slot from the start; a new
-- thread takes the lowest free slot (Take) and gives it back when it exits
-- (Give_Back). The kernel keeps Taken_Count = the process's live threads
-- (Process.threadCount), both changed under Process.lock in the same
-- critical section, so a thread that passed the thread limit always finds
-- a free slot. The proved model (tests/thread-slots) shows that no two live
-- threads ever own the same slot, so each line has one writer.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Readiness_Publication;

package Thread_Slots with SPARK_Mode => On, Pure is
    package RP renames Readiness_Publication;
    use type RP.Thread_Slot;

    subtype Slot is RP.Thread_Slot;
    type Slot_Set is array (Slot) of Boolean;   -- True: a live thread owns it

    -- A new process: only its main thread.
    Main_Only : constant Slot_Set :=
      [RP.Main_Thread_Slot => True, others => False];

    function Taken_Count (Set : Slot_Set) return Natural with
        Post => Taken_Count'Result <= RP.Maximum_Threads;

    -- The lowest free slot, now taken; Taken False (Set unchanged) when
    -- every slot is taken.
    procedure Take (Set : in out Slot_Set; S : out Slot; Taken : out Boolean) with
        Post => (if Taken then
                   not Set'Old (S)
                   and then Set = (Set'Old with delta S => True)
                   and then (for all T in Slot'First .. S => (if T /= S then Set'Old (T)))
                 else
                   Set = Set'Old and then (for all T in Slot => Set'Old (T)));

    -- An exiting thread's slot, free again.
    procedure Give_Back (Set : in out Slot_Set; S : Slot) with
        Pre  => Set (S),
        Post => Set = (Set'Old with delta S => False);
end Thread_Slots;
