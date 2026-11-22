------------------------------------------------------------------------------
--  Test-only model of one process's threads and their thread-count slots
--  (kernel Process.createThread / exitThread over Thread_Slots). Proves
--  that every live thread owns a taken slot and no two live threads own
--  the same one, so each count line has a single writer.
------------------------------------------------------------------------------
pragma Ada_2022;
with Readiness_Publication;
with Thread_Slots;

package Slot_Protocol with SPARK_Mode => On is
   package RP renames Readiness_Publication;
   use type RP.Thread_Slot;

   --  Thread table entries of the model (more than the slots, so a full
   --  slot set is reachable).
   Model_Threads : constant := RP.Maximum_Threads + 8;
   type Thread is range 0 .. Model_Threads - 1;
   Main : constant Thread := 0;

   type Live_Set is array (Thread) of Boolean;
   type Slot_Map is array (Thread) of Thread_Slots.Slot;

   type State is record
      Slots : Thread_Slots.Slot_Set;
      Live  : Live_Set;
      Owner : Slot_Map;   --  meaningful for live threads
   end record;

   function Inv (S : State) return Boolean is
     (S.Live (Main)
      and then S.Owner (Main) = RP.Main_Thread_Slot
      and then (for all T in Thread => (if S.Live (T) then S.Slots (S.Owner (T))))
      and then (for all T in Thread =>
                  (for all U in Thread =>
                     (if T /= U and then S.Live (T) and then S.Live (U)
                      then S.Owner (T) /= S.Owner (U)))));

   --  A new process: its main thread in slot 0.
   function Initial return State with
     Post => Inv (Initial'Result);

   --  THREAD_CREATE for table entry T (not live): it takes the lowest free
   --  slot, or is refused with nothing changed.
   procedure Create (S : in out State; T : Thread; Created : out Boolean) with
     Pre  => Inv (S) and then not S.Live (T),
     Post => Inv (S)
             and then (if Created then S.Live (T) and then not S.Slots'Old (S.Owner (T))
                       else S = S'Old);

   --  THREAD_EXIT of T (live, not the main thread): its slot is free again.
   procedure Finish (S : in out State; T : Thread) with
     Pre  => Inv (S) and then S.Live (T) and then T /= Main,
     Post => Inv (S) and then not S.Live (T)
             and then not S.Slots (S'Old.Owner (T));

   --  The single-writer property, for any two live threads.
   function One_Writer (S : State; T, U : Thread) return Boolean is
     (T = U or else not S.Live (T) or else not S.Live (U)
      or else S.Owner (T) /= S.Owner (U));
   procedure Prove_One_Writer (S : State; T, U : Thread) with
     Ghost, Pre => Inv (S), Post => One_Writer (S, T, U);
end Slot_Protocol;
