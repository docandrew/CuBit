--  The tear-free flip rule for one primary plane (docs/display-modesetting.md
--  2.2, DISP-000a): which of the three scanout buffers may be written, and
--  when a flip has completed. Pure, no MMIO.
--
--  Hardware facts (Linux v6.16 i915, facts only): a PLANE_SURF write arms
--  the double-buffered update, which latches at the next start of vblank
--  (skl_universal_plane.c icl_plane_update_arm; intel_atomic_plane.c);
--  PLANE_SURFLIVE reports the surface being scanned out
--  (skl_universal_plane_regs.h). Only the latest SURF value can latch.
--
--  The driver tracks:
--  - Armed: the surface last written to PLANE_SURF;
--  - Seen: the surface PLANE_SURFLIVE reported at the last observation,
--    which is always taken after the last SURF write;
--  - Maybe: surfaces armed and then replaced since that observation (one
--    of them may have latched between the observation and its replacement).
--  From the facts above, the surface the display engine scans out now or
--  next is always Seen, Armed or one of Maybe ("possibly live"). A buffer is
--  writable only when it is none of these, so a written buffer is never on
--  screen. This hardware assumption is not provable here; the register model
--  (tests/intel-gpu/primary_flip_tests.adb) exercises it with random latch
--  timing.
--
--  Presentation is a mailbox of depth one: at most one flip is owed a
--  completion. Arming a newer buffer while one is owed supersedes the owed
--  frame, which the caller answers at once; every armed frame is therefore
--  answered exactly once, latched or superseded, never lost.
package Intel_GPU_Flip_State with SPARK_Mode is
   type Source is (Firmware, Buffer_1, Buffer_2, Buffer_3);
   subtype Buffer is Source range Buffer_1 .. Buffer_3;
   type Buffer_Set is array (Buffer) of Boolean;

   type State is record
      Armed : Source := Firmware;
      Seen : Source := Firmware;
      Maybe : Buffer_Set := [others => False];
      Owed : Boolean := False;
      Faulted : Boolean := False;
   end record;

   --  The owed frame is the armed buffer, not yet seen live.
   function Valid (S : State) return Boolean is
     (if S.Owed then S.Armed in Buffer and then S.Seen /= S.Armed);

   function Possibly_Live (S : State; B : Buffer) return Boolean is
     (S.Armed = B or else S.Seen = B or else S.Maybe (B));
   function Writable (S : State; B : Buffer) return Boolean is
     (not S.Faulted and then not Possibly_Live (S, B));

   --  A writable buffer other than Except (Firmware: no exception).
   function Has_Writable (S : State; Except : Source) return Boolean is
     (for some B in Buffer => B /= Except and then Writable (S, B));
   function First_Writable (S : State; Except : Source) return Buffer
     with Pre => Has_Writable (S, Except),
          Post => First_Writable'Result /= Except and then
                  Writable (S, First_Writable'Result);

   --  Arm B (the caller writes PLANE_SURF := B). Superseded: the frame that
   --  was owed is replaced by this newer one and must be answered now.
   procedure Arm (S : in out State; B : Buffer; Superseded : out Boolean)
     with Pre => Valid (S) and then Writable (S, B),
          Post => Valid (S) and then S.Armed = B and then S.Owed and then
                  S.Seen = S'Old.Seen and then not S.Faulted and then
                  Superseded = S'Old.Owed and then
                  (for all C in Buffer =>
                     S.Maybe (C) =
                       (S'Old.Maybe (C) or else
                        (C = S'Old.Armed and then C /= S'Old.Seen))) and then
                  --  Nothing that was possibly live stops being so.
                  (for all C in Buffer =>
                     (if Possibly_Live (S'Old, C) then Possibly_Live (S, C)));

   --  Return to the firmware surface (takeover abandoned): nothing is owed.
   procedure Arm_Firmware (S : in out State; Superseded : out Boolean)
     with Pre => Valid (S),
          Post => Valid (S) and then S.Armed = Firmware and then not S.Owed and then
                  S.Seen = S'Old.Seen and then Superseded = S'Old.Owed and then
                  S.Faulted = S'Old.Faulted and then
                  (for all C in Buffer =>
                     (if Possibly_Live (S'Old, C) then Possibly_Live (S, C)));

   --  A PLANE_SURFLIVE reading taken after the last SURF write. A surface
   --  that cannot be live faults the state (no buffer is writable again).
   --  Latched: the owed frame is now on screen and is answered.
   procedure Observe (S : in out State; Live : Source; Latched : out Boolean)
     with Pre => Valid (S),
          Post => Valid (S) and then
                  (if S'Old.Faulted or else
                      (Live /= S'Old.Seen and then Live /= S'Old.Armed and then
                       not (Live in Buffer and then S'Old.Maybe (Live)))
                   then S.Faulted and then not Latched and then S.Owed = S'Old.Owed
                   else not S.Faulted and then S.Seen = Live and then
                        S.Armed = S'Old.Armed and then
                        (for all C in Buffer => not S.Maybe (C)) and then
                        Latched = (S'Old.Owed and then Live = S'Old.Armed) and then
                        S.Owed = (S'Old.Owed and then not Latched));
end Intel_GPU_Flip_State;
