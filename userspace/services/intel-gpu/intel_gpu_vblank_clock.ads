with Interfaces;
--  Latch-wait arithmetic for polled flip completion (DISP-000a, until the
--  display interrupt exists: DISP-000-irq). Pure, no MMIO.
--
--  PIPE_FRMCOUNT (0x70040 + 0x1000 * pipe; i915 i915_reg.h,
--  intel_vblank.c g4x_get_vblank_counter) is a wrapping 32-bit frame
--  counter. The driver notes the microsecond time at which it first saw
--  the counter change and the frame period it measured, and polls
--  PLANE_SURFLIVE just after the next predicted vblank instead of spinning.
package Intel_GPU_Vblank_Clock with SPARK_Mode is
   use Interfaces;

   --  Accepted refresh range: 24 .. 240 Hz.
   Minimum_Period_Us : constant := 4_166;
   Maximum_Period_Us : constant := 41_667;
   Default_Period_Us : constant := 16_667;
   subtype Period is Unsigned_64 range Minimum_Period_Us .. Maximum_Period_Us;
   --  Monotonic microseconds far from wrapping (about 146 000 years).
   Time_Limit : constant Unsigned_64 := 2 ** 62;
   subtype Time_Us is Unsigned_64 range 0 .. Time_Limit;

   --  Frames between two counter readings, across the wrap.
   function Frames_Between (Earlier, Later : Unsigned_32) return Unsigned_32 is
     (Later - Earlier);

   --  A flip armed at frame Armed_At and seen latched at frame Seen_At was
   --  late if more than one vblank passed (it latches at the first).
   function Late (Armed_At, Seen_At : Unsigned_32) return Boolean is
     (Frames_Between (Armed_At, Seen_At) > 1);

   --  The measured period over Frames frames that took Elapsed_Us, or the
   --  default when it is outside the accepted range.
   function Measure (Elapsed_Us : Unsigned_64; Frames : Unsigned_32) return Period
     with Post => (if Frames > 0 and then Elapsed_Us / Unsigned_64 (Frames) in Period
                   then Measure'Result = Elapsed_Us / Unsigned_64 (Frames)
                   else Measure'Result = Default_Period_Us);

   --  The next predicted vblank: one period after the last observed one
   --  when that is still ahead, else (a stale observation) one period from
   --  now. Always strictly after Now and never more than one period away.
   function Next_Vblank (Last_Vblank, Now : Time_Us; Each : Period) return Time_Us
     with Pre => Last_Vblank <= Now and then Now <= Time_Limit - 2 * Maximum_Period_Us,
          Post => Next_Vblank'Result > Now and then
                  Next_Vblank'Result - Now <= Each and then
                  (if Now - Last_Vblank < Each then
                     Next_Vblank'Result = Last_Vblank + Each);

   --  Poll a little after the vblank, when the latch has happened.
   Latch_Margin_Us : constant := 500;
   function Poll_At (Last_Vblank, Now : Time_Us; Each : Period) return Time_Us is
     (Next_Vblank (Last_Vblank, Now, Each) + Latch_Margin_Us)
     with Pre => Last_Vblank <= Now and then Now <= Time_Limit - 2 * Maximum_Period_Us,
          Post => Poll_At'Result > Now and then
                  Poll_At'Result - Now <= Each + Latch_Margin_Us;
end Intel_GPU_Vblank_Clock;
