with Interfaces;
with Intel_GPU_Flip_State;
with Intel_GPU_Vblank_Clock;
--  Tear-free flips of the firmware pipe's primary plane between three
--  driver-owned scanout buffers (DISP-000a, docs/display-modesetting.md 2).
--  The firmware mode, size, format, stride and PLANE_CTL stay as the
--  firmware left them; only PLANE_SURF changes, one 32-bit write per flip,
--  latched by the display engine at the next vblank. Completion is polled:
--  PLANE_SURFLIVE shows the new surface (Intel_GPU_Flip_State.Observe),
--  with PIPE_FRMCOUNT for the vblank clock and late-latch counting.
--
--  The writable-buffer rule is Intel_GPU_Flip_State's: display.svc writes
--  only a buffer that is neither armed, last seen live, nor possibly
--  latched since; a Present naming any other buffer is refused and counted
--  as writes_to_live. The firmware framebuffer is never written.
generic
   --  A register of the pipe's page (offset within the page), all ones when
   --  unreadable; writes are gated by the caller on ownership and power.
   with function Read (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write (Offset, Value : Interfaces.Unsigned_32; Written : out Boolean);
   with function Flush (Address, Bytes : Interfaces.Unsigned_64) return Boolean;
   with function Now_Us return Interfaces.Unsigned_64;
   --  One bounded pause while a synchronous sequence waits for a vblank.
   with procedure Pause;
package Intel_GPU_Primary_Flip is
   use Interfaces;
   package FS renames Intel_GPU_Flip_State;

   Buffer_Count : constant := 3;
   type Index is range 0 .. Buffer_Count - 1;
   function To_Buffer (Item : Index) return FS.Buffer is
     (FS.Buffer'Val (FS.Buffer'Pos (FS.Buffer'First) + Integer (Item)));

   type GGTT_Bases is array (Index) of Unsigned_32;
   type CPU_Addresses is array (Index) of Unsigned_64;

   --  Firmware_Surface is the firmware's live GGTT surface; offset 0 is a
   --  valid surface (GOP framebuffers commonly sit there).
   procedure Configure
     (Bases : GGTT_Bases; CPU : CPU_Addresses;
      Width, Height, Pitch : Positive; Firmware_Surface : Unsigned_32);
   type Configure_Problem is
     (Not_Called, Firmware_Surface_Unaligned, Buffer_Unaligned,
      Buffer_Is_Firmware_Surface, Buffer_Not_Mapped, Buffers_Not_Distinct,
      Ready_To_Flip);
   function Problem_Name (Item : Configure_Problem) return String is
     (case Item is
        when Not_Called => "not-configured",
        when Firmware_Surface_Unaligned => "firmware-surface-unaligned",
        when Buffer_Unaligned => "buffer-unaligned",
        when Buffer_Is_Firmware_Surface => "buffer-equals-firmware-surface",
        when Buffer_Not_Mapped => "buffer-not-mapped",
        when Buffers_Not_Distinct => "buffers-not-distinct",
        when Ready_To_Flip => "ready");
   function Last_Problem return Configure_Problem;
   --  The failed precondition, the buffer it concerns and the firmware
   --  surface, for the log.
   function Problem_Text return String;

   type Takeover_Result is (Taken, Not_Configured, Write_Failed, Latch_Timeout,
                            Unexpected_Surface);
   --  Fill the three buffers with Color and flip from the firmware surface
   --  to buffer 0. On any failure the firmware surface is armed again and
   --  flips stay disabled for this driver instance.
   procedure Take_Over (Color : Unsigned_32; Result : out Takeover_Result);
   function Ready return Boolean;
   function Result_Name (Item : Takeover_Result) return String is
     (case Item is
        when Taken => "taken", when Not_Configured => "not-configured",
        when Write_Failed => "write-failed", when Latch_Timeout => "latch-timeout",
        when Unexpected_Surface => "unexpected-PLANE_SURFLIVE");

   type Submit_Result is (Armed, Not_Writable, Not_Ready, Write_Failed);
   --  Present buffer Item: display has written rows First_Row .. First_Row
   --  + Rows - 1. Flush them, then arm. Superseded: an owed frame was
   --  replaced (mailbox) and must be answered now.
   procedure Submit (Item : Index; First_Row, Rows : Natural;
                     Result : out Submit_Result; Superseded : out Boolean);
   function Owed return Boolean;

   --  One loop turn: while a flip is owed, read PLANE_SURFLIVE and
   --  PIPE_FRMCOUNT. Latched: the owed frame is on screen.
   procedure Service (Latched : out Boolean);
   --  Microseconds at which the next poll is useful (Unsigned_64'Last when
   --  nothing is owed).
   function Next_Poll_Us return Unsigned_64;

   --  Fill every buffer with Color and end with buffer 0 live, without
   --  writing a possibly-live buffer: if buffer 0 is live, flip to another
   --  filled buffer first. Synchronous, bounded by the latch deadline.
   procedure Clear (Color : Unsigned_32; Success : out Boolean);

   --  Read-only views for the executor.
   function Width return Positive;
   function Height return Positive;
   function Pitch return Positive;
   function Frame_Bytes return Unsigned_64;
   function CPU_Of (Item : Index) return Unsigned_64;

   type Counters is record
      Vblanks : Unsigned_32 := 0;
      Armed, Latched, Superseded, Late, Writes_To_Live, Faults : Natural := 0;
      Latency_Max_Us, Latency_Total_Us : Unsigned_64 := 0;
      Period_Us : Intel_GPU_Vblank_Clock.Period := Intel_GPU_Vblank_Clock.Default_Period_Us;
   end record;
   procedure Take (Item : out Counters);

   --  For the stall watchdog: the owed flip's buffer and age, and the
   --  registers as read now.
   type Stall_View is record
      Owed : Boolean := False;
      Faulted : Boolean := False;
      Armed : Index := 0;
      Owed_Us : Unsigned_64 := 0;
      Surface_Word, Live_Word, Frame_Count : Unsigned_32 := 0;
   end record;
   function Stall return Stall_View;

   --  Pipe page offsets (i915 v6.16 skl_universal_plane_regs.h, i915_reg.h).
   Surface_Offset      : constant Unsigned_32 := 16#19C#;
   Live_Surface_Offset : constant Unsigned_32 := 16#1AC#;
   Frame_Count_Offset  : constant Unsigned_32 := 16#040#;
   Surface_Page_Mask   : constant Unsigned_32 := 16#FFFF_F000#;
   --  Two vblanks at 24 Hz, with margin.
   Latch_Deadline_Us : constant := 250_000;
end Intel_GPU_Primary_Flip;
