------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's audio (SCHED-001, app audio threads): the stream to the
--  mixer, the refill that synthesizes into its ring, and the thread that
--  runs the refill in a real-time reservation the manifest declares, so a
--  slow frame drops a frame and never a period.
--
--  @description
--  The window's thread renders frames (in software, 14-50 ms each under
--  QEMU on a laptop) and steps the game; when it also refilled the ring, a
--  slow frame or a busy CPU left the ring empty and the mixer underran.
--  The audio thread wakes every Interval_Ms, takes the game lock and tops
--  the ring up. It asks for its reservation first (SET_LATENCY_CONTRACT
--  names the calling thread); without one it still runs, as an ordinary
--  thread.
--
--  The game lock guards everything both threads touch: the game record
--  (the composer and synthesizer the refill drives, the simulation the
--  window's thread steps and draws) and the state below. The window's
--  thread holds it while it steps the game and builds a frame's scene,
--  not while it rasterizes.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Audio;
with CuBit.Handles;
with Neon_Game;
with Neon_Synth;

package Neon_Audio is

   type Game_Ref is access all Neon_Game.Game;

   --  Keep this many frames queued in the ring (37.3 ms at 48 kHz, of its
   --  2048): enough to ride out a software frame (12-16 ms under QEMU)
   --  plus the loop's wake-ups; 1024 underran under QEMU.
   AUDIO_TARGET_FRAMES : constant := 1_792;
   --  How often the device queue depth is asked of the mixer (an IPC call).
   PLAYBACK_POLL_US : constant := 50_000;

   --  How often the audio thread refills: well inside the ring's 37 ms.
   Interval_Ms : constant := 2;
   --  Its reservation: Budget_Us of every Period_Us (a fifth of a CPU; the
   --  synthesizer takes about a tenth, more in the boss fight). The
   --  manifest's (request-scheduling synth ...) declares the same share.
   Budget_Us : constant := 2_000;
   Period_Us : constant := 10_000;

   --  The state the refill keeps (under the game lock).
   Game : Game_Ref;
   Stream : CuBit.Audio.StreamHandle;
   Audio_On : Boolean := False with Atomic;
   Device_Frames : Natural := 0;
   Last_Playback_Us : Unsigned_64 := 0;
   Underruns_Now : Unsigned_32 := 0;
   Queued_Now : Natural := 0;
   Mix_Left, Mix_Right : Neon_Synth.Block;
   --  Synthesis cost: microseconds spent rendering and frames rendered
   --  since the last stats line, and the worst call scaled to a full block.
   Synth_Us, Synth_Frames : Unsigned_64 := 0;
   Synth_Worst_Us_Per_Block : Float := 0.0;

   --  Render the next Frames of the soundtrack into Mix_Left/Mix_Right.
   procedure Synthesize (Frames : Natural);
   --  Top the open stream's ring up to AUDIO_TARGET_FRAMES.
   procedure Refill (Now : Unsigned_64);

   --  Start the audio thread. Authority: the manifest's scheduling grant,
   --  if Granted. Started False: no thread (the window's thread refills).
   procedure Start
     (Authority : CuBit.Handles.Scheduling; Granted : Boolean; Started : out Boolean);
   function Running return Boolean;
   --  The kernel admitted the thread's reservation.
   function Realtime return Boolean;

   procedure Lock;
   procedure Unlock;

end Neon_Audio;
