------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's autopilot: deterministic controls computed from the game
--  state, for the replay benchmark and the headless runs (no person, no
--  devices). It starts a game, strafes, aims at the nearest enemy, fires
--  and dashes away from danger. A replay is the seed plus this pilot, so
--  every run of the same build produces the same frames and samples.
------------------------------------------------------------------------------
with Neon_Sim;
with Neon_Time; use Neon_Time;

package Neon_Autopilot is
   --  Controls for the simulation step at song time Now.
   function Drive (W : Neon_Sim.World; Now : Samples) return Neon_Sim.Controls;
end Neon_Autopilot;
