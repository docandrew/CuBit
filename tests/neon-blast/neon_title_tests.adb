------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted: the title's pickers are edges. One press steps the seed,
--  the start choice, the kept list and the bestiary once, however many
--  simulation steps the turn that carries it takes (a turn of 10 steps
--  once stepped the seed 10 times). Not a CuBit result.
------------------------------------------------------------------------------
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Game;
with Neon_Sim;
with Neon_Style;

procedure Neon_Title_Tests is
   use type Neon_Sim.Phase_Kind;
   type Game_Access is access Neon_Game.Game;
   G : constant Game_Access := new Neon_Game.Game;
   Turn_Steps : constant := 10;
   Failed : Boolean := False;
   procedure Check (Ok : Boolean; What : String) is
   begin
      Put_Line ("NEON-TITLE: " & (if Ok then "PASS " else "FAIL ") & What);
      Failed := Failed or else not Ok;
   end Check;
   Start_Seed : Unsigned_64;
begin
   Neon_Game.Start (G.all, 7);
   Start_Seed := G.Sim.Playlist;
   Neon_Game.Advance (G.all, (Seed_Step => 1, Choice_Step => 1, Keep => True, Bestiary => True,
                              others => <>),
                      Neon_Game.Sim_Time (G.all) + Turn_Steps * Tick_Samples);
   Check (G.Sim.Playlist = Start_Seed + 1, "one press steps the seed once");
   Check (G.Sim.Choice = 1, "one press steps the start once");
   Check (G.Sim.Favourites_Kept = 1 and then G.Sim.Favourites (1) = Start_Seed + 1,
          "one press keeps the seed once");
   Check (G.Sim.Bestiary, "one press opens the bestiary");
   Neon_Game.Advance (G.all, (Seed_Step => -1, Keep => True, others => <>),
                      Neon_Game.Sim_Time (G.all) + Turn_Steps * Tick_Samples);
   Check (G.Sim.Playlist = Start_Seed and then G.Sim.Favourites_Kept = 2,
          "back once, and that seed kept too");
   Check (Start_Seed = Neon_Style.Presets (Neon_Style.Lo_Fi), "the title opens on the lo-fi preset");
   Check (G.Sim.Phase = Neon_Sim.Title, "still on the title");
   if Failed then
      Set_Exit_Status (Failure);
   end if;
end Neon_Title_Tests;
