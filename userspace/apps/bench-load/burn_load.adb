--  SPDX-License-Identifier: GPL-3.0-or-later
--  bench-burn: a CPU-hungry ordinary program beside a game (SCHED-001, app
--  audio threads): after Start_After_Ms it spins for Burn_Ms, longer than
--  tests/neon-blast's run,
--  so Neon Blast's frames compete for CPU (tests/headless/run.sh
--  --test neon-blast with CUBIT_NEON_BURN=1).
with Interfaces; use Interfaces;
with CuBit.Waits;
with Load_Work;
procedure Burn_Load is
   --  The desktop and the game start first (with two burners on two CPUs
   --  from boot, the desktop missed the input injector's deadline).
   Start_After_Ms : constant := 30_000;
   Burn_Ms : constant := 900_000;
   Ignored : Unsigned_64;
begin
   Ignored := CuBit.Waits.Sleep_For (Start_After_Ms);
   Load_Work (Burn_Ms);
end Burn_Load;
