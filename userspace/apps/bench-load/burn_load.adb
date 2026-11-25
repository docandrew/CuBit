--  SPDX-License-Identifier: GPL-3.0-or-later
--  bench-burn: a CPU-hungry ordinary program beside a game (SCHED-001, app
--  audio threads): it spins for Burn_Ms, as long as tests/neon-blast's run,
--  so Neon Blast's frames compete for CPU (tests/headless/run.sh
--  --test neon-blast with CUBIT_NEON_BURN=1).
with Load_Work;
procedure Burn_Load is
   Burn_Ms : constant := 900_000;
begin
   Load_Work (Burn_Ms);
end Burn_Load;
