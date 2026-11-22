--  LAT-002: composing against the next latch (tests/compositor/pacing).
--  The laptop's backend releases a frame at its latch, so with anything
--  animating one frame is always in flight. Composing at release makes an
--  input that arrives later in the refresh miss the next latch; composing
--  at Compose_At takes it in when the work fits.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Pacing; use Compositor_Pacing;
procedure Pacing_Tests is
   Period : constant := 16_667;
   Latch : constant := 1_000_000;
   Cost : constant := 4_000;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Ada.Text_IO.Put_Line ("PACING: FAIL " & What);
         raise Program_Error;
      end if;
   end Check;
   S : State;
   --  The latch a frame composed at Start makes: the first one at least
   --  Cost + Margin_Us after it (what the arm needs).
   function Made (Start : Unsigned_64) return Unsigned_64 is
     (Latch + ((Start + Cost + Margin_Us - Latch + Period - 1) / Period) * Period);
begin
   Check (Compose_At (S, 5_000) = 5_000, "no refresh known: compose now");
   Latched (S, Latch, 0);
   Check (Compose_At (S, Latch + 100) = Latch + 100, "a virtual GPU (no period): compose now");
   Latched (S, Latch, Period);
   Frame_Cost (S, Latch + 1_000, Latch + 1_000 + Cost);
   Check (S.Cost_Us = Cost, "the cost is the composition-to-arm time");
   Check (Next_Latch (S, Latch + 2_000) = Latch + Period, "the next latch");
   Check (Compose_At (S, Latch + 2_000) = Latch + Period - Cost - Margin_Us,
          "early in the refresh: wait for the last start that makes the next latch");
   Check (Compose_At (S, Latch + Period - 100) = Latch + Period - 100, "too late: compose now");
   Check (Compose_At (S, Latch + 9 * Period) = Latch + 9 * Period, "a stale latch: compose now");
   --  The laptop's case: the in-flight frame is released 1 ms after its
   --  latch; a key press is published 10 ms into the refresh.
   declare
      Release : constant Unsigned_64 := Latch + 1_000;
      Press : constant Unsigned_64 := Latch + 10_000;
      Old_Start : constant Unsigned_64 := Release;   --  composed at release
      New_Start : constant Unsigned_64 := Compose_At (S, Release);
   begin
      Check (Old_Start < Press and then Made (Old_Start) = Latch + Period,
             "composing at release closes the next latch's frame before the press");
      Check (New_Start >= Press and then Made (New_Start) = Latch + Period,
             "composing at Compose_At takes the press into the next latch");
   end;
   --  The estimate decays toward smaller frames but follows a large one.
   for I in 1 .. 64 loop
      Frame_Cost (S, 0, 1_000);
   end loop;
   Check (S.Cost_Us < Cost and then S.Cost_Us >= 1_000, "the cost decays toward recent frames");
   Frame_Cost (S, 0, 9_000);
   Check (S.Cost_Us = 9_000, "a large frame raises the cost at once");
   Frame_Cost (S, 10, 5);
   Check (S.Cost_Us < 9_000, "an out-of-order sample only decays");
   Ada.Text_IO.Put_Line ("PACING: PASS checks=" & Natural'Image (Checks));
end Pacing_Tests;
