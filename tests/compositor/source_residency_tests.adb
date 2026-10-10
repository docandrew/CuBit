with Ada.Text_IO;
with Compositor_Source_Content;
with Source_Residency_Model;
-- Exhaustive small-domain checks of the pure residency bookkeeping that
-- persistent client GPU sources rely on: unique keys, conservative row
-- bands, LRU victim choice and stale-band hand-off.
procedure Source_Residency_Tests is
   package C renames Compositor_Source_Content;
   package M renames Source_Residency_Model;
   package B renames M.Book;
   use type C.Row_Band, C.Source_Key, M.Model_Slot, B.Use_Stamp;
   S : B.State;
   Allowed : B.Candidates;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & What); raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;
begin
   -- Bands: union covers both operands and stays one normalized band.
   for F1 in 0 .. 12 loop for L1 in 0 .. 12 loop for F2 in 0 .. 12 loop for L2 in 0 .. 12 loop
      declare
         A : constant C.Row_Band := C.Band (F1, L1);
         Z : constant C.Row_Band := C.Band (F2, L2);
         U : constant C.Row_Band := C.Union (A, Z);
      begin
         Check (C.Normal (U) and C.Covers (U, A) and C.Covers (U, Z), "union cover");
         for R in 0 .. 12 loop
            Check (not (C.Contains (A, R) or C.Contains (Z, R)) or C.Contains (U, R), "union row");
         end loop;
         Check (C.Clip (U, 7).Last <= 7, "clip");
      end;
   end loop; end loop; end loop; end loop;
   Check (C.Whole (0) = C.Empty_Band and C.Whole (480) = (0, 480), "whole");
   -- Fill all slots, find each key, refuse duplicates by construction.
   for I in M.Model_Slot loop
      Check (B.Has_Free (S) and B.First_Free (S) = I, "first free");
      B.Bind (S, I, C.Source_Key (100 + Natural (I - M.Model_Slot'First)));
      B.Touch (S, I);
   end loop;
   Check (not B.Has_Free (S), "full");
   for I in M.Model_Slot loop
      Check (B.Holds (S, C.Source_Key (100 + Natural (I - M.Model_Slot'First))) and B.Find (S, C.Source_Key (100 + Natural (I - M.Model_Slot'First))) = I, "find");
   end loop;
   -- LRU: the oldest allowed slot wins; touching moves it to newest.
   Allowed := (others => True);
   Check (B.Victim (S, Allowed) = M.Model_Slot'First, "lru first");
   B.Touch (S, M.Model_Slot'First);
   Check (B.Victim (S, Allowed) = M.Model_Slot'First + 1, "lru touch");
   Allowed (M.Model_Slot'First + 1) := False;
   Check (B.Victim (S, Allowed) = M.Model_Slot'First + 2, "lru pinned skipped");
   Allowed := (others => False); Allowed (M.Model_Slot'Last) := True;
   Check (B.Victim (S, Allowed) = M.Model_Slot'Last, "single candidate");
   -- Stale bands accumulate across notes and hand off once.
   B.Note (S, 105, (10, 20));
   B.Note (S, 105, (40, 41));
   B.Note (S, 999, (0, 5));
   Check (S.Entries (B.Find (S, 105)).Stale = (10, 41), "stale union");
   B.Clear_Stale (S, B.Find (S, 105));
   Check (S.Entries (B.Find (S, 105)).Stale = C.Empty_Band, "stale cleared");
   -- Unbind frees the key; rebinding elsewhere keeps uniqueness.
   B.Unbind (S, B.Find (S, 105));
   Check (not B.Holds (S, 105) and B.Has_Free (S), "unbind");
   B.Bind (S, B.First_Free (S), 105);
   Check (B.Holds (S, 105) and B.Valid (S), "rebind");
   -- 10000 random-ish use rounds keep the invariant and the LRU property.
   for Round in 1 .. 10_000 loop
      declare
         I : constant M.Model_Slot := M.Model_Slot'First + M.Model_Slot'Base ((Round * 7919) mod 16);
      begin
         B.Touch (S, I);
         Allowed := (others => True);
         Allowed (I) := False;
         declare V : constant M.Model_Slot := B.Victim (S, Allowed); begin
            for J in M.Model_Slot loop
               if J /= I then Check (S.Entries (V).Last_Use <= S.Entries (J).Last_Use, "lru min"); end if;
            end loop;
         end;
         Check (B.Valid (S), "valid");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS source residency:" & Checks'Image &
     " checks (row-band union/clip over 13^4 bands, unique keys, LRU victim, stale hand-off)");
end Source_Residency_Tests;
