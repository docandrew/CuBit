with Ada.Text_IO; use Ada.Text_IO;
with Glyph_Cache_Model;
with Compositor_Glyph_Cache;
procedure Glyph_Cache_Tests is
   package C renames Glyph_Cache_Model;
   use type C.Token, C.Lease, C.Phase, C.State, C.Serial_Number;
   package Small is new Compositor_Glyph_Cache (7);
   use type Small.Token, Small.Lease;
   S : C.State := C.Open (2_000);
   K : C.Key := (0, 65, (1, 1));
   T, Neighbor, Rejected, Old : C.Token;
   R, Late, New_Read : C.Lease;
   Held : array (C.Reader_Slot) of C.Lease;
   Accepted : Boolean;
begin
   C.Reserve (S, (1, 66, (1, 1)), Neighbor);
   C.Publish (S, Neighbor, True);
   for Cycle in 1 .. 4_096 loop
      C.Reserve (S, K, T);
      pragma Assert (T /= C.No_Token and C.Status (S, T) = C.Building and C.Charged (S) = 1_088);
      if Cycle > 1 then
         declare Before : constant C.State := S;
         begin
            pragma Assert (T.Position = Old.Position and T.Identity /= Old.Identity);
            C.Publish (S, Old, True); C.Begin_Retirement (S, Old, Accepted); C.Retired (S, Old, True);
            pragma Assert (not Accepted and S = Before);
         end;
      end if;
      C.Acquire (S, T, R); pragma Assert (R = C.No_Lease);
      C.Publish (S, T, False); pragma Assert (C.Status (S, T) = C.Building);
      C.Reserve (S, (0, 65, (2, 2)), Rejected);
      pragma Assert (Rejected = C.No_Token and C.Find (S, (0, 65, (16, 16))) = T.Position);
      C.Publish (S, T, True);
      C.Acquire (S, T, R); pragma Assert (R /= C.No_Lease and C.Active (S, R));
      C.Complete (S, R, False);
      C.Begin_Retirement (S, T, Accepted);
      pragma Assert (not Accepted and C.Active (S, R));
      Late := R;
      C.Complete (S, R, True);
      C.Acquire (S, T, New_Read);
      pragma Assert (New_Read /= R and New_Read.Position = R.Position);
      if Cycle = 1 then
         declare
            Before : constant C.State := S;
            Wrong_Read : C.Lease := New_Read;
            Wrong_Mask : C.Token := T;
         begin
            Wrong_Read.Identity := Wrong_Read.Identity + 2 ** 32;
            Wrong_Mask.Identity := Wrong_Mask.Identity + 2 ** 32;
            C.Complete (S, Wrong_Read, True);
            C.Begin_Retirement (S, Wrong_Mask, Accepted);
            C.Retired (S, Wrong_Mask, True);
            pragma Assert (not Accepted and S = Before and C.Active (S, New_Read));
         end;
      end if;
      C.Complete (S, Late, True);
      pragma Assert (C.Active (S, New_Read) and C.Reader_Count (S) = 1);
      C.Begin_Retirement (S, T, Accepted); pragma Assert (not Accepted);
      C.Complete (S, New_Read, True);
      C.Begin_Retirement (S, T, Accepted); pragma Assert (Accepted);
      C.Acquire (S, T, R); pragma Assert (R = C.No_Lease);
      C.Retired (S, T, False);
      pragma Assert (C.Current (S, T) and C.Charged (S) = 1_088);
      C.Reserve (S, K, Rejected); pragma Assert (Rejected = C.No_Token);
      C.Retired (S, T, True);
      pragma Assert (not C.Current (S, T) and C.Charged (S) = 544 and C.Current (S, Neighbor));
      if Cycle > 1 then
         declare Before : constant C.State := S;
         begin
            C.Publish (S, Old, True); C.Begin_Retirement (S, Old, Accepted); C.Retired (S, Old, True);
            pragma Assert (not Accepted and S = Before);
         end;
      end if;
      Old := T;
   end loop;
   -- Outstanding reads saturate at 32, without evicting a pinned mask.
   S := C.Open (544); C.Reserve (S, K, T); C.Publish (S, T, True);
   for I in Held'Range loop C.Acquire (S, T, Held (I)); pragma Assert (Held (I) /= C.No_Lease); end loop;
   C.Acquire (S, T, R);
   pragma Assert (R = C.No_Lease and C.Reader_Count (S) = 32 and C.Victim (S) = 0);
   for I in Held'Range loop
      C.Complete (S, Held (I), True);
      if I < Held'Last then
         C.Begin_Retirement (S, T, Accepted); pragma Assert (not Accepted);
      end if;
   end loop;
   pragma Assert (C.Victim (S) = T.Position);
   C.Begin_Retirement (S, T, Accepted); pragma Assert (Accepted);
   C.Retired (S, T, True); pragma Assert (C.Charged (S) = 0);
   -- Fill every metadata slot; distinct rational densities remain separate.
   S := C.Open (C.Byte_Count'Last);
   for I in C.Slot loop
      C.Reserve (S, ((I - 1) / 95, 32 + (I - 1) mod 95, (1, 1)), T);
      pragma Assert (T.Position = I); C.Publish (S, T, True);
   end loop;
   C.Reserve (S, (0, 65, (5, 4)), Rejected);
   pragma Assert (Rejected = C.No_Token and C.Charged (S) = 128 * 544);
   for I in C.Slot loop
      pragma Assert (C.Victim (S) = I);
      C.Begin_Retirement (S, C.At_Slot (S, I), Accepted); pragma Assert (Accepted);
      C.Retired (S, C.At_Slot (S, I), True);
   end loop;
   pragma Assert (C.Charged (S) = 0 and C.Victim (S) = 0);
   -- All supported ratios admit exactly their proved byte charge, including
   -- extreme density; a one-byte shortage rejects without changing state.
   for N in 1 .. 16 loop
      for D in 1 .. 16 loop
         K.Scale := (C.L.G.Scale_Component (N), C.L.G.Scale_Component (D));
         declare Size : constant Positive := C.L.Plan (K.Scale).Bytes;
         begin
            S := C.Open (Size - 1); C.Reserve (S, K, T);
            pragma Assert (T = C.No_Token and C.Charged (S) = 0);
            S := C.Open (Size); C.Reserve (S, K, T);
            pragma Assert (T /= C.No_Token and C.Bytes (S, T) = Size and C.Charged (S) = Size);
            C.Begin_Retirement (S, T, Accepted); pragma Assert (Accepted);
            C.Retired (S, T, True); pragma Assert (C.Charged (S) = 0);
         end;
      end loop;
   end loop;
   -- Force both non-wrapping identity counters to their last values.
   declare
      X : Small.State := Small.Open (544);
      XT : Small.Token;
      XR, Saved : Small.Lease;
      Good : Boolean;
   begin
      Small.Reserve (X, (0, 65, (1, 1)), XT); Small.Publish (X, XT, True);
      for I in 1 .. 7 loop
         Small.Acquire (X, XT, XR); pragma Assert (XR /= Small.No_Lease);
         Saved := XR; Small.Complete (X, XR, True);
      end loop;
      Small.Acquire (X, XT, XR); pragma Assert (XR = Small.No_Lease);
      Small.Complete (X, Saved, True); pragma Assert (Small.Reader_Count (X) = 0);
      Small.Begin_Retirement (X, XT, Good); Small.Retired (X, XT, True);
      for I in 2 .. 7 loop
         Small.Reserve (X, (0, 65, (1, 1)), XT); pragma Assert (XT /= Small.No_Token);
         Small.Begin_Retirement (X, XT, Good); Small.Retired (X, XT, True);
      end loop;
      Small.Reserve (X, (0, 65, (1, 1)), XT);
      pragma Assert (XT = Small.No_Token and Small.Charged (X) = 0);
   end;
   Put_Line ("GLYPH-CACHE: PASS 4096 reuse cycles, 32 retained readers, 128 slots, 256 density budgets, stale completions and identity exhaustion");
end Glyph_Cache_Tests;
