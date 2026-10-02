with Ada.Text_IO; use Ada.Text_IO;
with Glyph_Arena_Model;
with Compositor_Glyph_Arena;
with Compositor_Glyph_Layout;
procedure Glyph_Arena_Tests is
   package A renames Glyph_Arena_Model;
   package L renames Compositor_Glyph_Layout;
   package Small is new Compositor_Glyph_Arena (7);
   use type A.Token, Small.Token;
   S : A.State;
   T, Old, Neighbor : A.Token;
   Held : array (1 .. 4_096) of A.Token;
   Expected : array (0 .. A.Backing_Bytes / A.Cell_Bytes - 1) of Boolean := (others => False);
   Released : Boolean;
   procedure Check is
   begin
      for I in Expected'Range loop
         pragma Assert (Expected (I) = A.Occupied (S, I + 1));
      end loop;
   end Check;
   procedure Claim (Size : A.Request_Bytes; Result : out A.Token) is
      First : Natural;
   begin
      A.Reserve (S, Size, Result);
      if Result = A.No_Token then return; end if;
      pragma Assert (A.Current (S, Result) and A.Capacity (Result) = ((Size + 127) / 128) * 128);
      First := A.Offset (Result) / A.Cell_Bytes;
      for I in First .. First + A.Capacity (Result) / A.Cell_Bytes - 1 loop
         pragma Assert (not Expected (I)); Expected (I) := True;
      end loop;
      Check;
   end Claim;
   procedure Drop (Value : A.Token) is
      First : constant Natural := A.Offset (Value) / A.Cell_Bytes;
   begin
      A.Release (S, Value, True, Released); pragma Assert (Released);
      for I in First .. First + A.Capacity (Value) / A.Cell_Bytes - 1 loop
         pragma Assert (Expected (I)); Expected (I) := False;
      end loop;
      Check;
   end Drop;
begin
   A.Initialize (S);
   Claim (L.Maximum_Bytes, Neighbor);
   for Cycle in 1 .. 4_096 loop
      Claim (A.Request_Bytes (1 + Cycle mod 2_176), T);
      pragma Assert (T /= A.No_Token);
      if Cycle > 1 then
         pragma Assert (not A.Current (S, Old));
         A.Release (S, Old, True, Released); pragma Assert (not Released and A.Current (S, T));
      end if;
      A.Release (S, T, False, Released); pragma Assert (not Released and A.Current (S, T));
      Check; Old := T; Drop (T);
      pragma Assert (A.Current (S, Neighbor));
   end loop;
   Drop (Neighbor);
   -- Full cell occupancy and fragmented holes: no compaction or live copies.
   for I in Held'Range loop Claim (1, Held (I)); pragma Assert (Held (I) /= A.No_Token); end loop;
   Claim (1, T); pragma Assert (T = A.No_Token);
   for I in Held'Range loop if I mod 2 = 1 then Drop (Held (I)); end if; end loop;
   Claim (129, T); pragma Assert (T = A.No_Token);
   for I in Held'Range loop if I mod 2 = 0 then Drop (Held (I)); end if; end loop;
   -- Every density, plus exact and immediately below each cell boundary.
   for N in 1 .. 16 loop
      for D in 1 .. 16 loop
         Claim (L.Plan ((L.G.Scale_Component (N), L.G.Scale_Component (D))).Bytes, T);
         pragma Assert (T /= A.No_Token); Drop (T);
      end loop;
   end loop;
   for Size in 1 .. L.Maximum_Bytes / A.Cell_Bytes loop
      Claim (Size * A.Cell_Bytes, T); Drop (T);
      Claim (Size * A.Cell_Bytes - 1, T); Drop (T);
   end loop;
   for I in 1 .. 3 loop Claim (L.Maximum_Bytes, Held (I)); pragma Assert (Held (I) /= A.No_Token); end loop;
   Claim (L.Maximum_Bytes, T); pragma Assert (T = A.No_Token);
   for I in 1 .. 3 loop Drop (Held (I)); end loop;
   declare X : Small.State; XT : Small.Token;
   begin
      Small.Initialize (X);
      for I in 1 .. 7 loop
         Small.Reserve (X, 544, XT); pragma Assert (XT /= Small.No_Token);
         Small.Release (X, XT, True, Released); pragma Assert (Released);
      end loop;
      Small.Reserve (X, 544, XT); pragma Assert (XT = Small.No_Token);
   end;
   Put_Line ("GLYPH-ARENA: PASS 4096 reuse cycles, full/fragmented occupancy, 256 densities, 2176 rounding boundaries, stale retirement and exhaustion");
end Glyph_Arena_Tests;
