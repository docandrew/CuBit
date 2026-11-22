with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_PPGTT_Scratch;
procedure PPGTT_Scratch_Tests is
   package S renames Intel_GPU_PPGTT_Scratch;
   package P renames Intel_GPU_ADLN_PPGTT;
   Pages : constant S.Backing_Pages :=
     [16#1000#, 16#7000#, 16#3000#, 16#FFFF_F000#];
   Bad : S.Backing_Pages;
   Word : Unsigned_64;
   type Values is array (Positive range <>) of Unsigned_64;
begin
   pragma Assert (S.Valid (Pages));
   -- Every entry at a given fallback level is identical. Exercise all512
   -- positions at each of four levels, including the highest root slot.
   for Position in Unsigned_64 range 0 .. 511 loop
      for Depth in 0 .. 3 loop
         declare
            GPU : constant Unsigned_64 := Shift_Left (Position, 12 + 9 * Depth);
            Route : constant P.Walk := P.Locate (GPU);
         begin
            pragma Assert (Route.Valid);
            Word := S.Fallback (Pages, 3);
            for L in reverse S.Table_Level loop
               pragma Assert (Word / 4096 = Pages (L) / 4096);
               Word := S.Fill (Pages, L);
            end loop;
            pragma Assert (Word = Pages (0) + 27);
         end;
      end loop;
   end loop;
   for L in S.Level loop
      pragma Assert (S.Contains (Pages, Pages (L)));
      for R in S.Level loop
         if L /= R then
            Bad := Pages;
            Bad (L) := Pages (R);
            pragma Assert (not S.Valid (Bad));
            for K in S.Level loop
               pragma Assert (S.Fallback (Bad, K) = 0);
            end loop;
         end if;
      end loop;
      for Invalid of Values'(0, 1, 4095, 2 ** 32) loop
         Bad := Pages;
         Bad (L) := Invalid;
         pragma Assert (not S.Valid (Bad));
         for K in S.Level loop
            pragma Assert (S.Fallback (Bad, K) = 0);
         end loop;
      end loop;
   end loop;
   pragma Assert (not S.Contains (Pages, 16#2000#));
   Ada.Text_IO.Put_Line ("PPGTT scratch PASS: hierarchy, aliases, bounds (offline only)");
end PPGTT_Scratch_Tests;
