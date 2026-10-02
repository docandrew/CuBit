with Ada.Text_IO;
with Compositor_Density; use Compositor_Density;
procedure Density_Tests is
   Checks : Natural := 0;
   type Alignment_List is array (Positive range <>) of Row_Alignment;
   Alignments : constant Alignment_List := (4, 8, 12, 28, 252, 256, 4_092, 4_096);
   type Extent_List is array (Positive range <>) of Extent;
   Edges : constant Extent_List := (1, 17, 1_080, 1_920, 4_095, 4_096, 65_534, 65_535);
   function Oracle_Pixels (L : Extent; S : Scale) return Long_Long_Integer is
      Product : constant Long_Long_Integer :=
        Long_Long_Integer (L) * Long_Long_Integer (S.Numerator);
      D : constant Long_Long_Integer := Long_Long_Integer (S.Denominator);
      Result : Long_Long_Integer := Product / D;
   begin
      if Product rem D /= 0 then Result := Result + 1; end if;
      return Result;
   end Oracle_Pixels;
   procedure Check (W, H : Extent; S : Scale; A : Row_Alignment) is
      PW : constant Long_Long_Integer := Oracle_Pixels (W, S);
      PH : constant Long_Long_Integer := Oracle_Pixels (H, S);
      Pitch : Long_Long_Integer := PW * 4;
      Bytes : Long_Long_Integer;
      procedure At_Budget (Budget : Natural) is
         R : constant Layout := Plan (W, H, S, Budget, A);
      begin
         Checks := Checks + 1;
         if PW > 65_535 or PH > 65_535 then
            pragma Assert (R.Status = Extent_Exceeded);
         elsif Bytes > Long_Long_Integer (Budget) then
            pragma Assert (R.Status = Budget_Exceeded);
         else
            pragma Assert (R.Status = Accepted);
            pragma Assert (Long_Long_Integer (R.Width) = PW);
            pragma Assert (Long_Long_Integer (R.Height) = PH);
            pragma Assert (Long_Long_Integer (R.Pitch) = Pitch);
            pragma Assert (Long_Long_Integer (R.Bytes) = Bytes);
         end if;
      end At_Budget;
   begin
      pragma Assert (Long_Long_Integer (Pixels (W, S)) = PW);
      pragma Assert (Long_Long_Integer (Pixels (H, S)) = PH);
      -- Independent alignment oracle: advance bytes to the next divisible row.
      while Pitch rem Long_Long_Integer (A) /= 0 loop
         Pitch := Pitch + 1;
      end loop;
      Bytes := Pitch * PH;
      At_Budget (0);
      At_Budget (16 * 1_024 * 1_024);
      At_Budget (Natural'Last);
      if Bytes <= Long_Long_Integer (Natural'Last) then
         At_Budget (Natural (Bytes));
         At_Budget (Natural (Bytes - 1));
      end if;
   end Check;
begin
   for N in Scale_Component loop
      for D in Scale_Component loop
         for W in 1 .. 128 loop
            Check (W, 129 - W, (N, D), 64);
         end loop;
         for W of Edges loop
            for H of Edges loop
               for A of Alignments loop Check (W, H, (N, D), A); end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   -- A 1080p logical surface at 200% must not silently stay at 1080p.
   pragma Assert (Plan (1_920, 1_080, (2, 1), 16 * 1_024 * 1_024).Status = Budget_Exceeded);
   declare
      R : constant Layout := Plan (1_920, 1_080, (2, 1), 33_177_600);
   begin
      pragma Assert (R.Status = Accepted and then R.Width = 3_840 and then
        R.Height = 2_160 and then R.Bytes = 33_177_600);
   end;
   Ada.Text_IO.Put_Line ("density: PASS" & Checks'Image & " admission checks");
end Density_Tests;
