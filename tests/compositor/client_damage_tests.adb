with Ada.Text_IO;
with Client_Frame_Damage;
procedure Client_Damage_Tests is
   use Client_Frame_Damage;
   type Image is array (Natural range 0 .. 12, Natural range 0 .. 16) of Natural;
   Scene, Visible : Image := (others => (others => 0));
   Buffers : array (Slot) of Image := (others => (others => (others => 0)));
   S : State := Open ((0, 0, 17, 13));
   B : Slot := 1;
   Area, Repair : Box;
   Published_Count, Repaired_Pixels : Natural := 0;
   function Inside (R : Box; X, Y : Natural) return Boolean is
     (X >= R.Left and X < R.Right and Y >= R.Top and Y < R.Bottom);
begin
   for Round in 1 .. 4096 loop
      -- Multiple newest-state updates before painting; readers may retain the
      -- other slot arbitrarily long, and no pixel is copied from that slot.
      for Input in 1 .. 1 + Round mod 4 loop
         declare
            X : constant Natural := (Round * 7 + Input * 3) mod 17;
            Y : constant Natural := (Round * 5 + Input) mod 13;
         begin
            Area := (X, Y, Natural'Min (17, X + 1 + Round mod 3),
                     Natural'Min (13, Y + 1 + Input mod 2));
            for Row in Area.Top .. Area.Bottom - 1 loop
               for Col in Area.Left .. Area.Right - 1 loop
                  Scene (Row, Col) := Round;
               end loop;
            end loop;
            Invalidate (S, Area);
         end;
      end loop;
      for J in Slot loop
         for Y in 0 .. 12 loop
            for X in 0 .. 16 loop
               pragma Assert (Buffers (J) (Y, X) = Scene (Y, X) or else
                              Inside (Required (S, J), X, Y));
               pragma Assert (Visible (Y, X) = Scene (Y, X) or else
                              Inside (Publication_Damage (S), X, Y));
            end loop;
         end loop;
      end loop;
      Repair := Required (S, B);
      declare
         Before : constant State := S;
      begin
         for Y in Repair.Top .. Repair.Bottom - 1 loop
            for X in Repair.Left .. Repair.Right - 1 loop
               -- Inject a partially completed paint. The untouched debt must
               -- survive this failure and later state changes.
               if Round mod 7 /= 0 or Y = Repair.Top then
                  Buffers (B) (Y, X) := Scene (Y, X);
                  Repaired_Pixels := Repaired_Pixels + 1;
               end if;
            end loop;
         end loop;
         if Round mod 7 /= 0 and Round mod 11 /= 0 then
            pragma Assert (Buffers (B) = Scene);
            Visible := Buffers (B);
            Published (S, B, Repair);
            Published_Count := Published_Count + 1;
            B := (if B = 1 then 2 else 1);
         else
            -- Paint or publication failed; never acknowledge repair.
            pragma Assert (S = Before);
         end if;
      end;
   end loop;
   -- A configuration replacement starts both buffers wholly dirty, even when
   -- the previous geometry was smaller. Test exact coordinate upper bounds.
   S := Open ((0, 0, Coordinate'Last, Coordinate'Last));
   Published (S, 1, Bounds (S)); Published (S, 2, Bounds (S));
   Area := (65_534, 65_534, 65_535, 65_535);
   Invalidate (S, Area);
   pragma Assert (Required (S, 1) = Area and Required (S, 2) = Area);
   Published (S, 1, Area);
   pragma Assert (Publication_Damage (S) = Empty and Required (S, 2) = Area);
   Area := (0, 0, 1, 1); Invalidate (S, Area);
   pragma Assert (Required (S, 1) = Area and Required (S, 2) = Bounds (S));
   Ada.Text_IO.Put_Line ("PASS client repaint: 4096 pixel-model cycles, publications" &
     Natural'Image (Published_Count) & ", painted pixels" & Natural'Image (Repaired_Pixels));
end Client_Damage_Tests;
