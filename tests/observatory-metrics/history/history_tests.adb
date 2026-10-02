with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Observatory_History; use Observatory_History;
procedure History_Tests is
   H : History;
   ID : Identity := (1, 2, 3, 4, 5);
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
begin
   Check (Length (H) = 0);
   Observe (H, ID, 0, 0, False);
   Check (not Sample_At (H, 0).Has_Delta and not Sample_At (H, 0).Has_Latency);
   for I in 1 .. 1_000 loop
      Observe (H, ID, Unsigned_64 (I * 3), Unsigned_64 (I), I mod 7 = 0);
      Check (Length (H) = Observatory_History.Count'Min (I + 1, Capacity));
      declare S : constant Sample := Sample_At (H, Length (H) - 1); begin
         Check (S.Upper = Unsigned_64 (I) and S.Added = 3 and S.Has_Delta and S.Has_Latency);
         Check (S.Lossy = (I mod 7 = 0));
      end;
      if I >= Capacity then
         Check (Sample_At (H, 0).Upper = Unsigned_64 (I - Capacity + 1));
      end if;
   end loop;
   Break_Continuity (H); Observe (H, ID, 4000, 9, False);
   Check (not Sample_At (H, Length (H) - 1).Has_Delta);
   Observe (H, ID, 4000, 9, False);
   Check (Sample_At (H, Length (H) - 1).Has_Delta and Sample_At (H, Length (H) - 1).Added = 0);
   Observe (H, ID, 1, 2, False);
   Check (Length (H) = 1 and not Sample_At (H, 0).Has_Delta);
   ID.Publisher := 99; Observe (H, ID, Unsigned_64'Last, Unsigned_64'Last, True);
   Check (Length (H) = 1 and Sample_At (H, 0).Upper = Unsigned_64'Last and not Sample_At (H, 0).Has_Delta);
   for Height in Positive_Height loop
      Check (Scale (0, 0, Height) = 0);
      Check (Scale (Unsigned_64'Last, Unsigned_64'Last, Height) = Height);
      Check (Scale (Unsigned_64'Last - 1, Unsigned_64'Last, Height) = Height - 1);
      Check (Scale (1, 1, Height) = Height);
      Check (Scale (1, 2, Height) = Height / 2);
      for Value in 0 .. 500 loop
         Check (Scale (Unsigned_64 (Value), 500, Height) = Value * Height / 500);
         if Value > 0 then
            Check (Scale (Unsigned_64 (Value), 500, Height) >= Scale (Unsigned_64 (Value-1), 500, Height));
         end if;
      end loop;
   end loop;
   Put_Line ("PASS Observatory history:" & Checks'Image & " checks");
end History_Tests;
