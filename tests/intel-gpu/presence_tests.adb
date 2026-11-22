with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Presence; use Intel_GPU_Display_Presence;
procedure Presence_Tests is
   Masks : constant array (Pipe) of Unsigned_32 :=
     [16#40000000#, 16#00200000#, 16#10000000#, 16#00400000#];
   Word : Unsigned_32;
   S : Snapshot;
begin
   for Device in Unsigned_16 range 16#46D0# .. 16#46D4# loop
      for Combination in Unsigned_32 range 0 .. 15 loop
         Word := 0;
         for P in Pipe loop
            if (Combination and Shift_Left (1, Pipe'Pos (P))) /= 0 then
               Word := Word or Masks (P);
            end if;
         end loop;
         S := Decode (16#8086#, Device, 3, Word, Word);
         pragma Assert (S.Known);
         for P in Pipe loop
            pragma Assert (S.Pipes (P) =
              (if (Word and Masks (P)) = 0 then Present else Absent));
         end loop;
         for Power in Unsigned_32 range 0 .. 15 loop
            declare
               Held : Held_Set;
               Expected : Boolean := True;
            begin
               for P in Pipe loop
                  Held (P) := (Power and Shift_Left (1, Pipe'Pos (P))) /= 0;
                  if S.Pipes (P) = Present and then not Held (P) then
                     Expected := False;
                  end if;
               end loop;
               pragma Assert (Required_Power_Held (S, Held) = Expected);
               declare
                  Unknown_Snapshot : Snapshot := S;
               begin
                  Unknown_Snapshot.Known := False;
                  pragma Assert (not Required_Power_Held (Unknown_Snapshot, Held));
                  for P in Pipe loop
                     Unknown_Snapshot := S;
                     Unknown_Snapshot.Pipes (P) := Unknown;
                     pragma Assert (not Required_Power_Held (Unknown_Snapshot, Held));
                  end loop;
               end;
            end;
         end loop;
         S := Decode (16#8086#, Device, 3, Word, Word xor 1);
         pragma Assert (not S.Known and S.Pipes = Pipe_Set'[others => Unknown]);
      end loop;
   end loop;
   for Device in Unsigned_16 loop
      S := Decode (16#8086#, Device, 3, 0, 0);
      pragma Assert (S.Known = (Device in 16#46D0# .. 16#46D4#));
   end loop;
   pragma Assert (not Decode (0, 16#46D2#, 3, 0, 0).Known);
   pragma Assert (not Decode (16#8086#, 16#46D2#, 2, 0, 0).Known);
   pragma Assert (not Decode (16#8086#, 16#46D2#, 3,
     Unsigned_32'Last, Unsigned_32'Last).Known);
   Ada.Text_IO.Put_Line ("presence PASS: exact identities, all16 fuse combinations, unstable/sentinel rejection");
end Presence_Tests;
