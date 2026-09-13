with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Civil_Time; use Civil_Time;
with Time_Zones; use Time_Zones;
procedure Main is
   Date : Date_Time;
   Zone : Time_Zone;
   Found : Boolean;
   Expected : Integer_64 := 0;
   procedure Check (Name : String; UTC : Date_Time; Offset_Seconds : Integer_32) is
   begin
      Find (Name, Zone, Found);
      pragma Assert (Found);
      pragma Assert (Offset (Zone, Seconds (UTC)) = Offset_Seconds);
   end Check;
begin
   for Y in Year_Number loop
      for M in Month_Number loop
         for D in 1 .. Days (Y, M) loop
            Date := (Y, M, D, 0, 0, 0);
            pragma Assert (Seconds (Date) = Expected);
            pragma Assert (Split (Expected) = Date);
            Date := (Y, M, D, 23, 59, 59);
            pragma Assert (Split (Expected + 86_399) = Date);
            pragma Assert (Seconds (Date) = Expected + 86_399);
            Expected := Expected + 86_400;
         end loop;
      end loop;
   end loop;
   pragma Assert (Expected - 1 = Timestamp'Last);
   pragma Assert (not Valid ((2100, 2, 29, 0, 0, 0)));
   pragma Assert (Valid ((2000, 2, 29, 0, 0, 0)));
   for Candidate in Time_Zone loop
      Find (Name (Candidate), Zone, Found);
      pragma Assert (Found and then Zone = Candidate);
   end loop;
   Find ("Made/Up", Zone, Found);
   pragma Assert (not Found);
   pragma Assert (not Supported (946_684_799));
   pragma Assert (Supported (946_684_800));
   pragma Assert (Supported (4_102_444_799));
   pragma Assert (not Supported (4_102_444_800));
   Check ("UTC", (2026, 1, 1, 0, 0, 0), 0);
   Check ("America/Denver", (2026, 1, 1, 0, 0, 0), -25_200);
   Check ("America/Denver", (2026, 7, 1, 0, 0, 0), -21_600);
   Check ("America/Denver", (2026, 3, 8, 8, 59, 59), -25_200);
   Check ("America/Denver", (2026, 3, 8, 9, 0, 0), -21_600);
   Check ("America/Denver", (2026, 11, 1, 7, 59, 59), -21_600);
   Check ("America/Denver", (2026, 11, 1, 8, 0, 0), -25_200);
   Check ("Asia/Kathmandu", (2026, 1, 1, 0, 0, 0), 20_700);
   Check ("Australia/Lord_Howe", (2026, 1, 1, 0, 0, 0), 39_600);
   Check ("Australia/Lord_Howe", (2026, 7, 1, 0, 0, 0), 37_800);
   Put_Line ("CLOCK PASS: every date 1970..2399, timezone names, DST boundaries and fractional offsets");
end Main;
