with Interfaces; use Interfaces;
with CuBit.Settings;
with CuBit.Settings.Readable;
with Startup;
with CuBit.Messages; use CuBit.Messages;
with CuBit.System_Facts;
with Civil_Time;
with Time_Zones;
with Time_Zone_Rules;
with Clock_Discipline;
with Clock_Floor;
package body Wall_Clock is
   --  Build-time floor: UTC never falls below the source commit time.
   Floor_MS : constant Clock_Discipline.UTC_Milliseconds :=
     Clock_Floor.UTC_Seconds * 1_000;
   Discipline : Clock_Discipline.State;
   --  A text setting into Data: OK and Length 0 when absent, not OK when
   --  Config refused it or it does not fit.
   generic
      with package Setting is new CuBit.Settings.Readable (<>);
      with function Image (Item : Setting.Value) return String;
   procedure Text (Data : out String; Length : out Natural; OK : out Boolean);
   procedure Text (Data : out String; Length : out Natural; OK : out Boolean) is
      use all type CuBit.Settings.Outcome;
      Stored : constant Setting.Reading := Setting.Read;
   begin
      Data := [others => ' '];
      Length := 0;
      OK := Stored.Kind = Absent;
      if Stored.Kind = Present and then Image (Stored.Item)'Length <= Data'Length then
         Length := Image (Stored.Item)'Length;
         Data (Data'First .. Data'First + Length - 1) := Image (Stored.Item);
         OK := True;
      end if;
   end Text;
   procedure Boot_Sample is new Text (Startup.Settings.Clock_Boot_Sample, CuBit.Settings.Image);
   procedure Time_Zone is new Text (Startup.Settings.Clock_Time_Zone, CuBit.Settings.Image);
   procedure Initialize is
      Data : String (1 .. 128);
      Length, Position : Natural;
      OK : Boolean;
      Base_UTC, Base_Mono : Unsigned_64 := 0;
      Seed_Valid : Boolean;
      procedure Number (Value : out Unsigned_64) is
         Start : Natural;
      begin
         Value := 0;
         while Position <= Length and then Data (Position) = ' ' loop
            Position := Position + 1;
         end loop;
         Start := Position;
         while Position <= Length and then Data (Position) in '0' .. '9' loop
            if Value > (Unsigned_64'Last - 9) / 10 then OK := False; return; end if;
            Value := Value * 10 + Character'Pos (Data (Position)) - Character'Pos ('0');
            Position := Position + 1;
         end loop;
         if Position = Start then OK := False; end if;
      end Number;
      function Image (Value : Unsigned_64) return String is
         Text : constant String := Unsigned_64'Image (Value);
      begin
         return Text (Text'First + 1 .. Text'Last);
      end Image;
      --  "2026-10-19 06:12:00 UTC" for a supported instant.
      function Date_Text (UTC : Unsigned_64) return String is
         Date : constant Civil_Time.Date_Time := Civil_Time.Split (Integer_64 (UTC));
         --  Adding 100 keeps a leading zero in the last two digits.
         Two_Digit_Pad : constant := 100;
         function Two (Value : Natural) return String is
            Text : constant String := Natural'Image (Value + Two_Digit_Pad);
         begin
            return Text (Text'Last - 1 .. Text'Last);
         end Two;
         Year : constant String := Natural'Image (Date.Year);
      begin
         return Year (Year'First + 1 .. Year'Last) & "-" & Two (Date.Month) & "-" & Two (Date.Day) & " " &
           Two (Date.Hour) & ":" & Two (Date.Minute) & ":" & Two (Date.Second) & " UTC";
      end Date_Text;
      Floor_Seconds : constant Unsigned_64 := Clock_Floor.UTC_Seconds;
      Unavailable : constant String := "unavailable";
      Supported, Read_OK : Boolean;
   begin
      Boot_Sample (Data, Length, OK);
      Read_OK := OK;
      Position := 1;
      if OK then Number (Base_UTC); Number (Base_Mono); end if;
      Seed_Valid := OK and then Position = Length + 1;
      Supported := Seed_Valid and then Base_UTC <= Unsigned_64 (Civil_Time.Timestamp'Last) and then
        Time_Zone_Rules.Supported (Integer_64 (Base_UTC));
      Clock_Discipline.Initialize
        (Discipline, Supported, Base_UTC, Base_Mono, Floor_MS);
      --  Every outcome names the check, so one boot log says why the
      --  Deskbar shows no time (devmgr logs the RTC side: "devmgr: RTC ...").
      if Clock_Discipline.Available (Discipline) then
         debugPrint ("clock: RTC-derived UTC ready: " & Date_Text (Base_UTC) & ASCII.LF);
      elsif Supported then
         debugPrint ("clock: wall time unavailable: RTC " & Date_Text (Base_UTC) &
                     " is below the build time floor " & Date_Text (Floor_Seconds) &
                     " (RTC battery lost, or RTC holds local time west of UTC?)" & ASCII.LF);
      elsif Seed_Valid then
         debugPrint ("clock: wall time unavailable: RTC UTC seconds " & Image (Base_UTC) &
                     " outside the supported 2000..2099" & ASCII.LF);
      elsif not Read_OK then
         debugPrint ("clock: wall time unavailable: reading clock.boot-sample from Config failed" & ASCII.LF);
      elsif Length = Unavailable'Length and then Data (1 .. Length) = Unavailable then
         debugPrint ("clock: wall time unavailable: devmgr had no RTC seed " &
                     "(see ""devmgr: RTC seed unavailable"")" & ASCII.LF);
      elsif Length = 0 then
         debugPrint ("clock: wall time unavailable: no clock.boot-sample in Config" & ASCII.LF);
      else
         debugPrint ("clock: wall time unavailable: malformed clock.boot-sample """ &
                     Data (1 .. Length) & """" & ASCII.LF);
      end if;
   end Initialize;

   procedure Adjust
     (Candidate : CuBit.Clock_Control.Sample;
      Result : out CuBit.Clock_Control.Outcome;
      Quality : out CuBit.Clocks.Time_Quality) is
   begin
      Clock_Discipline.Apply
        (Discipline, Candidate, syscall (SYSCALL_GETTIME), Floor_MS, Result);
      Quality := Clock_Discipline.Quality (Discipline);
   end Adjust;
   Last_Published : Unsigned_64 := 0;

   procedure Publish is
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      UTC_MS : Clock_Discipline.UTC_Milliseconds;
      Current : Boolean;
      Offset : Unsigned_64;
   begin
      Clock_Discipline.Current (Discipline, Now, UTC_MS, Current);
      if Current and then Unsigned_64 (UTC_MS) > Now then
         Offset := Unsigned_64 (UTC_MS) - Now;
         if Offset /= Last_Published then
            if CuBit.System_Facts.Publish
                 (CuBit.System_Facts.SP.Wall_Clock_Offset, Offset)
            then
               Last_Published := Offset;
            end if;
         end if;
      end if;
   end Publish;

   function Snapshot return Message is
      Result : Message := NULL_MESSAGE;
      Date : Civil_Time.Date_Time;
      Quality : CuBit.Clocks.Time_Quality := CuBit.Clocks.Unknown_Time;
      Offset : Integer_32 := 0;
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      UTC : Unsigned_64 := 0;
      UTC_MS : Clock_Discipline.UTC_Milliseconds;
      Current : Boolean;
      Name : String (1 .. 128);
      Length : Natural;
      Zone : Time_Zones.Time_Zone;
      OK : Boolean;
   begin
      Clock_Discipline.Current (Discipline, Now, UTC_MS, Current);
      if Clock_Discipline.Available (Discipline) and then not Current then
         Quality := CuBit.Clocks.Unsupported_Date;
      elsif Current then
         UTC := UTC_MS / 1_000;
         Quality := CuBit.Clocks.Unsupported_Date;
         if UTC < 4_102_444_800 then
            Time_Zone (Name, Length, OK);
            if OK then
               Time_Zones.Find
                 ((if Length = 0 then "UTC" else Name (1 .. Length)), Zone, OK);
            end if;
            Quality := CuBit.Clocks.Invalid_Zone;
            if OK then
               Offset := Time_Zone_Rules.Offset (Zone, Integer_64 (UTC));
               Date := Civil_Time.Split (Integer_64 (UTC) + Integer_64 (Offset));
               Quality := Clock_Discipline.Quality (Discipline);
            end if;
         end if;
      end if;
      Result.tag := (label => 16#F000#, length => 4, protocol => 0, ipc => <>, reserved => 0);
      Result.words :=
        [UTC, Shift_Left (Unsigned_64 (Date.Year), 40) or
         Shift_Left (Unsigned_64 (Date.Month), 32) or
         Shift_Left (Unsigned_64 (Date.Day), 24) or
         Shift_Left (Unsigned_64 (Date.Hour), 16) or
         Shift_Left (Unsigned_64 (Date.Minute), 8) or Unsigned_64 (Date.Second),
         Unsigned_64 (Offset + 86_400),
         Unsigned_64 (CuBit.Clocks.Time_Quality'Enum_Rep (Quality))];
      return Result;
   end Snapshot;
end Wall_Clock;
