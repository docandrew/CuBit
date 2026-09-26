with Interfaces; use Interfaces;
with System;
with CuBit.Config;
with CuBit.Messages; use CuBit.Messages;
with Civil_Time;
with Time_Zones;
with Clock_Discipline;
with Clock_Floor;
package body Wall_Clock is
   --  Build-time floor: UTC never falls below the source commit time.
   Floor_MS : constant Clock_Discipline.UTC_Milliseconds :=
     Clock_Floor.UTC_Seconds * 1_000;
   Discipline : Clock_Discipline.State;
   procedure Text (Key : String; Data : out String; Length : out Natural;
                   OK : out Boolean) is
      Address : System.Address;
      Status : CuBit.Config.ConfigStatus;
      use type CuBit.Config.ConfigStatus;
   begin
      Data := [others => ' '];
      CuBit.Config.get (Key, Address, Length, Status);
      OK := Status = CuBit.Config.OK and then Length <= Data'Length;
      if OK and then Length > 0 then
         declare
            Source : String (1 .. Length) with Import, Address => Address;
         begin Data (Data'First .. Data'First + Length - 1) := Source; end;
      else Length := 0; end if;
      if Status = CuBit.Config.NotFound then OK := True; end if;
   end Text;
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
   begin
      Text ("clock.boot-sample", Data, Length, OK);
      Position := 1;
      if OK then Number (Base_UTC); Number (Base_Mono); end if;
      Seed_Valid := OK and then Position = Length + 1 and then
        Base_UTC in 946_684_800 .. 4_102_444_799;
      Clock_Discipline.Initialize
        (Discipline, Seed_Valid, Base_UTC, Base_Mono, Floor_MS);
      if Clock_Discipline.Available (Discipline) then
         debugPrint ("clock: RTC-derived UTC ready" & ASCII.LF);
      elsif Seed_Valid then
         debugPrint ("clock: RTC below build time floor, wall time unavailable" & ASCII.LF);
      else
         debugPrint ("clock: wall time unavailable" & ASCII.LF);
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
            Text ("clock.time-zone", Name, Length, OK);
            if OK then
               Time_Zones.Find
                 ((if Length = 0 then "UTC" else Name (1 .. Length)), Zone, OK);
            end if;
            Quality := CuBit.Clocks.Invalid_Zone;
            if OK then
               Offset := Time_Zones.Offset (Zone, Integer_64 (UTC));
               Date := Civil_Time.Split (Integer_64 (UTC) + Integer_64 (Offset));
               Quality := Clock_Discipline.Quality (Discipline);
            end if;
         end if;
      end if;
      Result.tag := (16#F000#, 4, 0, 0);
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
