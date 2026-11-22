pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Clocks; use CuBit.Clocks;
with CuBit.Clock_Control; use CuBit.Clock_Control;
with Clock_Discipline;
with Clock_Floor;
with SNTP; use SNTP;
with Server_List;

--  Linux-hosted checks for the SNTP, clock-discipline, clock-control
--  encoding and server-list cores. Assertions are enabled (-gnata).
procedure Main is
   NTP_2026 : constant Unsigned_64 := 1_767_225_600 + Unix_Offset; -- 2026-01-01
   Floor_MS : constant Clock_Discipline.UTC_Milliseconds :=
     Clock_Floor.UTC_Seconds * 1_000;

   function Stamp (Seconds : Unsigned_64; Millis : Unsigned_64 := 0) return Unsigned_64 is
     (Shift_Left (Seconds, 32) or (Millis * 2 ** 32 + 999) / 1_000);

   procedure Put64 (P : in out Packet; First : Positive; Value : Unsigned_64) is
   begin
      for I in 0 .. 7 loop
         P (First + I) := Unsigned_8 (Shift_Right (Value, 56 - 8 * I) and 255);
      end loop;
   end Put64;

   procedure Put32 (P : in out Packet; First : Positive; Value : Unsigned_32) is
   begin
      for I in 0 .. 3 loop
         P (First + I) := Unsigned_8 (Shift_Right (Value, 24 - 8 * I) and 255);
      end loop;
   end Put32;

   function Reply
     (Origin, Receive, Transmit : Unsigned_64;
      First_Byte : Unsigned_8 := 16#24#; -- LI 0, VN 4, mode 4
      Stratum : Unsigned_8 := 2;
      Root_Delay, Root_Dispersion : Unsigned_32 := 0) return Packet
   is
      P : Packet := [others => 0];
   begin
      P (1) := First_Byte;
      P (2) := Stratum;
      Put32 (P, 5, Root_Delay);
      Put32 (P, 9, Root_Dispersion);
      Put64 (P, 25, Origin);
      Put64 (P, 33, Receive);
      Put64 (P, 41, Transmit);
      return P;
   end Reply;

   procedure Test_SNTP is
      Nonce : constant Unsigned_64 := 16#0123_4567_89AB_CDEF#;
      Q : constant Packet := Request (Nonce);
      Result : Estimate;
      Status : Rejection;
      MS : UTC_Milliseconds;
      OK : Boolean;
      Base : constant Unsigned_64 := 1_767_225_600_000;
      Good : constant Packet :=
        Reply (Nonce, Stamp (NTP_2026, 0), Stamp (NTP_2026, 10),
               Root_Delay => 16#0000_0CCD#, Root_Dispersion => 16#0000_0666#);
   begin
      pragma Assert (Q (1) = 16#23# and Q (41) = 16#01# and Q (48) = 16#EF#);
      pragma Assert (for all I in 2 .. 40 => Q (I) = 0);

      To_Unix_MS (Shift_Left (NTP_2026, 32), MS, OK);
      pragma Assert (OK and MS = Base);
      To_Unix_MS (Shift_Left (NTP_2026, 32) or 16#8000_0000#, MS, OK);
      pragma Assert (OK and MS = Base + 500);
      --  Era 1: 2^32 + 4096 NTP seconds is shortly after 2036-02-07.
      To_Unix_MS (Shift_Left (Unsigned_64'(4096), 32), MS, OK);
      pragma Assert (OK and MS = (2 ** 32 + 4096 - Unix_Offset) * 1_000);
      To_Unix_MS (Shift_Left (Unsigned_64 (Unix_Offset - 1), 32), MS, OK);
      pragma Assert (not OK); -- 1969
      To_Unix_MS (Shift_Left (Unsigned_64 (Unsigned_32'Last), 32), MS, OK);
      pragma Assert (OK); -- 2036-02-07, still before 2099
      To_Unix_MS (Shift_Left (Unsigned_64 (2 ** 30), 32), MS, OK);
      pragma Assert (OK and MS = (2 ** 32 + 2 ** 30 - Unix_Offset) * 1_000); -- 2070
      To_Unix_MS (Shift_Left (Unsigned_64 (2 ** 31 - 1), 32), MS, OK);
      pragma Assert (not OK); -- era 1 reaches 2104, past the supported range

      --  Round trip 100 ms, server hold 10 ms: path 90, return leg 45.
      Evaluate (Good, Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Accepted);
      pragma Assert (Result.UTC_MS = Base + 10 + 45 and Result.Observed_MS = 1_100);
      --  Root delay 0x0CCD/65536 s rounds up to 51 ms (half: 25) and
      --  dispersion 0x0666 to 25 ms, plus 1 ms local resolution.
      pragma Assert (Result.Uncertainty_MS = 45 + 25 + 25 + 1);

      Evaluate (Good (1 .. 47), Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Wrong_Length);
      declare
         Shifted : Byte_Array (2 .. 49);
      begin
         Shifted := Good;
         Evaluate (Shifted, Nonce, 1_000, 1_100, Result, Status);
         pragma Assert (Status = Wrong_Length);
      end;
      Evaluate (Reply (Nonce, Stamp (NTP_2026), Stamp (NTP_2026), First_Byte => 16#23#),
                Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Wrong_Mode);
      Evaluate (Reply (Nonce, Stamp (NTP_2026), Stamp (NTP_2026), First_Byte => 16#14#),
                Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Wrong_Version);
      Evaluate (Reply (Nonce, Stamp (NTP_2026), Stamp (NTP_2026), Stratum => 0),
                Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Kiss_Of_Death);
      Evaluate (Reply (Nonce, Stamp (NTP_2026), Stamp (NTP_2026), Stratum => 16),
                Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Bad_Stratum);
      Evaluate (Reply (Nonce, Stamp (NTP_2026), Stamp (NTP_2026), First_Byte => 16#E4#),
                Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Unsynchronized);
      Evaluate (Good, Nonce + 1, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Origin_Mismatch);
      Evaluate (Reply (Nonce, Stamp (NTP_2026), 0), Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Zero_Timestamp);
      Evaluate (Reply (Nonce, Stamp (NTP_2026, 20), Stamp (NTP_2026, 10)),
                Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Server_Time_Order);
      Evaluate (Good, Nonce, 1_100, 1_000, Result, Status);
      pragma Assert (Status = Local_Time_Order);
      Evaluate (Good, Nonce, 1_000, 1_000 + Maximum_Round_Trip_MS + 1, Result, Status);
      pragma Assert (Status = Round_Trip_Too_Long);
      Evaluate (Reply (Nonce, Stamp (NTP_2026), Stamp (NTP_2026),
                       Root_Dispersion => 16#0001_0100#), -- ~1004 ms
                Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Root_Distance_Too_Large);
      --  Server hold longer than the measured round trip: path delay is 0.
      Evaluate (Reply (Nonce, Stamp (NTP_2026, 0), Stamp (NTP_2026, 300)),
                Nonce, 1_000, 1_100, Result, Status);
      pragma Assert (Status = Accepted and Result.UTC_MS = Base + 300 and
                     Result.Uncertainty_MS = 1);
   end Test_SNTP;

   procedure Test_Combine is
      Items : Estimate_Array := [others => <>];
      Result : Estimate;
      Agreeing : Server_Count;
      OK : Boolean;
      Base : constant Unsigned_64 := 1_767_225_600_000;
   begin
      Items (1) := (Base, 5_000, 20);
      Items (2) := (Base + 10, 5_000, 20);
      Items (3) := (Base - 5, 5_000, 20);
      Combine (Items, 3, Result, Agreeing, OK);
      pragma Assert (OK and Agreeing = 3);
      --  Intersection [Base-10 .. Base+15], nearest-below midpoint.
      pragma Assert (Result.UTC_MS = Base + 2 and Result.Observed_MS = 5_000);
      pragma Assert (Result.Uncertainty_MS = 13);

      --  A falseticker far away is outvoted.
      Items (3) := (Base + 3_600_000, 5_000, 20);
      Combine (Items, 3, Result, Agreeing, OK);
      pragma Assert (OK and Agreeing = 2 and Result.UTC_MS in Base - 10 .. Base + 30);

      --  Observations at different instants are projected forward.
      Items (1) := (Base, 5_000, 20);
      Items (2) := (Base + 1_000, 6_000, 20);
      Combine (Items, 2, Result, Agreeing, OK);
      pragma Assert (OK and Agreeing = 2 and Result.Observed_MS = 6_000 and
                     Result.UTC_MS = Base + 1_000);

      Combine (Items, 1, Result, Agreeing, OK);
      pragma Assert (not OK); -- one source never suffices
      Items (2) := (Base + 60_000, 5_000, 20);
      Combine (Items, 2, Result, Agreeing, OK);
      pragma Assert (not OK); -- two disagreeing sources
      Items := [(Base, 1, 20), (Base + 5, 1, 20), (Base + 90_000, 1, 20),
                (Base + 90_005, 1, 20)];
      Combine (Items, 4, Result, Agreeing, OK);
      pragma Assert (not OK); -- 2 against 2 is not a majority
      Combine (Items, 0, Result, Agreeing, OK);
      pragma Assert (not OK);
   end Test_Combine;

   procedure Test_Discipline is
      use Clock_Discipline;
      Item, Before : State;
      Outcome_Value : Outcome;
      UTC : Clock_Discipline.UTC_Milliseconds;
      OK : Boolean;
      RTC_S : constant Unsigned_64 := 1_790_000_000; -- 2026-09, above the floor
      Mono : constant Unsigned_64 := 50_000;
      function S (UTC_MS : Unsigned_64; Observed : Unsigned_64; U : Unsigned_32 := 50;
                  Sources : Natural := 3; Auth : Boolean := False) return Sample is
        (UTC_MS, Observed, U, Sources, Auth);
   begin
      Initialize (Item, True, 1_000_000_000, Mono, Floor_MS); -- 2001: below floor
      pragma Assert (not Available (Item) and Quality (Item) = Unknown_Time);
      Initialize (Item, False, RTC_S, Mono, Floor_MS);
      pragma Assert (not Available (Item));
      Initialize (Item, True, RTC_S, Mono, Floor_MS);
      pragma Assert (Available (Item) and Quality (Item) = RTC_Only);
      Current (Item, Mono + 1_500, UTC, OK);
      pragma Assert (OK and UTC = RTC_S * 1_000 + 1_500);
      Current (Item, Mono - 1, UTC, OK);
      pragma Assert (not OK);

      Before := Item;
      Apply (Item, S (RTC_S * 1_000, Mono + 11), Mono + 10, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Future_Observation and Item = Before);
      Apply (Item, S (RTC_S * 1_000, Mono), Mono + 10_001, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Stale and Item = Before);
      Apply (Item, S (RTC_S * 1_000, Mono, U => 1_001), Mono, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Uncertainty);
      Apply (Item, S (RTC_S * 1_000, Mono, Sources => 1), Mono, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Sources);
      Apply (Item, S (Floor_MS - 1, Mono), Mono, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Below_Floor);
      Apply (Item, S (Clock_Discipline.Latest_UTC_MS + 1, Mono), Mono, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Out_Of_Range);
      Apply (Item, S (Clock_Discipline.Latest_UTC_MS, Mono), Mono + 1, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Out_Of_Range);
      --  Unauthenticated: more than 15 minutes from the RTC is a conflict.
      Apply (Item, S (RTC_S * 1_000 + Maximum_Unauthenticated_Step_MS + 1, Mono),
             Mono, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Conflict and Item = Before);

      --  Within 15 minutes: stepped, quality becomes network-unauthenticated.
      Apply (Item, S (RTC_S * 1_000 + 600_000, Mono + 1_000), Mono + 1_200,
             Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Stepped and Quality (Item) = Network_Unauthenticated);
      Current (Item, Mono + 2_200, UTC, OK);
      pragma Assert (OK and UTC = RTC_S * 1_000 + 600_000 + 200 + 1_000);
      --  No walking: the bound is measured from the RTC, not the current time.
      Apply (Item, S (RTC_S * 1_000 + 600_000 + 600_000, Mono + 3_000), Mono + 3_000,
             Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Conflict);

      --  Authenticated time may move further and then outranks SNTP.
      Apply (Item, S (RTC_S * 1_000 + 3_600_000, Mono + 4_000, Sources => 1, Auth => True),
             Mono + 4_000, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Stepped and Quality (Item) = Network_Authenticated);
      Apply (Item, S (RTC_S * 1_000, Mono + 5_000), Mono + 5_000, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Conflict and Quality (Item) = Network_Authenticated);
      Apply (Item, S (RTC_S * 1_000, Mono + 5_000, Sources => 0, Auth => True),
             Mono + 5_000, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Rejected_Sources);

      --  With no usable RTC, any unauthenticated time above the floor works.
      Initialize (Item, True, 1_000_000_000, Mono, Floor_MS);
      Apply (Item, S (RTC_S * 1_000 + 86_400_000, Mono), Mono, Floor_MS, Outcome_Value);
      pragma Assert (Outcome_Value = Stepped and Available (Item));
   end Test_Discipline;

   procedure Test_Control is
      Item : constant Sample := (1_790_000_000_123, 99, 250, 3, True);
      Decoded : Sample;
      OK : Boolean;
      W : Words := Encode (Item);
   begin
      Decode (W, Decoded, OK);
      pragma Assert (OK and Decoded = Item);
      pragma Assert (Shift_Right (W (2), 40) = 1 and (Shift_Right (W (2), 32) and 255) = 3);
      W (3) := 1;
      Decode (W, Decoded, OK);
      pragma Assert (not OK);
      W := Encode (Item);
      W (2) := W (2) or Shift_Left (Unsigned_64'(1), 41);
      Decode (W, Decoded, OK);
      pragma Assert (not OK);
      pragma Assert (Is_Valid_Wall_Time (RTC_Only) and Is_Valid_Wall_Time (Network_Authenticated)
                     and not Is_Valid_Wall_Time (Invalid_Zone)
                     and not Is_Valid_Wall_Time (Unknown_Time));
   end Test_Control;

   procedure Test_Servers is
      use Server_List;
      Servers : Server_Array;
      Count : Server_Count;
      OK : Boolean;
      Text : String (1 .. Maximum_Scheme_Length);
      Length : Natural;
      Long_63 : constant String (1 .. 63) := [others => 'a'];
   begin
      Parse ("  time.example  b-2.example:124 10.0.2.2:18123 ", Servers, Count, OK);
      pragma Assert (OK and Count = 3);
      pragma Assert (Servers (1).Host (1 .. Servers (1).Length) = "time.example" and
                     Servers (1).Port = 123);
      pragma Assert (Servers (2).Port = 124 and Servers (3).Port = 18_123);
      Scheme (Servers (3), Text, Length);
      pragma Assert (Text (1 .. Length) = "@net:udp:10.0.2.2:18123");
      Scheme (Servers (1), Text, Length);
      pragma Assert (Text (1 .. Length) = "@net:udp:time.example:123");
      Parse (Long_63 & ":65535", Servers, Count, OK);
      pragma Assert (OK and Count = 1 and Servers (1).Port = 65_535);
      Scheme (Servers (1), Text, Length);
      pragma Assert (Length = Maximum_Scheme_Length);
      Parse ("", Servers, Count, OK); pragma Assert (not OK);
      Parse ("   ", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a:0", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a:65536", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a:", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a:12x", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a_b", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a:123456", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a b c d e", Servers, Count, OK); pragma Assert (not OK);
      Parse (Long_63 & "a", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a,b", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a::1", Servers, Count, OK); pragma Assert (not OK);
      Parse ("a b c d", Servers, Count, OK); pragma Assert (OK and Count = 4);
   end Test_Servers;
begin
   Test_SNTP;
   Test_Combine;
   Test_Discipline;
   Test_Control;
   Test_Servers;
   Ada.Text_IO.Put_Line
     ("Timesync: SNTP validation/estimate, era conversion, majority agreement, " &
      "clock floor/step/authentication policy, control encoding, server list PASS");
end Main;
