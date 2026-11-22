--  Hosted tests for CuBit.Filesystem_Events and Watch_Reserve
--  (docs/filesystem-protocol-v2.md step 4).
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with CuBit.Filesystem_Events; use CuBit.Filesystem_Events;
with Watch_Reserve;
with CuBit.Channel_Rings;

procedure Main is
   package WR renames Watch_Reserve;
   use type WR.Watch_State, WR.Decision;
   Failures, Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 20 then
            Ada.Text_IO.Put_Line ("FAIL: " & What);
         end if;
      end if;
   end Check;

   subtype Small is Natural range 0 .. 1_000_000;
   package Random is new Ada.Numerics.Discrete_Random (Small);
   G : Random.Generator;
   function R (N : Positive) return Natural is (Random.Random (G) mod N);

   function To_Name (Text : String; Length : out Name_Length) return Name_Bytes is
      Result : Name_Bytes := [others => 0];
   begin
      Length := Text'Length;
      for I in Text'Range loop
         Result (I - Text'First + 1) := Character'Pos (Text (I));
      end loop;
      return Result;
   end To_Name;

   function Relative_OK (Text : String) return Boolean is
      L : Name_Length;
      N : constant Name_Bytes := To_Name (Text, L);
   begin
      return Valid_Relative (N, L);
   end Relative_OK;

   Bytes : Record_Bytes;
   Used : Record_Length;
   Item, Back : Event;
   Name, Back_Name : Name_Bytes;
   Length, Back_Length : Name_Length;
   OK : Boolean;
begin
   Random.Reset (G, 2026_1008);
   --  Relative paths.
   Check (Relative_OK ("a") and Relative_OK ("a/b") and Relative_OK ("...") and Relative_OK (".a/b."),
          "good relative paths");
   Check (not Relative_OK ("") and not Relative_OK ("/a") and not Relative_OK ("a/")
          and not Relative_OK ("a//b") and not Relative_OK (".") and not Relative_OK ("a/..")
          and not Relative_OK ("./a") and not Relative_OK ("a" & Character'Val (0) & "b"),
          "bad relative paths");

   --  Round trips, and every truncation or damage of the header refused.
   for Round in 1 .. 20_000 loop
      declare
         Kind : constant Event_Kind := Event_Kind'Val (R (7));
         Text : String (1 .. 1 + R (300));
      begin
         for C of Text loop
            C := Character'Val (Character'Pos ('a') + R (26));
         end loop;
         if R (4) = 0 and then Text'Length > 3 then
            Text (2) := '/';
         end if;
         Name := To_Name (Text, Length);
         if not Named (Kind) then
            Length := 0;
            Name := [others => 0];
         end if;
         Item := (Watch => 1 + R (Maximum_Watches), Kind => Kind,
                  Flags => Unsigned_8 (R (2)), Object => Unsigned_64 (R (1_000_000)),
                  Cookie => Unsigned_64 (R (9)), Stamp => Unsigned_64 (Round));
         Encode (Item, Name, Length, Bytes, Used);
         Decode (Bytes, Used, Back, Back_Name, Back_Length, OK);
         Check (OK and then Back = Item and then Back_Length = Length
                and then Back_Name (1 .. Length) = Name (1 .. Length), "round trip");
         Decode (Bytes, Used - 1, Back, Back_Name, Back_Length, OK);
         Check (not OK, "short record refused");
         declare
            Damaged : Record_Bytes := Bytes;
         begin
            Damaged (Kind_At) := 0;
            Decode (Damaged, Used, Back, Back_Name, Back_Length, OK);
            Check (not OK, "unknown kind refused");
            Damaged := Bytes;
            Damaged (Watch_At) := 0;
            Damaged (Watch_At + 1) := 0;
            Decode (Damaged, Used, Back, Back_Name, Back_Length, OK);
            Check (not OK, "watch 0 refused");
         end;
         --  Garbage never faults.
         if Round mod 20 = 0 then
            for B of Bytes loop
               B := Unsigned_8 (R (256));
            end loop;
            Decode (Bytes, R (Largest_Record + 10), Back, Back_Name, Back_Length, OK);
         end if;
      end;
   end loop;

   --  The reserve: random event sizes, watches and a slow consumer. A model
   --  ring of free bytes; the invariant must hold, and every event is put
   --  or covered by a rescan record the client reads.
   declare
      Ring : constant := 16 * 4_096;
      Free : WR.Free_Bytes := Ring;
      Normal : WR.Normal_Count := 0;
      States : array (Watch_Number) of WR.Watch_State := [others => WR.Unused];
      Read : array (Watch_Number) of Boolean := [others => False];
      Result : WR.Decision;
      Put : Boolean;
      Puts, Rescans, Drops, Ends, Owed_Ends : Natural := 0;
   begin
      for Step in 1 .. 200_000 loop
         case R (40) is
            when 0 .. 3 =>   --  a new watch
               declare
                  W : constant Watch_Number := 1 + R (Maximum_Watches);
               begin
                  if States (W) = WR.Unused and then WR.May_Admit (Free, Normal) then
                     States (W) := WR.Watching;
                     Normal := Normal + 1;
                  end if;
               end;
            when 4 .. 11 =>   --  the client reads everything
               Free := Ring;
               Read := [others => True];
               --  Ended watches' numbers are free once their Watch_Ended is read.
               for S of States loop
                  if S = WR.Ended then
                     S := WR.Unused;
                  end if;
               end loop;
            when 12 =>   --  a watch ends (unwatched, or its folder went)
               declare
                  W : constant Watch_Number := 1 + R (Maximum_Watches);
               begin
                  if States (W) in WR.Live_State then
                     WR.End_Watch (States (W), Normal, Free, Put);
                     if Put then
                        Free := Free - WR.Reserve_Each;
                        Ends := Ends + 1;
                     else
                        Owed_Ends := Owed_Ends + 1;
                     end if;
                  end if;
               end;
            when others =>  --  an event
               declare
                  W : constant Watch_Number := 1 + R (Maximum_Watches);
                  Payload : constant Record_Length := Header_Bytes + R (Maximum_Name_Bytes);
               begin
                  if States (W) in WR.Live_State then
                     WR.Decide (States (W), Normal, Free, Payload, Read (W), Result);
                     case Result is
                        when WR.Put_Event =>
                           Free := Free - WR.Worst (Payload);
                           Puts := Puts + 1;
                        when WR.Put_Rescan =>
                           Free := Free - WR.Reserve_Each;
                           Read (W) := False;
                           Rescans := Rescans + 1;
                        when WR.Drop =>
                           Drops := Drops + 1;
                     end case;
                  end if;
               end;
         end case;
         for W in Watch_Number loop
            if States (W) /= WR.Unused then
               WR.Settle (States (W), Normal, Free, Read (W), Put);
               if Put then
                  Free := Free - WR.Reserve_Each;
                  Read (W) := False;
                  if States (W) = WR.Ended then
                     Ends := Ends + 1;
                  end if;
               end if;
            end if;
         end loop;
         if not WR.Holds (Free, Normal) then
            Check (False, "reserve invariant");
         end if;
         --  A watch is never left dropping without a record the client
         --  has not yet read, or one it still owes.
         for W in Watch_Number loop
            if States (W) = WR.Rescan_Posted and then Read (W) and then WR.May_Admit (Free, Normal) then
               Check (False, "settled watch left behind");
            end if;
            --  An owed Watch_Ended (or Rescan_Needed) goes in once there is room.
            if States (W) in WR.Rescan_Owed | WR.End_Owed
              and then Free >= WR.Reserve_Each * (Normal + 1)
            then
               Check (False, "owed record left behind");
            end if;
         end loop;
      end loop;
      Check (Puts > 0 and then Rescans > 0 and then Drops > 0 and then Ends > 0,
             "all paths exercised");
      Ada.Text_IO.Put_Line ("reserve:" & Puts'Image & " events," & Rescans'Image & " rescans,"
                            & Drops'Image & " covered," & Ends'Image & " ended ("
                            & Owed_Ends'Image & " owed first)");
   end;

   --  Directed: a watch ends with its reserve spent and the ring full. Its
   --  Watch_Ended is owed, not lost, and goes in once the client reads.
   declare
      State : WR.Watch_State := WR.Watching;
      Normal : WR.Normal_Count := 1;
      Free : WR.Free_Bytes := WR.Reserve_Each;
      Result : WR.Decision;
      Put : Boolean;
   begin
      WR.Decide (State, Normal, Free, Largest_Record, False, Result);
      Check (Result = WR.Put_Rescan and then Normal = 0, "full ring: rescan");
      Free := 0;
      WR.End_Watch (State, Normal, Free, Put);
      Check (not Put and then State = WR.End_Owed, "full ring: end owed");
      WR.Settle (State, Normal, Free, False, Put);
      Check (not Put and then State = WR.End_Owed, "still full: still owed");
      WR.Settle (State, Normal, WR.Reserve_Each, True, Put);
      Check (Put and then State = WR.Ended, "room: Watch_Ended goes in");
      --  A Watching watch always has room for its Watch_Ended.
      State := WR.Watching;
      Normal := 3;
      WR.End_Watch (State, Normal, 3 * WR.Reserve_Each, Put);
      Check (Put and then State = WR.Ended and then Normal = 2, "watching: ends at once");
   end;

   --  Reached: the client has read past a mark, across index wrap.
   declare
      use CuBit.Channel_Rings;
      Top : constant Index := Index'Last;
   begin
      Check (WR.Reached (10, 10, 0) and then WR.Reached (10, 12, 0) and then not WR.Reached (12, 10, 2)
             and then WR.Reached (Top, 1, 0) and then not WR.Reached (1, Top, 2), "reached");
   end;

   Ada.Text_IO.Put_Line ("FILESYSTEM-EVENTS:" & (if Failures = 0 then Checks'Image & " checks PASS"
                         else Failures'Image & " of" & Checks'Image & " checks FAIL"));
end Main;
