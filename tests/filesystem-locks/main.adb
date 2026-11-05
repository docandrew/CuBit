--  Hosted tests for File_Locks (Queue_Lock, flock semantics): fixed cases,
--  then random sequences of requests, releases, settles and expiries over
--  a few files and handles, checked after every step against properties
--  computed independently of the unit:
--    holders never conflict; the waiter count is exact; a request is
--    granted at once exactly when it is compatible and nobody waits for
--    the file; Settle grants only the oldest waiter of a file, and leaves
--    none that could be granted; Expire ends only waits whose deadline
--    passed, and leaves none; Cancel_Wait ends a wait and keeps the lock.
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with File_Locks;

procedure Main is
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

   Handles : constant := 8;
   Files : constant := 3;
   package L is new File_Locks (Capacity => Handles);
   use L;

   T : Table;
   R : Outcome;
   Released, Found, Was_Held, Was_Waiting : Boolean;
   G : Slot;

   function Count_Waiting (T : Table) return Natural is
      N : Natural := 0;
   begin
      for S in Slot loop
         if T.Slots (S).Waiting then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Count_Waiting;

   --  Computed here, not by the unit: may S hold Object as Wanted?
   function Fits (T : Table; S : Slot; Object : Object_Key; Wanted : Lock_Mode) return Boolean is
   begin
      for O in Slot loop
         if O /= S and then T.Slots (O).Held /= Unlocked and then T.Slots (O).Object = Object
           and then (Wanted = Exclusive or else T.Slots (O).Held = Exclusive)
         then
            return False;
         end if;
      end loop;
      return True;
   end Fits;

   --  The oldest waiter of Object (-1: none).
   function Oldest (T : Table; Object : Object_Key) return Integer is
      Best : Integer := -1;
   begin
      for S in Slot loop
         if T.Slots (S).Waiting and then T.Slots (S).Object = Object
           and then (Best < 0 or else T.Slots (S).Order < T.Slots (Best).Order)
         then
            Best := S;
         end if;
      end loop;
      return Best;
   end Oldest;

   function No_Conflict (T : Table) return Boolean is
   begin
      for A in Slot loop
         for B in Slot loop
            if A /= B and then T.Slots (A).Held /= Unlocked and then T.Slots (B).Held /= Unlocked
              and then T.Slots (A).Object = T.Slots (B).Object
              and then (T.Slots (A).Held = Exclusive or else T.Slots (B).Held = Exclusive)
            then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end No_Conflict;

   procedure Invariants (What : String) is
   begin
      Check (No_Conflict (T) and then Consistent (T), What & ": holders conflict");
      Check (T.Waiting_Count = Count_Waiting (T), What & ": waiter count");
   end Invariants;

   --  Settle Object until nothing more is granted, checking each grant.
   procedure Settle_All (Object : Object_Key; What : String; Grants : out Natural) is
      Head : Integer;
   begin
      Grants := 0;
      for Round in 1 .. Handles + 1 loop
         Head := Oldest (T, Object);
         Settle (T, Object, G, Found);
         exit when not Found;
         Grants := Grants + 1;
         Check (Head = G, What & ": granted a waiter that was not the oldest");
         Invariants (What & " settle");
      end loop;
      Head := Oldest (T, Object);
      Check (Head < 0 or else not Fits (T, Head, Object, T.Slots (Head).Wanted),
             What & ": a grantable waiter left waiting");
   end Settle_All;

   Grants : Natural := 0;
begin
   --  Fixed cases.
   Request (T, 0, 1, Shared, False, No_Deadline, R, Released);
   Check (R = Granted, "first shared granted");
   Request (T, 1, 1, Shared, False, No_Deadline, R, Released);
   Check (R = Granted, "second shared granted");
   Request (T, 2, 1, Exclusive, False, No_Deadline, R, Released);
   Check (R = Busy, "exclusive refused while shared held");
   Request (T, 2, 2, Exclusive, False, No_Deadline, R, Released);
   Check (R = Granted, "exclusive on another file granted");
   Request (T, 3, 2, Shared, False, No_Deadline, R, Released);
   Check (R = Busy, "shared refused while exclusive held");
   Request (T, 2, 2, Exclusive, False, No_Deadline, R, Released);
   Check (R = Granted and then not Released, "same mode again: granted, nothing released");
   Invariants ("fixed");

   --  An exclusive waiter is not starved by later shared requests.
   Request (T, 4, 1, Exclusive, True, No_Deadline, R, Released);
   Check (R = Queued, "exclusive queued behind shared holders");
   Request (T, 5, 1, Shared, True, No_Deadline, R, Released);
   Check (R = Queued, "shared queued behind the exclusive waiter");
   Request (T, 6, 1, Shared, False, No_Deadline, R, Released);
   Check (R = Busy, "shared try refused while a writer waits");
   Release (T, 0, Was_Held, Was_Waiting);
   Check (Was_Held and then not Was_Waiting, "release of a held lock");
   Settle_All (1, "one reader left", Grants);
   Check (Grants = 0, "writer waits for the last reader");
   Release (T, 1, Was_Held, Was_Waiting);
   Settle_All (1, "readers gone", Grants);
   Check (Grants = 1 and then Held (T, 4) = Exclusive, "writer granted");
   Release (T, 4, Was_Held, Was_Waiting);
   Settle_All (1, "writer gone", Grants);
   Check (Grants = 1 and then Held (T, 5) = Shared, "reader granted after the writer");
   Invariants ("fixed");

   --  Conversion releases first: shared to exclusive with another reader.
   Request (T, 6, 1, Shared, False, No_Deadline, R, Released);
   Check (R = Granted, "second reader");
   Request (T, 5, 1, Exclusive, False, No_Deadline, R, Released);
   Check (R = Busy and then Released and then Held (T, 5) = Unlocked,
          "upgrade refused, the shared lock released first");
   Release (T, 6, Was_Held, Was_Waiting);
   Request (T, 5, 1, Exclusive, False, No_Deadline, R, Released);
   Check (R = Granted and then not Released, "upgrade granted alone");
   Request (T, 5, 1, Shared, False, No_Deadline, R, Released);
   Check (R = Granted and then Released and then Held (T, 5) = Shared, "downgrade");
   Invariants ("fixed");

   --  Deadlines.
   Request (T, 7, 2, Shared, True, 100, R, Released);
   Check (R = Queued, "waiter with a deadline");
   Check (Earliest_Deadline (T) = 100, "earliest deadline");
   Expire (T, 99, G, Found);
   Check (not Found, "not expired before its deadline");
   Expire (T, 100, G, Found);
   Check (Found and then G = 7 and then not Waiting (T, 7) and then Held (T, 7) = Unlocked,
          "expired at its deadline");
   Check (Earliest_Deadline (T) = No_Deadline, "no deadline left");
   Request (T, 7, 2, Shared, True, No_Deadline, R, Released);
   Expire (T, Unsigned_64'Last - 1, G, Found);
   Check (not Found, "no deadline never expires");
   --  A waiter's handle goes: its wait ends.
   Release (T, 7, Was_Held, Was_Waiting);
   Check (not Was_Held and then Was_Waiting and then not Waiting (T, 7), "a wait cancelled");
   Invariants ("fixed");
   Release (T, 2, Was_Held, Was_Waiting);
   Release (T, 5, Was_Held, Was_Waiting);
   Settle_All (1, "cleanup", Grants);
   Check (Grants = 0, "nobody waited for file 1");
   Settle_All (2, "cleanup", Grants);
   Check (Grants = 0, "nobody waited for file 2");
   Check (Count_Waiting (T) = 0 and then T.Waiting_Count = 0, "nothing waits");

   --  Random sequences.
   declare
      subtype Choice is Natural range 0 .. 1_000_000;
      package Random is new Ada.Numerics.Discrete_Random (Choice);
      Gen : Random.Generator;
      function Pick (N : Positive) return Natural is (Random.Random (Gen) mod N);
      Now : Unsigned_64 := 0;
      Opened : array (Slot) of Object_Key;   --  the file each handle has open
      Expected : Outcome;
   begin
      Random.Reset (Gen, 2026_1102);
      for Run in 1 .. 2_000 loop
         T := (others => <>);
         for S in Slot loop
            Opened (S) := Object_Key (1 + Pick (Files));
         end loop;
         for Step in 1 .. 300 loop
            declare
               S : constant Slot := Pick (Handles);
               Object : constant Object_Key := Opened (S);
            begin
               case Pick (11) is
                  when 0 .. 4 =>
                     if not Waiting (T, S) then
                        declare
                           Wanted : constant Lock_Mode := (if Pick (2) = 0 then Shared else Exclusive);
                           Wait : constant Boolean := Pick (2) = 0;
                           Until_At : constant Unsigned_64 :=
                             (if Pick (3) = 0 then No_Deadline else Now + Unsigned_64 (Pick (50)));
                           Prior : constant Mode := Held (T, S);
                        begin
                           if Prior = Wanted then
                              Expected := Granted;
                           else
                              --  As if released first.
                              declare
                                 Copy : Table := T;
                              begin
                                 Copy.Slots (S).Held := Unlocked;
                                 Expected :=
                                   (if Fits (Copy, S, Object, Wanted) and then Oldest (Copy, Object) < 0
                                    then Granted elsif Wait then Queued else Busy);
                              end;
                           end if;
                           Request (T, S, Object, Wanted, Wait, Until_At, R, Released);
                           Check (R = Expected, "request outcome");
                           Check (Released = (Prior /= Unlocked and then Prior /= Wanted),
                                  "released only on a mode change");
                           Invariants ("request");
                           if Released then
                              Settle_All (Object, "after a conversion", Grants);
                           end if;
                        end;
                     end if;
                  when 5 .. 6 =>
                     Release (T, S, Was_Held, Was_Waiting);
                     Invariants ("release");
                     if Was_Held or else Was_Waiting then
                        Settle_All (Object, "after a release", Grants);
                     end if;
                  when 7 =>
                     --  The handle closes and the slot opens another file.
                     Release (T, S, Was_Held, Was_Waiting);
                     if Was_Held or else Was_Waiting then
                        Settle_All (Object, "after a close", Grants);
                     end if;
                     Opened (S) := Object_Key (1 + Pick (Files));
                  when 8 =>
                     --  The client's queue goes: its wait ends, its lock stays.
                     declare
                        Prior : constant Mode := Held (T, S);
                     begin
                        Cancel_Wait (T, S, Was_Waiting);
                        Check (Held (T, S) = Prior and then not Waiting (T, S), "cancel keeps the lock");
                        Invariants ("cancel");
                        if Was_Waiting then
                           Settle_All (Object, "after a cancel", Grants);
                        end if;
                     end;
                  when others =>
                     Now := Now + Unsigned_64 (Pick (20));
                     for Round in 1 .. Handles + 1 loop
                        Expire (T, Now, G, Found);
                        exit when not Found;
                        Check (T.Slots (G).Until_At <= Now, "expired before its deadline");
                        Invariants ("expire");
                        Settle_All (T.Slots (G).Object, "after an expiry", Grants);
                     end loop;
                     for O in Slot loop
                        Check (not Waiting (T, O) or else T.Slots (O).Until_At > Now,
                               "a passed deadline left waiting");
                     end loop;
                     Check (Earliest_Deadline (T) > Now, "earliest deadline after expiry");
               end case;
            end;
         end loop;
      end loop;
   end;

   Ada.Text_IO.Put_Line ("FILE-LOCKS:" & Checks'Image & " checks" &
                         (if Failures = 0 then " PASS" else Failures'Image & " FAIL"));
end Main;
