with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Click_Sequences; use CuBit.Click_Sequences;
with CuBit.Presentation_State;
procedure Main is
   S : State;
   Kind : Press_Kind;
   procedure First (At_Point : Point := (100, 100);
                    Now_Ms : Unsigned_64 := 100) is
   begin
      Reset (S);
      Press (S, 1, At_Point, Now_Ms, Kind);
      pragma Assert (Kind = Single_Press);
      Release (S, At_Point, Now_Ms + 20);
   end First;
begin
   for Delay_Ms in Unsigned_64 range 20 .. 501 loop
      First;
      Press (S, 1, (100, 100), 100 + Delay_Ms, Kind);
      pragma Assert ((Kind = Double_Press) = (Delay_Ms <= 500));
   end loop;
   for Delta_X in -6 .. 6 loop
      for Delta_Y in -6 .. 6 loop
         First;
         Press (S, 1, (100 + Delta_X, 100 + Delta_Y), 200, Kind);
         pragma Assert ((Kind = Double_Press) =
           (abs Delta_X <= 4 and abs Delta_Y <= 4));
      end loop;
   end loop;
   First;
   Press (S, 1, (100, 100), 200, Kind);
   pragma Assert (Kind = Double_Press);
   Release (S, (100, 100), 220);
   Press (S, 1, (100, 100), 250, Kind);
   pragma Assert (Kind = Single_Press);
   Release (S, (100, 100), 270);
   Press (S, 1, (100, 100), 300, Kind);
   pragma Assert (Kind = Double_Press);
   First;
   Press (S, 2, (100, 100), 200, Kind);
   pragma Assert (Kind = Single_Press);
   First;
   Motion (S, (105, 100)); Motion (S, (100, 100));
   Press (S, 1, (100, 100), 200, Kind);
   pragma Assert (Kind = Single_Press);
   Reset (S);
   Press (S, 1, (100, 100), 100, Kind);
   Motion (S, (100, 105)); Motion (S, (100, 100));
   Release (S, (100, 100), 120);
   Press (S, 1, (100, 100), 200, Kind);
   pragma Assert (Kind = Single_Press);
   Reset (S);
   Press (S, 1, (100, 100), 100, Kind);
   Press (S, 1, (100, 100), 200, Kind);
   pragma Assert (Kind = Single_Press); -- no intervening release
   First; Reset (S);
   Press (S, 1, (100, 100), 200, Kind);
   pragma Assert (Kind = Single_Press);
   First;
   Press (S, No_Target, (100, 100), 150, Kind);
   Press (S, 1, (100, 100), 200, Kind);
   pragma Assert (Kind = Single_Press);
   First;
   Press (S, 1, (100, 100), 119, Kind);
   pragma Assert (Kind = Single_Press); -- time went backwards after release
   First;
   Press (S, 1, (100, 100), Unsigned_64'Last, Kind);
   pragma Assert (Kind = Single_Press);
   First ((Natural'Last, Natural'Last), Unsigned_64'Last - 100);
   Press (S, 1, (0, 0), Unsigned_64'Last - 1, Kind);
   pragma Assert (Kind = Single_Press);
   First ((0, 0), Unsigned_64'Last - 100);
   Press (S, 1, (0, 0), Unsigned_64'Last - 1, Kind);
   pragma Assert (Kind = Double_Press);
   Put_Line ("PASS: time/target/slop/release-qualified disjoint double clicks");
   declare
      package P is new CuBit.Presentation_State (Maximum_Identifier => 3);
      use P;
      type Event_Set is array (P.Event) of Boolean;
      Oracle : constant array (P.Phase) of Event_Set :=
        [Idle => [others => False],
         Queued => [Begin_Read | Discard => True, others => False],
         Reading => [Release_Buffer | Was_Presented | Discard => True,
                     others => False],
         Copied => [Was_Presented | Discard => True, others => False],
         Shown_Held | Dropped_Held => [Release_Buffer => True, others => False],
         Shown_Released | Dropped_Released => [Retire => True, others => False]];
      type Next_Set is array (P.Event) of P.Phase;
      Expected_Next : constant array (P.Phase) of Next_Set :=
        [Idle => [others => Idle],
         Queued => [Begin_Read => Reading, Discard => Dropped_Held,
                    others => Queued],
         Reading => [Release_Buffer => Copied, Was_Presented => Shown_Held,
                     Discard => Dropped_Held, others => Reading],
         Copied => [Was_Presented => Shown_Released, Discard => Dropped_Released,
                    others => Copied],
         Shown_Held => [Release_Buffer => Shown_Released, others => Shown_Held],
         Dropped_Held => [Release_Buffer => Dropped_Released, others => Dropped_Held],
         Shown_Released => [Retire => Idle, others => Shown_Released],
         Dropped_Released => [Retire => Idle, others => Dropped_Released]];
      Item, Before : P.State;
      Result : Admission;
      ID, Ignored_ID : Submission_ID;
      Applied, Returned : Boolean;
      procedure Do_Event (Action : Event) is
      begin
         Apply (Item, Identifier (Item), Action, Applied, Returned);
         pragma Assert (Applied);
      end Do_Event;
      function Fixture (Wanted : Phase) return P.State is
         Fresh : P.State;
      begin
         Item := Fresh;
         if Wanted /= Idle then
            Submit (Item, Result, ID);
            pragma Assert (Result = Accepted);
            if Wanted in Reading | Copied | Shown_Held | Shown_Released then
               Do_Event (Begin_Read);
            end if;
            if Wanted = Copied then Do_Event (Release_Buffer); end if;
            if Wanted in Shown_Held | Shown_Released then
               Do_Event (Was_Presented);
            elsif Wanted in Dropped_Held | Dropped_Released then
               Do_Event (Discard);
            end if;
            if Wanted in Shown_Released | Dropped_Released then
               Do_Event (Release_Buffer);
            end if;
         end if;
         pragma Assert (Current (Item) = Wanted);
         return Item;
      end Fixture;
   begin
      for Stage in Phase loop
         for Action in Event loop
            Item := Fixture (Stage);
            Before := Item;
            Apply (Item, 99, Action, Applied, Returned);
            pragma Assert (not Applied and not Returned and Item = Before);
            Apply (Item, No_Submission, Action, Applied, Returned);
            pragma Assert (not Applied and not Returned and Item = Before);
            Apply (Item, Identifier (Item), Action, Applied, Returned);
            pragma Assert (Applied = Oracle (Stage) (Action));
            pragma Assert (Current (Item) = Expected_Next (Stage) (Action));
            pragma Assert (Returned = (Applied and Action = Release_Buffer));
            if Returned then
               pragma Assert (Buffer_Held (Before) and not Buffer_Held (Item));
               Apply (Item, Identifier (Item), Action, Applied, Returned);
               pragma Assert (not Applied and not Returned);
            end if;
         end loop;
         Item := Fixture (Stage);
         Before := Item;
         Close (Item);
         pragma Assert (Is_Closed (Item));
         pragma Assert (Buffer_Held (Item) = Buffer_Held (Before));
         pragma Assert (Current (Item) =
           (if Stage = Queued then Dropped_Held else Stage));
         Before := Item;
         Submit (Item, Result, Ignored_ID);
         pragma Assert (Result = Closed and Item = Before and Ignored_ID = 0);
         Close (Item);
         pragma Assert (Item = Before);
      end loop;
      Item := Fixture (Idle);
      for Sequence in Submission_ID range 1 .. 3 loop
         Submit (Item, Result, ID);
         pragma Assert (Result = Accepted and ID = Sequence);
         Before := Item;
         Submit (Item, Result, Ignored_ID);
         pragma Assert (Result = Busy and Item = Before and Ignored_ID = 0);
         Apply (Item, ID - 1, Begin_Read, Applied, Returned);
         pragma Assert (not Applied and Item = Before);
         Do_Event (Discard);
         Do_Event (Release_Buffer);
         pragma Assert (Returned);
         Do_Event (Retire);
      end loop;
      Before := Item;
      Submit (Item, Result, ID);
      pragma Assert (Result = Exhausted and Item = Before and ID = 0);
      -- Closing a reader does not return its buffer or retire it early.
      Item := Fixture (Reading);
      Close (Item);
      Apply (Item, Identifier (Item), Retire, Applied, Returned);
      pragma Assert (not Applied and not Returned and Buffer_Held (Item));
      Do_Event (Discard); Do_Event (Release_Buffer); Do_Event (Retire);
      Submit (Item, Result, ID);
      pragma Assert (Result = Closed);
      -- Source return does not imply pending scanout can be cancelled.
      Item := Fixture (Copied);
      Close (Item);
      Apply (Item, Identifier (Item), Retire, Applied, Returned);
      pragma Assert (not Applied and not Returned and Current (Item) = Copied);
      Do_Event (Was_Presented); Do_Event (Retire);
      Put_Line ("PASS: submission transition matrix, stale IDs, release-once, close and exhaustion");
   end;
end Main;
