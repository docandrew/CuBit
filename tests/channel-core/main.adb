------------------------------------------------------------------------------
--  Hosted differential tests for the channel core (CAP-002 step 3):
--  Bell_Sets, Channel_Admission, Channel_Schedule and Request_Tags, each
--  against a straightforward reference model over random operations.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with CuBit.Grant_References;
with CuBit.Channel_Limits;
with CuBit.Channel_Declarations;
with Core_Instances; use Core_Instances;
with RC_Instance;
with System.Storage_Elements;
with Interfaces;

procedure Main is
   package GR renames CuBit.Grant_References;

   package Random_Natural is new Ada.Numerics.Discrete_Random (Natural);
   Gen : Random_Natural.Generator;
   function Pick (N : Positive) return Natural is (Random_Natural.Random (Gen) mod N);

   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 12 then
            Put_Line ("FAIL: " & What);
         end if;
      end if;
   end Check;

   Steps : constant := 200_000;

   ---------------------------------------------------------------------------
   procedure Test_Bells is
      --  Universe: 40 references (slots 0..19 x generations 1..2) over 8
      --  buckets of 2 ways, so buckets fill and registrations go Unrouted.
      subtype U is Natural range 0 .. 39;
      function Ref (I : U) return GR.Reference is
        ((slot => GR.Global_Slot (I mod 20), generation => GR.Generation (1 + I / 20)));
      Registered, Rung : array (U) of Boolean := [others => False];
      S : Bells.Set := Bells.Empty;
      R : Bells.Register_Result;
      Known, Was : Boolean;
      I : U;
      use type Bells.Register_Result;
   begin
      for Step in 1 .. Steps loop
         I := Pick (40);
         case Pick (4) is
            when 0 =>
               Bells.Register (S, Ref (I), R);
               if Registered (I) then
                  Check (R = Bells.Already, "register twice is Already");
               elsif R = Bells.Registered then
                  Registered (I) := True; Rung (I) := False;
               else
                  --  Unrouted only when its bucket holds Ways others.
                  declare
                     In_Bucket : Natural := 0;
                  begin
                     for J in U loop
                        if Registered (J) and then Bells.Bucket_Of (Ref (J)) = Bells.Bucket_Of (Ref (I)) then
                           In_Bucket := In_Bucket + 1;
                        end if;
                     end loop;
                     Check (In_Bucket = 2, "unrouted only for a full bucket");
                  end;
               end if;
            when 1 =>
               Bells.Unregister (S, Ref (I));
               Registered (I) := False; Rung (I) := False;
            when 2 =>
               Bells.Note (S, Ref (I), Known);
               Check (Known = Registered (I), "note knows exactly the registered");
               if Known then Rung (I) := True; end if;
            when others =>
               Bells.Take (S, Ref (I), Was);
               Check (Was = (Registered (I) and then Rung (I)), "take reports exactly its own ring");
               Rung (I) := False;
         end case;
         Check (Bells.Holds (S, Ref (I)) = Registered (I), "holds matches model");
      end loop;
   end Test_Bells;

   ---------------------------------------------------------------------------
   procedure Test_Admission is
      use Admission;
      use CuBit.Channel_Limits;
      --  Offer 4 is Server_Limit in total and per client: only the
      --  structural bound and the client table's size apply.
      Declared : constant Limit_Table :=
        [(Per_Client => (At_Most, 1), Total => (At_Most, 1)),
         (Per_Client => (At_Most, 2), Total => (At_Most, 5)),
         (Per_Client => (At_Most, 3), Total => (Kind => Server_Limit)),
         (Per_Client => (Kind => Server_Limit), Total => (Kind => Server_Limit))];
      Small_Size : constant := 6;
      Large_Size : constant := 12;
      Small : State (Small_Size);
      Large : State (Large_Size);
      Grown : Boolean := False;
      type Client_Range is range 1 .. 14;  --  more clients than records
      Model : array (Client_Range, Offer_Index) of Natural := [others => [others => 0]];
      function Total (O : Offer_Index) return Natural is
         T : Natural := 0;
      begin
         for C in Client_Range loop T := T + Model (C, O); end loop;
         return T;
      end Total;
      function Clients_In_Use return Natural is
         N : Natural := 0;
      begin
         for C in Client_Range loop
            if (for some O in Offer_Index => Model (C, O) > 0) then N := N + 1; end if;
         end loop;
         return N;
      end Clients_In_Use;
      function Cap (L : CuBit.Channel_Limits.Channel_Limit) return Natural is
        (if L.Kind = At_Most then Natural (L.Count) else Natural'Last);
      C : Client_Range;
      O : Offer_Index;
      Result : Core_Instances.Admission.Admission;
      procedure Step (S : in out State) is
      begin
         C := Client_Range (1 + Pick (14));
         O := Offer_Index (1 + Pick (4));
         if Pick (2) = 0 then
            Admit (S, O, Client_Id (C), Result);
            if Total (O) >= Cap (Declared (O).Total) then
               Check (Result = Total_Limit, "total limit");
            elsif Model (C, O) >= Cap (Declared (O).Per_Client) then
               Check (Result = Per_Client_Limit, "per-client limit");
            elsif (for all P in Offer_Index => Model (C, P) = 0) and then Clients_In_Use = S.Size then
               Check (Result = No_Client_Room, "no client room");
            else
               Check (Result = Admitted, "admitted");
               Model (C, O) := Model (C, O) + 1;
            end if;
         else
            Release (S, O, Client_Id (C));
            if Model (C, O) > 0 then Model (C, O) := Model (C, O) - 1; end if;
         end if;
         for P in Offer_Index loop
            Check (S.Totals (P) = Total (P), "totals equal the sum");
         end loop;
      end Step;
   begin
      Start (Small, Declared);
      for K in 1 .. Steps loop
         if not Grown then
            Step (Small);
            if K = Steps / 2 then
               Grow (Small, Large);
               Grown := True;
               for P in Offer_Index loop
                  Check (Large.Totals (P) = Small.Totals (P), "growth keeps totals");
               end loop;
            end if;
         else
            Step (Large);
         end if;
      end loop;
   end Test_Admission;

   ---------------------------------------------------------------------------
   procedure Test_Schedule is
      use Schedule;
      S : State (12);
      Found : Boolean;
      I : Index;
      subtype Index is Schedule.Index range 1 .. 12;
      Served_In_Turn : array (Index) of Natural := [others => 0];
      Ready_At_Start : array (Index) of Boolean := [others => False];
      procedure New_Turn is
      begin
         for J in Index loop
            if Served_In_Turn (J) = 0 and then Ready_At_Start (J) and then S.Active (J) and then S.Ready (J) then
               Check (False, "a channel ready all turn was not served");
            end if;
         end loop;
         Begin_Turn (S);
         Served_In_Turn := [others => 0];
         for J in Index loop Ready_At_Start (J) := S.Active (J) and then S.Ready (J); end loop;
      end New_Turn;
   begin
      for Step in 1 .. Steps loop
         I := Index (1 + Pick (12));
         case Pick (6) is
            when 0 =>
               if not S.Active (I) then
                  Open (S, I, Class'Val (Pick (3)));
                  Served_In_Turn (I) := 0;  --  a new channel in that position
               end if;
            when 1 =>
               if Pick (4) = 0 then Close (S, I); Ready_At_Start (I) := False; end if;
            when 2 | 3 =>
               if S.Active (I) then Mark_Ready (S, I); end if;
            when others =>
               Next (S, Found, I);
               if Found then
                  Check (S.Active (I) and then S.Ready (I) and then not S.Served (I), "eligible");
                  for J in Index loop
                     if S.Active (J) and then S.Ready (J) and then not S.Served (J) then
                        Check (S.Class_Of (J) >= S.Class_Of (I), "highest class first");
                     end if;
                  end loop;
                  Served_In_Turn (I) := Served_In_Turn (I) + 1;
                  Check (Served_In_Turn (I) = 1, "at most once per turn");
                  Served (S, I, Still_Ready => Pick (3) = 0);
               else
                  New_Turn;
               end if;
         end case;
      end loop;
   end Test_Schedule;

   ---------------------------------------------------------------------------
   procedure Test_Tags is
      use Tags;
      T : Table;
      Number : Tag;
      E : Entry_Index;
      R : Issue_Result;
      M : Match;
      Model_State : array (Entry_Index) of Entry_State := [others => Free];
      Model_Tag : array (Entry_Index) of Tag := [others => No_Tag];
      Last_Issued : Tag := No_Tag;
   begin
      for Step in 1 .. Steps loop
         case Pick (5) is
            when 0 | 1 =>
               Issue (T, Number, E, R);
               if (for all J in Entry_Index => Model_State (J) /= Free) then
                  Check (R = Full, "full");
               else
                  Check (R = Issued and then Model_State (E) = Free, "issued into a free entry");
                  Check (Number > Last_Issued, "tags only grow");
                  Last_Issued := Number;
                  Model_State (E) := Waiting; Model_Tag (E) := Number;
               end if;
            when 2 =>
               --  An answer: a current tag, an old one, or garbage.
               Number := (case Pick (3) is
                            when 0 => Model_Tag (Entry_Index (1 + Pick (8))),
                            when 1 => Tag (Pick (Natural (Last_Issued) + 2)),
                            when others => Tag (Pick (1000)));
               Complete (T, Number, E, M);
               declare
                  Owner : Natural := 0;
               begin
                  for J in Entry_Index loop
                     if Model_State (J) /= Free and then Model_Tag (J) = Number and then Number /= No_Tag then
                        Owner := J;
                     end if;
                  end loop;
                  if Owner = 0 then
                     Check (M = Unknown, "unknown tag changes nothing");
                  elsif Model_State (Owner) = Waiting then
                     Check (M = Matched and then E = Owner, "matched its own entry");
                     Model_State (Owner) := Answered;
                  else
                     Check (M = Duplicate, "duplicate answer refused");
                  end if;
               end;
            when 3 =>
               if Pick (8) = 0 then
                  Fail_All (T);
                  for J in Entry_Index loop
                     if Model_State (J) = Waiting then Model_State (J) := Ended; end if;
                  end loop;
               end if;
            when others =>
               E := Entry_Index (1 + Pick (8));
               if Model_State (E) in Answered | Ended then
                  Release (T, E);
                  Model_State (E) := Free; Model_Tag (E) := No_Tag;
               end if;
         end case;
         for J in Entry_Index loop
            Check (T.Entries (J).State = Model_State (J) and then T.Entries (J).Number = Model_Tag (J),
                   "table equals model");
         end loop;
      end loop;
   end Test_Tags;


   ---------------------------------------------------------------------------
   --  The request channel over two in-memory regions: a client and a
   --  server that sleep only after arming and wake only when rung. A lost
   --  wake shows as both asleep with work waiting.
   procedure Test_Request_Channel is
      use RC_Instance;
      use Interfaces;
      use type RC.Token;
      type Region is array (1 .. 8 * RC.Page_Bytes) of System.Storage_Elements.Storage_Element
        with Alignment => 4096;
      Client_Region, Server_Region : aliased Region := [others => 0];
      C : RC.Client;
      S : RC.Server;
      Client_Asleep, Server_Asleep : Boolean := False;
      Issued, Answered : Unsigned_64 := 0;
      Pending : array (1 .. 64) of RC.Token := [others => 0];
      Pending_Count : Natural := 0;
      Seen : array (Unsigned_64 range 1 .. Unsigned_64 (Steps) + 1) of Boolean := [others => False];
      Item : RC.Queues.Submission;
      Answer : RC.Queues.Completion;
      Got, Ring, Submitted, Armed : Boolean;
   begin
      RC.Attach_Client (C, Client_Region'Address, Server_Region'Address);
      RC.Attach_Server (S, Server_Region'Address, Client_Region'Address);
      for Step in 1 .. Steps loop
         case Pick (6) is
            when 0 | 1 =>   --  the client submits, if awake
               if not Client_Asleep and then Issued < Unsigned_64 (Steps) then
                  RC.Submit (C, RC.Token (Issued + 1), (Value => Issued + 1), Submitted, Ring);
                  if Submitted then
                     Issued := Issued + 1;
                     if Ring then Server_Asleep := False; end if;
                  end if;
               end if;
            when 2 =>   --  the client reaps everything, then may sleep
               if not Client_Asleep then
                  loop
                     RC.Reap (C, Answer, Got);
                     exit when not Got;
                     Check (Answer.Answer.Value = Unsigned_64 (Answer.Tag) * 3, "answer matches its request");
                     Check (not Seen (Unsigned_64 (Answer.Tag)), "each request answered once");
                     Seen (Unsigned_64 (Answer.Tag)) := True;
                     Answered := Answered + 1;
                  end loop;
                  if Pick (3) = 0 and then RC.Outstanding (C) > 0 then
                     Armed := RC.Arm_For_Completions (C);
                     Client_Asleep := Armed;
                  end if;
               end if;
            when 3 | 4 =>   --  the server takes some, answers some later
               if not Server_Asleep then
                  for K in 1 .. 1 + Pick (3) loop
                     RC.Take (S, Item, Got, Ring);
                     if Ring then Client_Asleep := False; RC.Disarm_For_Room (C); end if;
                     exit when not Got;
                     Pending_Count := Pending_Count + 1;
                     Pending (Pending_Count) := Item.Tag;
                     Check (Item.Item.Value = Unsigned_64 (Item.Tag), "request copied intact");
                  end loop;
                  if Pending_Count = 0 and then Pick (2) = 0 then
                     --  The race the re-check covers: a request lands after
                     --  the server last looked and before it arms (the
                     --  client sees no armed word, so it does not ring).
                     if Pick (2) = 0 and then not Client_Asleep and then Issued < Unsigned_64 (Steps) then
                        RC.Submit (C, RC.Token (Issued + 1), (Value => Issued + 1), Submitted, Ring);
                        if Submitted then
                           Issued := Issued + 1;
                        end if;
                     end if;
                     Armed := RC.Arm_For_Requests (S);
                     Server_Asleep := Armed;
                  end if;
               end if;
            when others =>   --  the server answers its oldest pending
               if not Server_Asleep and then Pending_Count > 0 then
                  RC.Complete (S, Pending (1), (Value => Unsigned_64 (Pending (1)) * 3), Ring);
                  Pending (1 .. Pending_Count - 1) := Pending (2 .. Pending_Count);
                  Pending_Count := Pending_Count - 1;
                  if Ring then Client_Asleep := False; RC.Disarm_For_Completions (C); end if;
               end if;
         end case;
         --  Woken sides disarm, as the glue does after a wake.
         if not Server_Asleep then RC.Disarm_For_Requests (S); end if;
         Check (not (Client_Asleep and then Server_Asleep and then
                     (RC.Outstanding (C) > 0 or else Pending_Count > 0)),
                "no lost wake: both asleep with work waiting");
         Check (RC.Owed (S) = Pending_Count, "owed equals pending");
         Check (RC.Owed (S) <= 4, "never more owed than completion slots");
         if Client_Asleep and then Server_Asleep then
            --  An outside event wakes the client (as a timeout would).
            Client_Asleep := False; RC.Disarm_For_Completions (C);
         end if;
      end loop;
      Check (Answered <= Issued, "no answer without a request");
   end Test_Request_Channel;


   --  A hostile client: it writes its Submitted index ahead of what it
   --  produced. The server must still never owe more answers than it has
   --  completion slots, and accepts no index beyond the ring.
   procedure Test_Hostile_Client is
      use RC_Instance;
      use Interfaces;
      type Region is array (1 .. 8 * RC.Page_Bytes) of System.Storage_Elements.Storage_Element
        with Alignment => 4096;
      Client_Region, Server_Region : aliased Region := [others => 0];
      S : RC.Server;
      Item : RC.Queues.Submission;
      Got, Ring : Boolean;
      Submitted : Unsigned_32 with Import, Volatile, Address => Client_Region'Address;
      Reaped : Unsigned_32 with Import, Volatile,
        Address => System.Storage_Elements."+" (Client_Region'Address, System.Storage_Elements.Storage_Offset (RC.Reaped_At));
   begin
      RC.Attach_Server (S, Server_Region'Address, Client_Region'Address);
      for Step in 1 .. Steps / 10 loop
         Submitted := Submitted + Unsigned_32 (if Pick (20) = 0 then Pick (40) else Pick (3));   --  sometimes past the ring
         if Pick (5) = 0 then Reaped := Reaped + Unsigned_32 (Pick (6)); end if;
         RC.Take (S, Item, Got, Ring);
         Check (RC.Owed (S) <= 4, "a hostile client cannot make the server overcommit");
         if RC.Owed (S) > 0 and then Pick (2) = 0 then
            RC.Complete (S, 1, (Value => 0), Ring);
         end if;
      end loop;
   end Test_Hostile_Client;


   ---------------------------------------------------------------------------
   --  .cubit.channels: random valid tables round trip; flipped bytes of an
   --  encoding are refused or decode to a valid table.
   procedure Test_Declarations is
      package CD renames CuBit.Channel_Declarations;
      package CL renames CuBit.Channel_Limits;
      use type CD.Decode_Result, CL.Limit_Kind, CD.Offer_Array, Interfaces.Unsigned_8;
      Letters : constant String := "abcdefgh.-_0123456789";
      function Random_Limit return CL.Channel_Limit is
        (if Pick (3) = 0 then (Kind => CL.Server_Limit)
         else (Kind => CL.At_Most, Count => 1 + Pick (CL.Maximum_Total)));
      T, Back : CD.Table;
      Raw : CD.Bytes (1 .. CD.Maximum_Table_Bytes);
      Length : CD.Table_Length;
      Outcome : CD.Decode_Result;
   begin
      for Step in 1 .. Steps / 20 loop
         T := (Count => 0, Offers => [others => (others => <>)]);
         for I in 1 .. Pick (CD.Maximum_Offers + 1) loop
            declare
               O : CD.Offer;
               L : constant Positive := 1 + Pick (CD.Maximum_Contract_Bytes);
            begin
               O.Contract.Length := L;
               for K in 1 .. L loop
                  O.Contract.Text (K) := Letters (1 + Pick (Letters'Length));
               end loop;
               --  Distinct: a unique suffix digit run would be complex; tag
               --  the first byte with the index.
               O.Contract.Text (1) := Character'Val (Character'Pos ('a') + I);
               O.Per_Client := Random_Limit;
               O.Total := Random_Limit;
               if O.Per_Client.Kind = CL.At_Most and then O.Total.Kind = CL.At_Most
                 and then O.Per_Client.Count > O.Total.Count
               then
                  O.Per_Client := O.Total;
               end if;
               O.Class := CL.Channel_Class'Val (Pick (3));
               T.Count := I;
               T.Offers (I) := O;
            end;
         end loop;
         CD.Encode (T, Raw, Length);
         CD.Decode (Raw (1 .. Length), Back, Outcome);
         Check (Outcome = CD.Decoded and then Back.Count = T.Count
                and then Back.Offers (1 .. Back.Count) = T.Offers (1 .. T.Count),
                "declarations round trip");
         --  Noise: flip a byte.
         if Length > 0 then
            declare
               At_Byte : constant Positive := 1 + Pick (Length);
            begin
               Raw (At_Byte) := Raw (At_Byte) xor Interfaces.Unsigned_8 (1 + Pick (255));
               CD.Decode (Raw (1 .. Length), Back, Outcome);
               Check (Outcome /= CD.Decoded or else CD.Valid_Table (Back), "noise decodes only valid tables");
            end;
         end if;
      end loop;
   end Test_Declarations;

   Seed : Integer := 20261017;
begin
   if Ada.Command_Line.Argument_Count > 0 then
      Seed := Integer'Value (Ada.Command_Line.Argument (1));
   end if;
   Random_Natural.Reset (Gen, Seed);
   Test_Bells;
   Test_Admission;
   Test_Schedule;
   Test_Tags;
   Test_Request_Channel;
   Test_Hostile_Client;
   Test_Declarations;
   Put_Line ("channel-core: checks" & Checks'Image & ", failures" & Failures'Image);
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
