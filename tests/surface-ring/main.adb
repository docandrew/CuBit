------------------------------------------------------------------------------
--  Hosted tests for the surface ring (IPC-004): the element codec
--  (Compositor_Surface_Wire) and its request channel instance
--  (Compositor_Surface_Ring) over two in-memory regions.
--
--  What the channel test shows: a frame, its retirement and input go
--  through shared memory with no wake owed while the peer is awake (the
--  zero-system-call steady state); a sleeping peer is owed exactly one wake
--  per arming, however many requests or answers follow.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Desktop_Protocol;
with CuBit.Grant_References;
with Compositor_Surface_Ring;
with Compositor_Surface_Wire;

procedure Main is
   package DP renames CuBit.Desktop_Protocol;
   package Wire renames Compositor_Surface_Wire;
   package SR renames Compositor_Surface_Ring;
   package RC renames SR.RC;
   use type DP.Status_Code, DP.Input_Event_Kind, Wire.Request_Kind;
   use type Wire.Request, Wire.Result, Wire.Result_Decoding, Wire.Request_Words;
   use type RC.Token;

   Failures : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   Grant : constant CuBit.Grant_References.Reference := (slot => 7, generation => 3);

   procedure Request_Round_Trips is
      Items : constant array (1 .. 5) of Wire.Request :=
        [(Kind => Wire.Present, Surface => 4, Area => (0, 0, 0, 0), Input_After => 0),
         (Kind => Wire.Present, Surface => 9, Area => (3, 5, 20, 10), Input_After => 77),
         (Kind => Wire.Publish, Surface => 2, Area => (1, 2, 3, 4), Input_After => 12,
          Epoch => 5, Grant => Grant),
         (Kind => Wire.Await_Retirement, Surface => 2, Retiring_Epoch => 5, Ticket => 11),
         (Kind => Wire.Receive_Input, Surface => 2, After => 41)];
   begin
      for Item of Items loop
         declare
            Back : constant Wire.Request_Decoding := Wire.Decode (Wire.Encode (Item));
         begin
            Check (Back.Valid and then Back.Value = Item, "request round trip " & Item.Kind'Image);
         end;
      end loop;
   end Request_Round_Trips;

   procedure Request_Rejections is
      Good : constant Wire.Request_Words :=
        Wire.Encode ((Kind => Wire.Present, Surface => 4, Area => (1, 1, 2, 2), Input_After => 0));
      W : Wire.Request_Words;
   begin
      W := Good; W.Kind := 0;
      Check (not Wire.Decode (W).Valid, "unknown kind rejected");
      W := Good; W.Kind := 5;
      Check (not Wire.Decode (W).Valid, "kind past the last rejected");
      W := Good; W.Surface := 0;
      Check (not Wire.Decode (W).Valid, "zero surface rejected");
      W := Good; W.A := Wire.Pack ((0, 0, 4, 0));
      Check (not Wire.Decode (W).Valid, "mixed zero extent rejected");
      W := Good; W.A := Wire.Pack ((3, 0, 0, 0));
      Check (not Wire.Decode (W).Valid, "empty damage away from the origin rejected");
      W := Good; W.C := 1;
      Check (not Wire.Decode (W).Valid, "present spare word rejected");
      W := Wire.Encode ((Kind => Wire.Publish, Surface => 2, Area => (0, 0, 0, 0), Input_After => 0,
                         Epoch => 1, Grant => Grant));
      W.D := 0;
      Check (not Wire.Decode (W).Valid, "publish without a grant rejected");
      W.D := CuBit.Grant_References.Encode (Grant); W.C := 0;
      Check (not Wire.Decode (W).Valid, "publish epoch zero rejected");
      W.C := 2 ** 31;
      Check (not Wire.Decode (W).Valid, "publish epoch past an identity rejected");
      W := Wire.Encode ((Kind => Wire.Await_Retirement, Surface => 2, Retiring_Epoch => 1, Ticket => 1));
      W.B := 0;
      Check (not Wire.Decode (W).Valid, "retirement ticket zero rejected");
      W := Wire.Encode ((Kind => Wire.Receive_Input, Surface => 2, After => 0));
      W.B := 1;
      Check (not Wire.Decode (W).Valid, "input spare word rejected");
   end Request_Rejections;

   function Key (Serial : Unsigned_64) return DP.Input_Envelope is
     ((DP.Key_Pressed, Serial, 16#1E#, 0, False));

   procedure Result_Codec is
      Batch : Wire.Result := (Kind => Wire.Receive_Input, Status => DP.Success, Count => 3, More => True,
                              Events => [others => (others => <>)]);
      W : Wire.Result_Words;
   begin
      Batch.Events (1) := Key (5);
      Batch.Events (2) := (DP.Key_Released, 6, 16#1E#, 0, False);
      Batch.Events (3) := (DP.Surface_Configured, 9, 320, 200, False);
      declare
         Back : constant Wire.Result_Decoding := Wire.Decode (Wire.Encode (Batch), Wire.Receive_Input);
      begin
         Check (Back.Valid and then Back.Value = Batch, "input batch round trip");
      end;
      Check (not Wire.Decode (Wire.Encode (Batch), Wire.Present).Valid, "answer of another kind rejected");
      W := Wire.Encode (Batch); W.Events (2).Serial := 5;
      Check (not Wire.Decode (W, Wire.Receive_Input).Valid, "repeated serial rejected");
      W := Wire.Encode (Batch); W.Events (3).Serial := 4;
      Check (not Wire.Decode (W, Wire.Receive_Input).Valid, "decreasing serial rejected");
      W := Wire.Encode (Batch); W.Events (1).Kind := 0;
      Check (not Wire.Decode (W, Wire.Receive_Input).Valid, "No_Input inside a batch rejected");
      W := Wire.Encode (Batch); W.Events (1).Payload0 := 200;
      Check (not Wire.Decode (W, Wire.Receive_Input).Valid, "invalid scan code rejected");
      W := Wire.Encode (Batch); W.Count := 0;
      Check (not Wire.Decode (W, Wire.Receive_Input).Valid, "empty successful batch rejected");
      W := Wire.Encode (Batch); W.Count := Wire.Input_Batch + 1;
      Check (not Wire.Decode (W, Wire.Receive_Input).Valid, "oversized batch rejected");
      W := Wire.Encode (Batch); W.A := 2;
      Check (not Wire.Decode (W, Wire.Receive_Input).Valid, "malformed More rejected");
      declare
         Receipt : constant Wire.Result := (Kind => Wire.Publish, Status => DP.Success, Epoch => 5, Ticket => 11);
         Refused : constant Wire.Result := (Kind => Wire.Await_Retirement, Status => DP.Bad_State, others => <>);
      begin
         Check (Wire.Decode (Wire.Encode (Receipt), Wire.Publish) = (True, Receipt), "receipt round trip");
         Check (Wire.Decode (Wire.Encode (Refused), Wire.Await_Retirement) = (True, Refused), "refusal round trip");
         W := Wire.Encode (Refused); W.A := 1;
         Check (not Wire.Decode (W, Wire.Await_Retirement).Valid, "refusal carrying data rejected");
         W := Wire.Encode (Receipt); W.B := 0;
         Check (not Wire.Decode (W, Wire.Publish).Valid, "receipt ticket zero rejected");
         W := Wire.Encode (Receipt); W.Status := 99;
         Check (not Wire.Decode (W, Wire.Publish).Valid, "unknown status rejected");
      end;
   end Result_Codec;

   --  Exhaustive single-field sweep: no word value makes a request decode
   --  into something the encoder would not have written.
   procedure Request_Sweep is
      Values : constant array (1 .. 9) of Unsigned_64 :=
        [0, 1, 2, 3, 4, 5, 16#FFFF#, 2 ** 31, Unsigned_64'Last];
      W : Wire.Request_Words;
      D : Wire.Request_Decoding;
   begin
      for K in Values'Range loop
         for S in Values'Range loop
            for A in Values'Range loop
               for B in Values'Range loop
                  W := (Values (K), Values (S), Values (A), Values (B), 0, 0);
                  D := Wire.Decode (W);
                  if D.Valid then
                     Check (Wire.Encode (D.Value) = W, "decode is the encoder's inverse");
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
   end Request_Sweep;

   --  The window and Desktop over two in-memory regions.
   procedure Channel is
      type Region is array (1 .. 8 * RC.Page_Bytes) of System.Storage_Elements.Storage_Element
        with Alignment => 4096;
      Client_Region, Server_Region : aliased Region := [others => 0];
      C : RC.Client;
      S : RC.Server;
      Submitted, Ring, Got : Boolean;
      Item : RC.Queues.Submission;
      Answer : RC.Queues.Completion;
      Wakes_Owed : Natural := 0;
   begin
      Check (RC.Client_Pages * RC.Page_Bytes <= Region'Length and then
             RC.Server_Pages * RC.Page_Bytes <= Region'Length, "regions hold both rings");
      RC.Attach_Client (C, Client_Region'Address, Server_Region'Address);
      RC.Attach_Server (S, Server_Region'Address, Client_Region'Address);
      --  Busy-busy: neither side armed. A frame, its retirement wait and the
      --  input request go and come back with no wake owed either way.
      for Frame in 1 .. 100 loop
         RC.Submit (C, RC.Token (3 * Frame - 2), Wire.Encode
           ((Kind => Wire.Publish, Surface => 2, Area => (0, 0, 0, 0), Input_After => 0,
             Epoch => 1, Grant => Grant)), Submitted, Ring);
         Check (Submitted and not Ring, "busy publish owes no wake");
         RC.Submit (C, RC.Token (3 * Frame - 1), Wire.Encode
           ((Kind => Wire.Await_Retirement, Surface => 2, Retiring_Epoch => 1, Ticket => 1)), Submitted, Ring);
         Check (Submitted and not Ring, "busy retirement wait owes no wake");
         RC.Submit (C, RC.Token (3 * Frame), Wire.Encode
           ((Kind => Wire.Receive_Input, Surface => 2, After => Unsigned_64 (Frame))), Submitted, Ring);
         Check (Submitted and not Ring, "busy input request owes no wake");
         for N in 1 .. 3 loop
            RC.Take (S, Item, Got, Ring);
            Check (Got and not Ring, "server takes without a wake owed");
            declare
               D : constant Wire.Request_Decoding := Wire.Decode (Item.Item);
               R : Wire.Result;
            begin
               Check (D.Valid, "server decodes the copy");
               if D.Valid then
                  R := (case D.Value.Kind is
                          when Wire.Publish => (Kind => Wire.Publish, Status => DP.Success, Epoch => 1,
                                                Ticket => Unsigned_64 (Frame)),
                          when Wire.Await_Retirement =>
                            (Kind => Wire.Await_Retirement, Status => DP.Success, Epoch => 1, Ticket => 1),
                          when Wire.Receive_Input =>
                            (Kind => Wire.Receive_Input, Status => DP.Success, Count => 1, More => False,
                             Events => [1 => Key (D.Value.After + 1), others => (others => <>)]),
                          when Wire.Present => (Kind => Wire.Present, Status => DP.Success));
                  RC.Complete (S, Item.Tag, Wire.Encode (R), Ring);
                  Check (not Ring, "busy answer owes no wake");
               end if;
            end;
         end loop;
         for N in 1 .. 3 loop
            RC.Reap (C, Answer, Got);
            Check (Got, "client reaps every answer");
            declare
               Kind : constant Wire.Request_Kind :=
                 (case (Answer.Tag - 1) mod 3 is when 0 => Wire.Publish, when 1 => Wire.Await_Retirement,
                     when others => Wire.Receive_Input);
               D : constant Wire.Result_Decoding := Wire.Decode (Answer.Answer, Kind);
            begin
               Check (D.Valid and then D.Value.Status = DP.Success, "answer decodes against its request");
               if D.Valid and then Kind = Wire.Receive_Input then
                  Check (D.Value.Events (1).Serial = Unsigned_64 (Frame) + 1, "input after the acknowledged serial");
               end if;
            end;
         end loop;
      end loop;
      --  Idle: Desktop arms and sleeps. The first request owes it one wake;
      --  the rest of the burst owes none until it has woken and armed again.
      Check (RC.Arm_For_Requests (S), "server arms with nothing waiting");
      for N in 1 .. 4 loop
         RC.Submit (C, RC.Token (1_000 + N), Wire.Encode
           ((Kind => Wire.Present, Surface => 2, Area => (0, 0, 0, 0), Input_After => 0)), Submitted, Ring);
         Check (Submitted, "idle present queued");
         if Ring then Wakes_Owed := Wakes_Owed + 1; end if;
      end loop;
      Check (Wakes_Owed = 1, "one wake per arming for a burst of requests");
      RC.Disarm_For_Requests (S);
      --  The window sleeps; Desktop answers all four: one wake owed.
      Check (RC.Arm_For_Completions (C), "client arms with nothing waiting");
      Wakes_Owed := 0;
      for N in 1 .. 4 loop
         RC.Take (S, Item, Got, Ring);
         RC.Complete (S, Item.Tag, Wire.Encode ((Kind => Wire.Present, Status => DP.Success)), Ring);
         if Ring then Wakes_Owed := Wakes_Owed + 1; end if;
      end loop;
      Check (Wakes_Owed = 1, "one wake per arming for a burst of answers");
      RC.Disarm_For_Completions (C);
      for N in 1 .. 4 loop
         RC.Reap (C, Answer, Got);
         Check (Got and then Wire.Decode (Answer.Answer, Wire.Present).Valid, "idle answers reaped");
      end loop;
      --  An armed client with an answer already waiting does not sleep.
      RC.Submit (C, 2_000, Wire.Encode ((Kind => Wire.Receive_Input, Surface => 2, After => 0)), Submitted, Ring);
      RC.Take (S, Item, Got, Ring);
      RC.Complete (S, Item.Tag, Wire.Encode ((Kind => Wire.Receive_Input, Status => DP.Bad_Object, others => <>)),
                   Ring);
      Check (not RC.Arm_For_Completions (C), "arming sees the waiting answer");
   end Channel;
begin
   Request_Round_Trips;
   Request_Rejections;
   Result_Codec;
   Request_Sweep;
   Channel;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("SURFACE-RING: PASS checks=" & Checks'Image);
   else
      Ada.Text_IO.Put_Line ("SURFACE-RING: FAIL" & Failures'Image & " of" & Checks'Image);
   end if;
end Main;
