------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Host tests for CuBit.Channel_Rings: a byte stream through a producer and
--  consumer pair arrives in order and intact, across 32-bit index wrap, with
--  random write and read sizes; hostile peer indices are refused and leave
--  the ring unchanged. Contracts are checked at run time (-gnata).
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Channel_Rings; use CuBit.Channel_Rings;
with CuBit.Datagram_Rings; use CuBit.Datagram_Rings;
with Slot_Ring_Small;
with Slot_Ring_Frames;
with CuBit.Frame_Rings;
with Queue_Small;
with CuBit.Stream_Rings;
with CuBit.Stream_Regions;
with System.Storage_Elements;
with CuBit.Net_Control_Queues;
with CuBit.Net_Channel_Layout;

procedure Main is
   Failures : Natural := 0;
   Seed     : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;

   function Random return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Random;

   function Below (N : Positive) return Natural is (Natural (Random mod Unsigned_64 (N)));

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   --  The stream's byte at a given stream offset.
   function Pattern (Offset : Unsigned_64) return Unsigned_8 is
     (Unsigned_8 ((Offset * 131 + Offset / 251) mod 256));

   --  Push Total bytes through a ring of Size whose indices start at Origin,
   --  alternating copying and zero-copy (slice) operations.
   procedure Stream (Size : Ring_Size; Origin : Index; Total : Unsigned_64) is
      Ring     : Bytes (0 .. Size - 1) := [others => 16#EE#];
      P        : Producer := New_Producer (Size, Origin);
      C        : Consumer := New_Consumer (Size, Origin);
      Sent, Got : Unsigned_64 := 0;
      OK       : Boolean;
      Intact   : Boolean := True;
   begin
      while Got < Total loop
         --  Produce.
         declare
            Want : constant Natural :=
              Natural (Unsigned_64'Min (Unsigned_64 (Below (Size + Size / 2)), Total - Sent));
         begin
            if Random mod 2 = 0 then
               declare
                  Data : Bytes (1_000 .. 999 + Want);
                  Written : Natural;
               begin
                  for I in Data'Range loop
                     Data (I) := Pattern (Sent + Unsigned_64 (I - Data'First));
                  end loop;
                  Write (P, Ring, Data, Written);
                  Sent := Sent + Unsigned_64 (Written);
               end;
            else
               declare
                  First, L1, L2, N : Natural;
               begin
                  Free_Slices (P, First, L1, L2);
                  N := Natural'Min (Want, L1 + L2);
                  for K in 0 .. N - 1 loop
                     Ring (if K < L1 then First + K else K - L1) := Pattern (Sent + Unsigned_64 (K));
                  end loop;
                  Commit (P, N);
                  Sent := Sent + Unsigned_64 (N);
               end;
            end if;
         end;
         Accept_Produced (C, P.Produced, OK);
         Check (OK, "honest produced index accepted");

         --  Consume.
         declare
            Want : constant Natural := Below (Size + Size / 2);
         begin
            if Random mod 2 = 0 then
               declare
                  Data  : Bytes (7 .. 6 + Want) := [others => 0];
                  Count : Natural;
               begin
                  Read (C, Ring, Data, Count);
                  for I in 0 .. Count - 1 loop
                     Intact := Intact and then Data (Data'First + I) = Pattern (Got + Unsigned_64 (I));
                  end loop;
                  Got := Got + Unsigned_64 (Count);
               end;
            else
               declare
                  First, L1, L2, N : Natural;
               begin
                  Data_Slices (C, First, L1, L2);
                  N := Natural'Min (Want, L1 + L2);
                  for K in 0 .. N - 1 loop
                     Intact := Intact and then
                       Ring (if K < L1 then First + K else K - L1) = Pattern (Got + Unsigned_64 (K));
                  end loop;
                  Consume (C, N);
                  Got := Got + Unsigned_64 (N);
               end;
            end if;
         end;
         Accept_Consumed (P, C.Consumed, OK);
         Check (OK, "honest consumed index accepted");
      end loop;
      Check (Intact, "stream intact, size" & Size'Image & " origin" & Origin'Image);
      Check (Sent = Total and then Got = Total, "stream complete");
   end Stream;

   --  A hostile peer writes random indices; each is accepted exactly when it
   --  follows the rules, and a refusal changes nothing.
   procedure Hostile (Size : Ring_Size) is
      P  : Producer := New_Producer (Size, Index'Last - 10);
      C  : Consumer := New_Consumer (Size, Index'Last - 10);
      OK : Boolean;
      Accepted, Refused : Natural := 0;
   begin
      for Round in 1 .. 200_000 loop
         Commit (P, Below (Space (P) + 1));
         declare
            Before : constant Producer := P;
            Value  : constant Index :=
              (case Random mod 4 is
                 when 0 => Index (Random mod 2 ** 32),
                 when 1 => Consumed (P) + Index (Below (P.Fill + 1)),
                 when 2 => P.Produced + Index (Below (Size) + 1),
                 when others => Consumed (P) - Index (Below (Size) + 1));
            Legal  : constant Boolean :=
              Value - Consumed (Before) <= Before.Produced - Consumed (Before);
         begin
            Accept_Consumed (P, Value, OK);
            Check (OK = Legal, "consumed index judged");
            Check ((if OK then Consumed (P) = Value and then P.Produced = Before.Produced
                    else P = Before), "consumed effect");
            Check (P.Fill <= Size, "fill bounded");
         end;
         declare
            Before : constant Consumer := C;
            Value  : constant Index :=
              (case Random mod 4 is
                 when 0 => Index (Random mod 2 ** 32),
                 when 1 => Produced (C) + Index (Below (Size - C.Available + 1)),
                 when 2 => C.Consumed + Index (Below (Size) + Size + 1),
                 when others => Produced (C) - Index (Below (Size) + 1));
            Legal  : constant Boolean :=
              Value - Before.Consumed <= Index (Size) and then
              Value - Before.Consumed >= Produced (Before) - Before.Consumed;
         begin
            Accept_Produced (C, Value, OK);
            Check (OK = Legal, "produced index judged");
            Check ((if OK then Produced (C) = Value and then C.Consumed = Before.Consumed
                    else C = Before), "produced effect");
            if OK then Accepted := Accepted + 1; else Refused := Refused + 1; end if;
            Consume (C, Below (C.Available + 1));
         end;
      end loop;
      Check (Accepted > 10_000 and then Refused > 10_000, "hostile indices exercised both ways");
   end Hostile;

   --  Datagrams of random sizes through a ring (wrapping, padding), each
   --  read back intact and in order, some into short buffers (truncated).
   procedure Datagrams (Size : Ring_Size; Origin : Index) is
      Ring : Bytes (0 .. Size - 1) := [others => 16#EE#];
      P : Producer := New_Producer (Size, Origin);
      C : Consumer := New_Consumer (Size, Origin);
      Sent, Got : Natural := 0;
      Intact : Boolean := True;
      Lengths : array (0 .. 1023) of Natural := [others => 0];
      PR : Put_Result;
      TR : Take_Result;
      OK : Boolean;
   begin
      for Round in 1 .. 60_000 loop
         if Random mod 2 = 0 and then Sent - Got < Lengths'Length then
            declare
               Len : constant Natural := Below (Natural'Min (Size / 3, 2_000));
               Data : Bytes (1 .. Len);
            begin
               for I in Data'Range loop
                  Data (I) := Pattern (Unsigned_64 (Sent) * 7 + Unsigned_64 (I));
               end loop;
               Put (P, Ring, Data, PR);
               if PR = Put then
                  Lengths (Sent mod Lengths'Length) := Len;
                  Sent := Sent + 1;
               end if;
               Check (PR /= Too_Large, "datagram fits");
            end;
         else
            Accept_Produced (C, P.Produced, OK);
            declare
               Room : constant Natural := Below (2_100);
               Into : Bytes (5 .. 4 + Room) := [others => 0];
               Len : Natural;
               Trunc : Boolean;
            begin
               Take (C, Ring, Into, Len, Trunc, TR);
               Check (TR /= Malformed, "honest records are well formed");
               if TR = Taken then
                  declare
                     Want : constant Natural := Lengths (Got mod Lengths'Length);
                  begin
                     Intact := Intact and then Len = Natural'Min (Want, Room) and then
                       Trunc = (Want > Room);
                     for I in 1 .. Len loop
                        Intact := Intact and then
                          Into (4 + I) = Pattern (Unsigned_64 (Got) * 7 + Unsigned_64 (I));
                     end loop;
                  end;
                  Got := Got + 1;
               end if;
            end;
            Accept_Consumed (P, C.Consumed, OK);
         end if;
      end loop;
      Check (Intact and then Got > 1_000, "datagrams intact, in order, truncation reported");
   end Datagrams;

   --  A hostile producer's headers: never a read outside the ring or the
   --  buffer (contracts are checked), and bad headers are reported.
   procedure Hostile_Datagrams is
      Ring : Bytes (0 .. 4_095);
      C : Consumer;
      OK : Boolean;
      Into : Bytes (1 .. 100);
      Len : Natural;
      Trunc : Boolean;
      TR : Take_Result;
      Malformed_Seen : Natural := 0;
   begin
      for Round in 1 .. 50_000 loop
         for I in Ring'Range loop
            Ring (I) := Unsigned_8 (Random mod 256);
         end loop;
         C := New_Consumer (4_096, Index (Random mod 2 ** 32));
         Accept_Produced (C, C.Consumed + Index (Below (4_097)), OK);
         Take (C, Ring, Into, Len, Trunc, TR);
         if TR = Malformed then
            Malformed_Seen := Malformed_Seen + 1;
         end if;
      end loop;
      Check (Malformed_Seen > 10_000, "hostile datagram headers reported");
   end Hostile_Datagrams;

   --  A 4-slot ring: a value sequence arrives in order across 32-bit wrap,
   --  with random batch sizes, and hostile peer indices are refused
   --  exactly as an independent formulation says, leaving the side as it
   --  was. Every in-flight element's slot differs from the next one pushed.
   procedure Slots_Small (Origin : Index; Steps : Positive) is
      package S renames Slot_Ring_Small;
      use type S.Producer, S.Consumer;
      R    : S.Ring := [others => 0];
      P    : S.Producer := S.New_Producer (Origin);
      C    : S.Consumer := S.New_Consumer (Origin);
      Next_Out, Next_In : Unsigned_32 := 0;
      OK   : Boolean;
      V    : Unsigned_32;
   begin
      for Step in 1 .. Steps loop
         for K in 1 .. Below (S.Slots + 1) loop
            exit when S.Space (P) = 0;
            --  The slot about to be written holds nothing in flight.
            for Back in 1 .. P.Fill loop
               Check (S.Slot_Of (P.Produced - Index (Back)) /= S.Next_Slot (P),
                      "slot ring: in-flight slot reused");
            end loop;
            S.Push (P, R, Next_Out);
            Next_Out := Next_Out + 1;
         end loop;
         S.Accept_Produced (C, P.Produced, OK);
         Check (OK, "slot ring: honest produced index accepted");
         for K in 1 .. Below (S.Slots + 1) loop
            exit when C.Available = 0;
            S.Take (C, R, V);
            Check (V = Next_In, "slot ring: values in order");
            Next_In := Next_In + 1;
         end loop;
         S.Accept_Consumed (P, C.Consumed, OK);
         Check (OK, "slot ring: honest consumed index accepted");
         --  Hostile indices, judged independently.
         declare
            Bad : constant Index := Index (Random mod 2 ** 32);
            P0  : constant S.Producer := P;
            C0  : constant S.Consumer := C;
            Freed : constant Unsigned_32 := Unsigned_32 (Bad - (P.Produced - Index (P.Fill)));
            Ahead : constant Unsigned_32 := Unsigned_32 (Bad - C.Consumed);
         begin
            S.Accept_Consumed (P, Bad, OK);
            Check (OK = (Freed <= Unsigned_32 (P0.Fill)), "slot ring: consumed index judged");
            if not OK then
               Check (P = P0, "slot ring: rejected consumed index changes nothing");
            end if;
            P := P0;
            S.Accept_Produced (C, Bad, OK);
            Check (OK = (Ahead <= Unsigned_32 (S.Slots) and then Ahead >= Unsigned_32 (C0.Available)),
                   "slot ring: produced index judged");
            if not OK then
               Check (C = C0, "slot ring: rejected produced index changes nothing");
            end if;
            C := C0;
         end;
      end loop;
      Check (Next_In > Unsigned_32 (Steps / 2), "slot ring: values flowed");
   end Slots_Small;

   --  Indices less than a ring apart never share a slot, for the frame
   --  ring's 128 slots, around the 2 ** 32 wrap.
   procedure Slots_Distinct is
      package F renames Slot_Ring_Frames;
   begin
      for Base in Index'Last - 300 .. Index'Last loop
         for D in 1 .. F.Slots - 1 loop
            if F.Slot_Of (Base) = F.Slot_Of (Base + Index (D)) then
               Check (False, "frame ring: indices a ring apart share a slot");
            end if;
         end loop;
      end loop;
      Check (F.Slots = 128 and then F.Slot_Of (Index'Last) = 127 and then
             F.Slot_Of (0) = 0, "frame ring: slots across the wrap");
      --  The packet grant: header page, then 128 + 128 slots, 129 pages.
      Check (CuBit.Frame_Rings.Grant_Bytes = 129 * 4_096 and then
             CuBit.Frame_Rings.Receive_Slot_At (127) + 2_048 =
               CuBit.Frame_Rings.Transmit_Slots_At and then
             CuBit.Frame_Rings.Fits (14) and then CuBit.Frame_Rings.Fits (2_032) and then
             not CuBit.Frame_Rings.Fits (13) and then not CuBit.Frame_Rings.Fits (2_033),
             "frame rings: grant layout");
   end Slots_Distinct;

   --  A queue pair whose completion ring (2 slots) is smaller than its
   --  submission ring (4): random submit, take, complete and reap steps.
   --  Every answer pairs with its request's token and value, answers come
   --  in the order requests were taken, the service never owes more than
   --  it has room for, and a client that stops reaping stalls only the
   --  service's intake, never an answer.
   procedure Queue_Pair is
      package Q renames Queue_Small;
      use type Q.Token;
      SR : Q.Submissions.Ring := [others => (Tag => 0, Item => 0)];
      CR : Q.Completions.Ring := [others => (Tag => 0, Answer => 0)];
      C  : Q.Client;
      S  : Q.Server;
      Next_Tag, Next_Answer : Q.Token := 1;
      Taken : array (0 .. 3) of Q.Submission;   --  owed, in order
      Owed_First : Natural := 0;
      Sub  : Q.Submission;
      Comp : Q.Completion;
      OK   : Boolean;
      Reaped : Natural := 0;
   begin
      for Step in 1 .. 400_000 loop
         case Below (4) is
            when 0 =>
               Q.Accept_Taken (C, S.Requests.Consumed, OK);
               Check (OK, "queue: honest taken index accepted");
               if Q.Can_Submit (C) then
                  Q.Submit (C, SR, Next_Tag, Unsigned_32 (Next_Tag mod 2 ** 32) * 3);
                  Next_Tag := Next_Tag + 1;
               end if;
            when 1 =>
               Q.Submissions.Accept_Produced (S.Requests, C.Requests.Produced, OK);
               Check (OK, "queue: honest submissions accepted");
               if Q.Can_Take (S) then
                  Q.Take (S, SR, Sub);
                  Taken ((Owed_First + S.Owed - 1) mod 4) := Sub;
               end if;
            when 2 =>
               if S.Owed > 0 then
                  declare
                     R : constant Q.Submission := Taken (Owed_First);
                  begin
                     Q.Complete (S, CR, R.Tag, R.Item + 1);
                     Owed_First := (Owed_First + 1) mod 4;
                  end;
               end if;
            when others =>
               Q.Completions.Accept_Produced (C.Answers, S.Answers.Produced, OK);
               Check (OK, "queue: honest answers accepted");
               if C.Answers.Available > 0 then
                  Q.Reap (C, CR, Comp, OK);
                  Check (OK, "queue: every answer had a request");
                  Check (Comp.Tag = Next_Answer and then
                         Comp.Answer = Unsigned_32 (Comp.Tag mod 2 ** 32) * 3 + 1,
                         "queue: answer pairs with its request");
                  Next_Answer := Next_Answer + 1;
                  Reaped := Reaped + 1;
               end if;
               Q.Accept_Reaped (S, C.Answers.Consumed, OK);
               Check (OK, "queue: honest reaped index accepted");
         end case;
         Check (Q.Valid (S) and then S.Owed <= Q.Completion_Slots and then
                C.Pending <= Q.Completion_Slots, "queue: bounds");
      end loop;
      Check (Reaped > 20_000, "queue: answers flowed");   --  about 40,000
   end Queue_Pair;

   --  The control queue's entries lie as C sees them (cubit_net_channel.h).
   procedure Control_Layout is
      package NC renames CuBit.Net_Control_Queues;
      package L renames CuBit.Net_Channel_Layout;
      S : constant NC.Queues.Submission := (Tag => 0, Item => <>);
      A : constant NC.Queues.Completion := (Tag => 0, Answer => <>);
   begin
      Check (S.Tag'Position = L.Request_Token_At and then
             S.Item'Position = L.Request_Operation_At and then
             S.Item'Position + S.Item.Length'Position = L.Request_Length_At and then
             S.Item'Position + S.Item.Object'Position = L.Request_Object_At and then
             S.Item'Position + S.Item.Buffer'Position = L.Request_Buffer_At and then
             NC.Queues.Submission'Size = L.Queue_Entry_Bytes * 8,
             "control queue: request layout");
      Check (A.Tag'Position = L.Answer_Token_At and then
             A.Answer'Position = L.Answer_Status_At and then
             A.Answer'Position + A.Answer.Value'Position = L.Answer_Value_At and then
             NC.Queues.Completion'Size = L.Queue_Entry_Bytes * 8,
             "control queue: answer layout");
   end Control_Layout;

   --  CuBit.Stream_Rings: one producer, a reader that keeps up and one that
   --  falls behind. Payloads carry a sequence number (4 bytes) and a length
   --  that varies, so records wrap with pads.
   procedure Broadcast_Streams (Size : Ring_Size; Origin : Index; Total : Natural) is
      package SR renames CuBit.Stream_Rings;
      use type SR.Publish_Result;
      use type SR.Read_Result;
      Ring : Bytes (0 .. Size - 1) := [others => 0];
      P : Producer := (Size => Size, Produced => Origin, Fill => 0);
      Fast, Slow : Index := Origin;
      Expected_Fast : Unsigned_32 := 0;
      Slow_Last : Unsigned_32 := 0;
      Slow_Lost : Natural := 0;
      Into : Bytes (0 .. 255);
      Length : Natural;
      Truncated, Lost : Boolean;
      Evicted : Natural;
      Put : SR.Publish_Result;
      Got : SR.Read_Result;
      function Sequence_Of (B : Bytes) return Unsigned_32 is
        (Unsigned_32 (B (B'First)) or Shift_Left (Unsigned_32 (B (B'First + 1)), 8)
         or Shift_Left (Unsigned_32 (B (B'First + 2)), 16)
         or Shift_Left (Unsigned_32 (B (B'First + 3)), 24));
   begin
      for N in 0 .. Total - 1 loop
         declare
            Payload_Length : constant Natural := 4 + (N * 37) mod 200;
            Data : Bytes (0 .. Payload_Length - 1);
            Seq : constant Unsigned_32 := Unsigned_32 (N);
         begin
            Data (0) := Unsigned_8 (Seq and 255);
            Data (1) := Unsigned_8 (Shift_Right (Seq, 8) and 255);
            Data (2) := Unsigned_8 (Shift_Right (Seq, 16) and 255);
            Data (3) := Unsigned_8 (Shift_Right (Seq, 24) and 255);
            for K in 4 .. Data'Last loop
               Data (K) := Unsigned_8 ((N + K) mod 251);
            end loop;
            SR.Publish (P, Ring, Data, Evicted, Put);
            Check (Put = SR.Published, "stream publish");
         end;
         --  The fast reader reads everything as it comes: no loss, in order.
         loop
            SR.Read (Fast, Size, Ring, P.Produced, Consumed (P), Into, Length, Truncated, Lost, Got);
            exit when Got /= SR.Taken;
            Check (not Lost and then not Truncated and then Length >= 4, "fast reader keeps up");
            Check (Sequence_Of (Into (0 .. 3)) = Expected_Fast, "fast reader in order");
            for K in 4 .. Length - 1 loop
               Check (Into (K) = Unsigned_8 ((Natural (Expected_Fast) + K) mod 251), "payload intact");
            end loop;
            Expected_Fast := Expected_Fast + 1;
         end loop;
         Check (Got = SR.Empty, "fast reader drained");
         --  The slow reader looks every 97 records: it loses the oldest,
         --  then reads the newest in order.
         if N mod 97 = 96 then
            declare
               First_Read : Boolean := True;
            begin
               loop
                  declare
                     Start : constant Index := Slow;
                  begin
                     SR.Read (Slow, Size, Ring, P.Produced, Consumed (P), Into, Length, Truncated, Lost, Got);
                     exit when Got /= SR.Taken;
                     Check (SR.Intact ((if Lost then Consumed (P) else Start), P.Produced, Consumed (P)),
                            "nothing overwrote the record meanwhile");
                     if Lost then
                        Slow_Lost := Slow_Lost + 1;
                     elsif not First_Read then
                        Check (Sequence_Of (Into (0 .. 3)) = Slow_Last + 1, "slow reader in order after a gap");
                     end if;
                     Slow_Last := Sequence_Of (Into (0 .. 3));
                     First_Read := False;
                  end;
               end loop;
               Check (Slow_Last = Unsigned_32 (N), "slow reader reaches the newest");
            end;
         end if;
      end loop;
      Check (Natural (Expected_Fast) = Total, "fast reader read every record");
      Check (Size > 16_384 or else Slow_Lost > 0, "a small ring made the slow reader lose records");
      --  A record over half the ring is refused, and nothing changes.
      declare
         Big : constant Bytes (0 .. Size / 2) := [others => 7];
         Before : constant Producer := P;
      begin
         SR.Publish (P, Ring, Big, Evicted, Put);
         Check (Put = SR.Too_Large and then P = Before and then Evicted = 0, "too large refused");
      end;
      --  A copy the producer overwrote is not Intact.
      declare
         Start : constant Index := Fast;
         Data : constant Bytes (0 .. 99) := [others => 1];
      begin
         for N in 1 .. Size / 64 loop
            SR.Publish (P, Ring, Data, Evicted, Put);
         end loop;
         Check (not SR.Intact (Start, P.Produced, Consumed (P)), "an overwritten record is not intact");
      end;
      --  A corrupt header at OLDEST: the producer empties the ring and goes on.
      declare
         At_Oldest : constant Natural := Position (Consumed (P), Size);
         Data : constant Bytes (0 .. 9) := [others => 3];
      begin
         if P.Fill > 0 then
            Ring (At_Oldest + 2) := 99;     --  no such record kind
         end if;
         for N in 1 .. Size / 8 loop
            SR.Publish (P, Ring, Data, Evicted, Put);
            Check (Put = SR.Published and then Valid (P), "producer survives a corrupt header");
         end loop;
      end;
   end Broadcast_Streams;

   --  The adapters' order (CuBit.Stream_Regions): Make_Room, publish OLDEST,
   --  then Put. Put must then succeed, and change no byte of the records
   --  OLDEST still covers (a reader's copy of them stays whole). Records of
   --  Largest_Payload go in wherever the ring stands.
   procedure Room_Before_Put (Size : Ring_Size; Origin : Index; Total : Natural) is
      package SR renames CuBit.Stream_Rings;
      package DR renames CuBit.Datagram_Rings;
      use type SR.Publish_Result;
      use type DR.Put_Result;
      Ring : Bytes (0 .. Size - 1) := [others => 0];
      P : Producer := (Size => Size, Produced => Origin, Fill => 0);
      Largest : constant Natural := SR.Largest_Payload (Size);
      Evicted : Natural;
      Room : SR.Publish_Result;
      Put : DR.Put_Result;
   begin
      Check (SR.Fits (Size, Largest) and then not SR.Fits (Size, Largest + 1),
             "largest payload is the largest that fits");
      for N in 0 .. Total - 1 loop
         declare
            Length : constant Natural :=
              (if N mod 11 = 0 then Largest else (N * 53) mod (Largest + 1));
            Data : constant Bytes (0 .. Length - 1) := [others => Unsigned_8 (N mod 256)];
            Before : constant Bytes := Ring;
         begin
            SR.Make_Room (P, Ring, Length, Evicted, Room);
            Check (Room = SR.Published, "room made");
            declare
               Kept_From : constant Index := Consumed (P);
               Kept : constant Natural := P.Fill;
            begin
               DR.Put (P, Ring, Data, Put);
               Check (Put = DR.Put, "put after making room");
               for K in 0 .. Kept - 1 loop
                  declare
                     At_Byte : constant Natural := Position (Kept_From + Index (K), Size);
                  begin
                     Check (Ring (At_Byte) = Before (At_Byte), "kept records untouched");
                  end;
               end loop;
            end;
         end;
      end loop;
   end Room_Before_Put;

   --  CuBit.Stream_Regions over a real region, as a launcher lends one:
   --  initialized by the owner, adopted by a writer (Writer_Of), read in
   --  place by the owner (Read_Owned), across many wraps.
   procedure Region_Round_Trip (Pages : Positive; Total : Natural) is
      package Regions renames CuBit.Stream_Regions;
      type Page_Array is array (1 .. Natural (Regions.Region_Bytes (Pages))) of Unsigned_8
        with Alignment => 4_096;
      Region : Page_Array := [others => 16#AA#];
      Base : constant Unsigned_64 :=
        Unsigned_64 (System.Storage_Elements.To_Integer (Region'Address));
      Writer : Producer;
      Into : Bytes (0 .. 4_095);
      Length : Natural;
      Expected : Natural := 0;
   begin
      Regions.Initialize (Base, 1);
      Check (Regions.Element (Base) = 1 and then Regions.Produced (Base) = 0, "region initialized");
      Writer := Regions.Writer_Of (Base, Pages);
      Check (Writer.Fill = 0 and then Writer.Size = CuBit.Stream_Rings.Ring_Bytes (Pages),
             "writer adopts an empty region");
      Check (Regions.Read_Owned (Base, Pages, Into'Address, Into'Length) = 0, "empty region reads nothing");
      for N in 0 .. Total - 1 loop
         declare
            Data : Bytes (0 .. 1 + N mod 300) := [others => Unsigned_8 (N mod 256)];
         begin
            Check (Regions.Write (Base, Writer, Data'Address, Data'Length), "region write");
            --  Writer re-adopted from the words, as a new process would.
            if N mod 7 = 0 then
               Writer := Regions.Writer_Of (Base, Pages);
            end if;
         end;
         if N mod 3 = 2 then
            loop
               Length := Regions.Read_Owned (Base, Pages, Into'Address, Into'Length);
               exit when Length = 0;
               while Expected <= N and then 2 + Expected mod 300 /= Length loop
                  Expected := Expected + 1;      --  dropped as oldest
               end loop;
               Check (Expected <= N and then Into (0) = Unsigned_8 (Expected mod 256),
                      "owner reads records in order");
               Expected := Expected + 1;
            end loop;
            Check (Expected = N + 1, "owner caught up");
         end if;
      end loop;
   end Region_Round_Trip;

begin
   Control_Layout;
   Queue_Pair;
   Slots_Small (0, 200_000);
   Slots_Small (Index'Last - 5, 200_000);
   Slots_Distinct;
   Datagrams (4_096, 0);
   Datagrams (4_096, Index'Last - 703);   --  aligned: 2 ** 32 - 704
   Datagrams (65_536, Index'Last - 29_999);   --  aligned: 2 ** 32 - 30_000
   Hostile_Datagrams;
   Broadcast_Streams (4_096, 0, 20_000);
   Broadcast_Streams (4_096, Index'Last - 1_023, 20_000);
   Broadcast_Streams (65_536, Index'Last - 40_959, 50_000);
   Room_Before_Put (4_096, 0, 5_000);
   Region_Round_Trip (1, 3_000);
   Region_Round_Trip (4, 3_000);
   Room_Before_Put (4_096, Index'Last - 1_023, 5_000);
   Room_Before_Put (65_536, Index'Last - 40_959, 5_000);
   Check (Valid_Size (4_096) and then Valid_Size (1_048_576), "sizes");
   Check (not Valid_Size (0) and then not Valid_Size (6_000) and then
          not Valid_Size (2_097_152) and then not Valid_Size (2_048), "bad sizes");
   for Order in 0 .. 2 loop
      declare
         Size : constant Ring_Size := 4_096 * 2 ** (Order * 2);
      begin
      Stream (Size, 0, 3_000_000);
      Stream (Size, Index'Last - Index (Size / 3), 3_000_000);   --  wraps 2 ** 32
      Stream (Size, Index'Last, 500_000);
      Hostile (Size);
      end;
   end loop;
   if Failures = 0 then
      Put_Line ("channel-rings: PASS");
   else
      Put_Line ("channel-rings: FAIL (" & Failures'Image & " )");
   end if;
end Main;
