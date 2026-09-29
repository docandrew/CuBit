--  Executable checks for userspace/net/src (proofs: gnatprove on this
--  project; see README.md).
with Ada.Text_IO;    use Ada.Text_IO;
with Interfaces;     use Interfaces;
with TCP_Sequence;   use TCP_Sequence;
with TCP_Acceptance; use TCP_Acceptance;
with TCP_RTO;
with Send_Queue_64;
with TCP_Connection;
with Receive_Queue_64;
with TCP_Congestion;
with TCP_Options;
with TCP_Timestamps;
with Scoreboard_8;
with Scoreboard_Full; pragma Unreferenced (Scoreboard_Full);
with SipHash;
with TCP_Isn;
with TCP_Syn_Cookies;
with Pool_Small;
with Pool_Full; pragma Unreferenced (Pool_Full);
with Table_Small;
with Table_Full; pragma Unreferenced (Table_Full);
with Table_Netstack; pragma Unreferenced (Table_Netstack);
with Timers_Small;
with Timers_Full; pragma Unreferenced (Timers_Full);
with Send_Chunked_Small;
with Send_Chunked_Full; pragma Unreferenced (Send_Chunked_Full);
with Endpoint_Small; pragma Unreferenced (Endpoint_Small);
with Flow_Small; pragma Unreferenced (Flow_Small);
with TCP_Header;
with IPv4_Header;
with ICMPv4_Error;
with TCP_Reset;
with ARP_Cache;
with ARP_Packet;
with UDP_Header; pragma Unreferenced (UDP_Header);
with DNS_Name;
with Internet_Checksum;
with DNS_Response;
with Descriptors_80;
with Channel_Arenas;
with Virtqueue_Index;
with Channel_Service;
with IPv6_Header;
with ND_Message;
with Neighbor_Cache;
with RA_Message;
with SLAAC_Table;
with Receive_Queue_256k; pragma Unreferenced (Receive_Queue_256k);
--  Proved at a realistic size too (gnatprove analyzes what main reaches).
with Send_Queue_256k; pragma Unreferenced (Send_Queue_256k);

procedure Main is
   Failures : Natural := 0;

   procedure Check (OK : Boolean; Name : String) is
   begin
      if not OK then
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   Kept_First : Seq;
   Kept_Skip, Kept_Count : Segment_Length;
begin
   --  Sequence order, including wraparound.
   Check (Lt (1, 2) and then not Lt (2, 1), "order");
   Check (Lt (Seq'Last, 0) and then Lt (Seq'Last - 5, 3), "order across wrap");
   Check (not Lt (0, Half) and then not Lt (Half, 0), "half-space is unordered");
   Check (Le (7, 7) and then not Lt (7, 7), "reflexive Le, irreflexive Lt");
   Check (In_Window (Seq'Last, Seq'Last - 1, 4) and then In_Window (1, Seq'Last - 1, 4)
          and then not In_Window (2, Seq'Last - 1, 4), "window across wrap");

   --  RFC 9293 3.10.7.4, the four cases.
   Check (Acceptable (100, 0, 100, 0) and then not Acceptable (101, 0, 100, 0),
          "empty segment, zero window");
   Check (Acceptable (150, 0, 100, 100) and then not Acceptable (200, 0, 100, 100),
          "empty segment, open window");
   Check (not Acceptable (100, 10, 100, 0), "data into a zero window");
   Check (Acceptable (95, 10, 100, 100) and then Acceptable (195, 10, 100, 100)
          and then not Acceptable (80, 10, 100, 100) and then not Acceptable (200, 10, 100, 100),
          "data overlapping either window edge");

   --  Trimming.
   Trim (95, 10, 100, 100, Kept_First, Kept_Skip, Kept_Count);
   Check (Kept_First = 100 and then Kept_Skip = 5 and then Kept_Count = 5, "drop the old prefix");
   Trim (195, 10, 100, 100, Kept_First, Kept_Skip, Kept_Count);
   Check (Kept_First = 195 and then Kept_Skip = 0 and then Kept_Count = 5, "cut at the right edge");
   Trim (Seq'Last - 2, 10, 2, 100, Kept_First, Kept_Skip, Kept_Count);
   Check (Kept_First = 2 and then Kept_Skip = 5 and then Kept_Count = 5, "trim across wrap");

   --  RFC 6298.
   declare
      E : TCP_RTO.Estimator;
   begin
      Check (E.RTO = 1_000, "initial RTO");
      TCP_RTO.Update (E, 100);   --  SRTT 100, RTTVAR 50, RTO 300
      Check (E.SRTT = 100 and then E.RTTVAR = 50 and then E.RTO = 300, "first sample");
      TCP_RTO.Update (E, 100);   --  RTTVAR 37, SRTT 100, RTO 248
      Check (E.SRTT = 100 and then E.RTTVAR = 37 and then E.RTO = 248, "second sample");
      TCP_RTO.Update (E, 1);     --  small RTT: clamped at the minimum
      for I in 1 .. 20 loop TCP_RTO.Update (E, 1); end loop;
      Check (E.RTO = TCP_RTO.Minimum_RTO, "minimum RTO");
      for I in 1 .. 20 loop TCP_RTO.Back_Off (E); end loop;
      Check (E.RTO = TCP_RTO.Maximum_RTO, "backoff saturates");
   end;

   --  The send queue: ring wraparound, partial pushes, retransmission.
   declare
      use Send_Queue_64;
      Q : Queue;
      Data : Byte_Array (1 .. 64);
      Got : Natural;
      At_Seq : Seq;
      Result : Ack_Result;
      Freed : Natural;
      Input : Byte_Array (1 .. 100);
   begin
      for I in Input'Range loop Input (I) := Unsigned_8 (I mod 256); end loop;
      Initialize (Q, Seq'Last - 10);                  --  wraps the sequence space
      Push (Q, Input, Got);
      Check (Got = 64 and then Send_Queue_64.Count (Q) = 64 and then Free (Q) = 0, "push fills to capacity");
      Take (Q, 30, Data, At_Seq, Got);
      Check (Got = 30 and then At_Seq = Seq'Last - 10 and then Data (1) = 1
             and then Data (30) = 30 and then Sent (Q) = 30, "take the first segment");
      Acknowledge (Q, Seq'Last - 10 + 20, Result, Freed);   --  across the wrap
      Check (Result = Advanced and then Freed = 20 and then Send_Queue_64.Count (Q) = 44
             and then Sent (Q) = 10 and then Element (Q, 0) = 21, "partial ack across wrap");
      Acknowledge (Q, Una (Q), Result, Freed);
      Check (Result = Duplicate and then Freed = 0, "duplicate ack");
      Acknowledge (Q, Nxt (Q) + 1, Result, Freed);
      Check (Result = Unsent_Data and then Send_Queue_64.Count (Q) = 44, "ack of unsent data refused");
      Acknowledge (Q, Una (Q) - 5, Result, Freed);
      Check (Result = Old and then Freed = 0, "old ack ignored");
      Push (Q, Input (65 .. 100), Got);                --  wraps the ring
      Check (Got = 20 and then Send_Queue_64.Count (Q) = 64 and then Element (Q, 63) = 84,
             "push wraps the ring");
      Rewind (Q);
      Take (Q, 64, Data, At_Seq, Got);
      Check (Got = 64 and then At_Seq = Una (Q) and then Data (1) = 21 and then Data (64) = 84,
             "retransmission resends identical bytes");
   end;

   --  Reassembly: out-of-order arrival, gap fill, duplicates, reading.
   declare
      use Receive_Queue_64;
      R : Queue;
      Output : Byte_Array (1 .. 64);
      Got : Natural;
      function Bytes (First, Last : Natural) return Byte_Array is
        ([for I in 1 .. Last - First + 1 => Unsigned_8 (First + I - 1)]);
   begin
      Initialize (R, Seq'Last - 3);                 --  wraps the sequence space
      Insert (R, 10, Bytes (10, 19));               --  out of order
      Check (Ready (R) = 0 and then Present (R, 10) and then not Present (R, 9),
             "out-of-order bytes kept, RCV.NXT unchanged");
      Insert (R, 0, Bytes (0, 9));                  --  fills the gap
      Check (Ready (R) = 20 and then Element (R, 15) = 15, "gap filled: RCV.NXT jumps");
      Insert (R, 30, Bytes (30, 34));               --  out of order ...
      Insert (R, 32, [99, 99]);                     --  ... overlapped: the later text wins
      Check (Element (R, 31) = 31 and then Element (R, 32) = 99 and then Element (R, 34) = 34 and then
             Element (R, 5) = 5 and then Ready (R) = 20,
             "overlapping out-of-order text: last wins; in-order bytes untouched");
      Insert (R, 60, Bytes (60, 69));               --  runs past the window
      Check (Present (R, 63) and then Ready (R) = 20, "bytes past the window dropped");
      Read (R, Output (1 .. 8), Got);
      Check (Got = 8 and then Output (8) = 7 and then Ready (R) = 12
             and then Read_Start (R) = Seq'Last - 3 + 8, "read the front");
      Insert (R, 12, Bytes (20, 51));               --  continues, wrapping the ring
      Check (Ready (R) = 44 and then Element (R, 43) = 51, "ring wraps");
      Read (R, Output, Got);
      Check (Got = 44 and then Output (1) = 8 and then Output (44) = 51, "read the rest");
      --  The early out-of-order bytes (60 .. 63) are now offsets 8 .. 11;
      --  everything past them, including the slots just read, is free.
      Check (Present (R, 8) and then Present (R, 11) and then
             (for all Off in 12 .. 63 => not Present (R, Off)),
             "read frees its slots");
      Insert (R, 56, [1 .. 8 => 200]);              --  lands on freed slots
      Check (Element (R, 56) = 200 and then Element (R, 63) = 200,
             "new data on freed slots is stored, not taken as a duplicate");
   end;

   --  NewReno: slow start, fast retransmit and recovery, timeouts, CA.
   declare
      use TCP_Congestion;
      C : Controller;
      A : Ack_Action;
      D : Duplicate_Action;
   begin
      Initialize (C, 1000, 0);
      Check (C.Cwnd = 10_000, "initial window (RFC 6928)");
      On_Ack (C, 1000, 1001, 9000, A);
      Check (C.Cwnd = 11_000 and then A = Nothing, "slow start: + acknowledged bytes");
      On_Ack (C, 5000, 6001, 6000, A);
      Check (C.Cwnd = 12_000, "slow start: at most one SMSS per ACK");
      On_Duplicate_Ack (C, 6001, 18_001, 12_000, D);
      On_Duplicate_Ack (C, 6001, 18_001, 12_000, D);
      Check (D = Nothing and then C.Cwnd = 12_000, "two duplicates: no action");
      On_Duplicate_Ack (C, 6001, 18_001, 12_000, D);
      Check (D = Fast_Retransmit and then C.Ssthresh = 6000 and then C.Cwnd = 9000
             and then C.In_Recovery, "third duplicate: fast retransmit");
      On_Duplicate_Ack (C, 6001, 18_001, 12_000, D);
      Check (D = Nothing and then C.Cwnd = 10_000, "recovery inflates by SMSS");
      On_Ack (C, 2000, 8001, 10_000, A);
      Check (A = Retransmit_First and then C.Cwnd = 9000 and then C.In_Recovery,
             "partial ACK: deflate, retransmit next hole");
      On_Ack (C, 10_000, 18_001, 0, A);
      Check (A = Nothing and then not C.In_Recovery and then C.Cwnd = 2000,
             "full ACK: leave recovery at min (ssthresh, flight + SMSS)");
      On_Timeout (C, 4000, 30_001, First => True);
      Check (C.Cwnd = 1000 and then C.Ssthresh = 2000, "timeout: loss window");
      On_Timeout (C, 0, 30_001, First => False);
      Check (C.Ssthresh = 2000, "repeated timeout keeps ssthresh");
      for I in 1 .. 3 loop
         On_Duplicate_Ack (C, 20_001, 30_001, 1000, D);
      end loop;
      Check (D = Nothing and then not C.In_Recovery,
             "no fast retransmit for data sent before the timeout");
      On_Ack (C, 1000, 30_001, 0, A);
      Check (C.Cwnd = 2000, "slow start up to ssthresh");
      On_Ack (C, 1000, 31_001, 0, A);
      Check (C.Cwnd = 2000, "congestion avoidance: no growth before a window");
      On_Ack (C, 1000, 32_001, 0, A);
      Check (C.Cwnd = 3000, "congestion avoidance: one SMSS per window");
      Check (Allowance (C, 1000, 1500) = 500 and then Allowance (C, 3000, 90_000) = 0,
             "allowance: peer window and cwnd both bound");
      Restart_After_Idle (C);
      Check (C.Cwnd = 3000, "idle restart never grows the window");

      --  Past 2 GiB without loss, recover must still guard correctly.
      Initialize (C, 1000, 0);
      for I in Unsigned_32 range 1 .. 4 loop
         On_Ack (C, 1000, Seq (I * 1_000_000_000), 0, A);
      end loop;
      for I in 1 .. 3 loop
         On_Duplicate_Ack (C, 4_000_000_000, 4_000_010_000, 10_000, D);
      end loop;
      Check (D = Fast_Retransmit, "fast retransmit after the sequence space wraps");
   end;

   --  Option negotiation and window scaling.
   declare
      use TCP_Options;
      Ours : constant Offer := (MSS => 1460, others => <>);
      Full : constant Received :=
        (Has_MSS => True, MSS => 1460, Has_Window_Scale => True, Shift_Count => 9,
         SACK_Permitted => True, Has_Timestamps => True, others => <>);
      A : Agreement;
   begin
      Check (MSS_For (1500, False) = 1460 and then MSS_For (1500, True) = 1440,
             "MSS from the link MTU");
      A := Negotiate (Ours, Full, 1460, False);
      Check (A.Scaling and then A.Rcv_Shift = 7 and then A.Snd_Shift = 9 and then A.SACK
             and then A.Timestamps and then A.Send_MSS = 1448,
             "both SYNs carry everything: all in effect, MSS less timestamps");
      A := Negotiate (Ours, (Full with delta Has_Window_Scale => False), 1460, False);
      Check (not A.Scaling and then A.Rcv_Shift = 0 and then A.Snd_Shift = 0,
             "scaling needs both SYNs");
      A := Negotiate (Ours, (Full with delta Shift_Count => 20), 1460, False);
      Check (A.Snd_Shift = 14, "a shift above 14 is used as 14");
      A := Negotiate (Ours, (Full with delta Has_MSS => False, Has_Timestamps => False),
                      1460, False);
      Check (A.Send_MSS = 536 and then not A.Timestamps, "no MSS option: 536");
      A := Negotiate (Ours, (Full with delta MSS => 10), 1460, False);
      Check (A.Send_MSS = Minimum_MSS, "tiny MSS: floored");
      A := Negotiate (Ours, (Full with delta MSS => 9000, Has_Timestamps => False),
                      1460, False);
      Check (A.Send_MSS = 1460, "large peer MSS: our link limits");
      Check (Scaled (65_535, 14) = 1_073_725_440, "largest scaled window");
      Check (Advertise (100_000, 7) = 781 and then Scaled (781, 7) <= 100_000,
             "advertised window rounds down");
      Check (Advertise (2 ** 30, 0) = 65_535, "unscaled window saturates");
      A := Negotiate (Ours, Full, 1460, False);
      Check (Peer_Window (1000, True, A) = 1000 and then Peer_Window (1000, False, A) = 512_000,
             "SYN windows are never scaled");
   end;

   --  Timestamps: PAWS and TS.Recent.
   declare
      use TCP_Timestamps;
      S : State;
      V : Verdict;
      Day : constant := 24 * 60 * 60 * 1000;
   begin
      Update (S, 100, 10, 10, 0);
      Check (S.Recent_Valid and then S.Recent = 100, "TS.Recent from a segment at Last.ACK.sent");
      TCP_Timestamps.Check (S,50, False, 1000, V);
      Check (V = Refuse, "PAWS refuses an old timestamp");
      TCP_Timestamps.Check (S,50, True, 1000, V);
      Check (V = Pass, "a RST is not refused by PAWS");
      TCP_Timestamps.Check (S,150, False, 1000, V);
      Check (V = Pass, "newer timestamp passes");
      Update (S, 150, 20, 10, 1000);
      Check (S.Recent = 100, "segment after Last.ACK.sent does not set TS.Recent");
      Update (S, 150, 5, 10, 1000);
      Check (S.Recent = 150 and then S.Recent_Time = 1000, "TS.Recent advances");
      Update (S, 120, 5, 10, 2000);
      Check (S.Recent = 150, "TS.Recent never moves backwards");
      S := (Recent => Seq'Last - 9, Recent_Valid => True, Recent_Time => 0);
      TCP_Timestamps.Check (S,5, False, 10, V);
      Check (V = Pass, "timestamps compare across the wrap");
      TCP_Timestamps.Check (S,1, False, 25 * Day, V);
      Check (V = Pass and then not S.Recent_Valid, "TS.Recent expires after 24 days");
      Update (S, 1, 5, 10, 25 * Day);
      Check (S.Recent_Valid and then S.Recent = 1, "expired TS.Recent is replaced");
      Check (RTT_Sample (1000, 900) = 100 and then RTT_Sample (5, Seq'Last - 4) = 10
             and then RTT_Sample (Seq'Last, 0) = TCP_RTO.Maximum_Sample,
             "RTT samples, across the wrap, clipped");
   end;

   --  SACK scoreboard.
   declare
      use Scoreboard_8;
      B : Board;
   begin
      Clear (B);
      Add (B, 2000, 3000);
      Check (SACKed (B, 2500) and then not SACKed (B, 1999) and then not SACKed (B, 3000),
             "SACK block recorded, half-open");
      Add (B, 3000, 4000);
      Check (Scoreboard_8.Count (B) = 1 and then SACKed (B, 3500), "adjacent blocks merge");
      Add (B, 6000, 7000);
      Check (Scoreboard_8.Count (B) = 2 and then not SACKed (B, 5000), "separate block kept apart");
      Add (B, 3500, 6500);
      Check (Scoreboard_8.Count (B) = 1 and then SACKed (B, 5000), "block bridging two ranges merges all");
      Check (Is_Lost (B, 0, 1000) and then not Is_Lost (B, 6500, 1000),
             "IsLost: more than 2 SMSS SACKed above");
      Check (Next_Unsacked (B, 0) = 0 and then Next_Unsacked (B, 2500) = 7000,
             "next unSACKed byte");
      Advance (B, 2500);
      Check (SACKed (B, 0) and then SACKed (B, 4499) and then not SACKed (B, 4500),
             "SND.UNA advancing shifts the board");
      Advance (B, 10_000);
      Check (Scoreboard_8.Count (B) = 0, "everything acknowledged: board empty");

      Add (B, 100, 101);
      Add (B, 200, 201);
      Add (B, 300, 301);
      Check (Is_Lost (B, 50, 1000) and then not Is_Lost (B, 150, 1000),
             "IsLost: three separate SACKed ranges above");

      Clear (B);
      for I in 1 .. 9 loop
         Add (B, I * 100, I * 100 + 10);
      end loop;
      Check (Scoreboard_8.Count (B) = 8 and then SACKed (B, 800) and then not SACKed (B, 900),
             "full board drops a new block, keeps the old");
   end;

   --  SipHash-2-4 reference vectors (key 00 .. 0f; messages 00, 01, ...),
   --  ISNs and SYN cookies.
   declare
      use SipHash;
      use TCP_Syn_Cookies;
      K : constant Key := To_Key ([for I in 0 .. 15 => Unsigned_8 (I)]);
      E : TCP_Isn.Endpoints;
      F : TCP_Isn.Endpoints;
      Cookie : Seq;
   begin
      Check (Hash (K, [1 .. 0 => 0]) = 16#726f_db47_dd0e_0e31#, "SipHash: empty message");
      Check (Hash (K, [for I in 0 .. 14 => Unsigned_8 (I)]) = 16#a129_ca61_49be_45e5#,
             "SipHash: 15-byte message (the paper's example)");

      E.Local (11 .. 16) := [16#FF#, 16#FF#, 10, 0, 2, 15];
      E.Remote (11 .. 16) := [16#FF#, 16#FF#, 93, 184, 216, 34];
      E.Local_Port := 49_152;
      E.Remote_Port := 443;
      F := (E with delta Local_Port => 49_153);
      Check (TCP_Isn.Initial_Sequence (K, E, 1000) - TCP_Isn.Initial_Sequence (K, E, 0) = 1000,
             "ISN advances with the clock");
      Check (TCP_Isn.Initial_Sequence (K, E, 0) /= TCP_Isn.Initial_Sequence (K, F, 0),
             "ISN differs per 4-tuple");

      Check (Index_For (1460) = 5 and then Index_For (1000) = 0 and then Index_For (9000) = 7,
             "cookie MSS index: largest entry not above");
      Cookie := Make (K, E, 777, 6400, 1460);
      Check (Accepts (K, E, 777, Cookie, 6400) and then Accepts (K, E, 777, Cookie, 6400 + 64)
             and then Cookie_MSS (Cookie) = 1460, "cookie accepted in its period and the next");
      Check (not Accepts (K, E, 777, Cookie, 6400 + 128), "cookie expires");
      Check (not Accepts (K, E, 778, Cookie, 6400) and then not Accepts (K, F, 777, Cookie, 6400),
             "cookie bound to the client ISN and 4-tuple");
   end;

   --  Chunk pool: ownership, zeroing, accounting.
   declare
      use Pool_Small;
      P : Pool;
      C, D : Chunk_Id;
      OK : Boolean;
   begin
      Initialize (P);
      Check (Free_Count (P) = 8 and then Held (P, 1) = 0, "pool starts free");
      Allocate (P, 1, C, OK);
      Check (OK and then Owner (P, C) = 1 and then Held (P, 1) = 1 and then Free_Count (P) = 7,
             "allocate charges the owner");
      Write (P, 1, C, 3, 42);
      Check (Read (P, 1, C, 3) = 42, "owner reads what it wrote");
      Release (P, 1, C);
      Check (Owner (P, C) = Free and then Held (P, 1) = 0 and then Free_Count (P) = 8,
             "release returns the chunk");
      Allocate (P, 2, D, OK);
      Check (OK and then D = C and then Read (P, 2, D, 3) = 0,
             "a reused chunk carries nothing from its last owner");
      for I in 1 .. 7 loop
         Allocate (P, 3, C, OK);
      end loop;
      Check (OK and then Free_Count (P) = 0 and then Held (P, 3) = 7, "pool fills");
      Allocate (P, 4, C, OK);
      Check (not OK and then Held (P, 4) = 0, "empty pool refuses");
   end;

   --  Connection table: lookup, stale handles, limits.
   declare
      use Table_Small;
      T : Table;
      H, H2 : Handle;
      St : Insert_Status;
      E : TCP_Isn.Endpoints;
      F : TCP_Isn.Endpoints;
      Placed : Natural := 0;
   begin
      Initialize (T, SipHash.To_Key ([for I in 0 .. 15 => Unsigned_8 (I)]));
      E.Remote_Port := 443;
      E.Local_Port := 50_000;
      Insert (T, E, H, St);
      Check (St = Inserted and then Find (T, E) = H.Index and then Table_Small.Count (T) = 1,
             "insert then find");
      Insert (T, E, H2, St);
      Check (St = Exists and then Table_Small.Count (T) = 1, "a 4-tuple opens once");
      F := (E with delta Local_Port => 50_001);
      Check (Find (T, F) = 0, "an absent tuple is not found");
      Remove (T, H);
      Check (Find (T, E) = 0 and then not Current (T, H) and then Table_Small.Count (T) = 0,
             "removed: not found, handle stale");
      Insert (T, E, H2, St);
      Check (St = Inserted and then not Current (T, H) and then Current (T, H2),
             "a reused slot does not revive an old handle");
      for P in Unsigned_16 range 1 .. 20 loop
         Insert (T, (E with delta Local_Port => P), H, St);
         if St = Inserted then
            Placed := Placed + 1;
         end if;
      end loop;
      Check (Table_Small.Count (T) = 1 + Placed and then Table_Small.Count (T) <= 8, "the table never exceeds its slots");
   end;

   --  Timers: earliest first, re-arming, cancelling.
   declare
      use Timers_Small;
      H : Heap;
      T : Timer_Id;
      Found : Boolean;
   begin
      Initialize (H);
      Arm (H, 5, 300);
      Arm (H, 2, 100);
      Arm (H, 9, 200);
      Next_Due (H, 50, T, Found);
      Check (not Found, "nothing due before the earliest deadline");
      Next_Due (H, 150, T, Found);
      Check (Found and then T = 2, "earliest deadline first");
      Arm (H, 2, 400);
      Next_Due (H, 1000, T, Found);
      Check (Found and then T = 9 and then Timers_Small.Count (H) = 3,
             "re-arming moves a timer, count unchanged");
      Cancel (H, 9);
      Next_Due (H, 1000, T, Found);
      Check (Found and then T = 5 and then not Armed (H, 9), "cancelled timer gone");
      Cancel (H, 9);
      Check (Timers_Small.Count (H) = 2, "cancelling twice is harmless");
      for I in Timer_Id loop
         Arm (H, I, Time (1000 - I));
      end loop;
      Next_Due (H, 1000, T, Found);
      Check (Found and then T = 16 and then Timers_Small.Count (H) = 16, "full heap: earliest first");
   end;

   --  Chunked send queue over the pool: allocation, ACK release, isolation.
   declare
      package SQ renames Send_Chunked_Small;
      P : Pool_Small.Pool;
      A, B : SQ.Queue;
      Got : Natural;
      Seg : Pool_Small.Byte_Array (1 .. 64);
      First : Seq;
      R : SQ.Ack_Result;
      Freed : Natural;
      use type SQ.Ack_Result;
   begin
      Pool_Small.Initialize (P);
      SQ.Initialize (A, P, 1, 1000);
      SQ.Initialize (B, P, 2, 5000);
      SQ.Push (A, P, [for I in 1 .. 40 => Unsigned_8 (I - 1)], Got);
      Check (Got = 40 and then SQ.Count (A) = 40 and then Pool_Small.Held (P, 1) = 3,
             "push allocates chunks as data arrives");
      SQ.Push (B, P, [for I in 1 .. 20 => 200], Got);
      Check (Got = 20 and then Pool_Small.Held (P, 2) = 2, "a second queue shares the pool");
      SQ.Take (A, P, 20, Seg, First, Got);
      Check (Got = 20 and then First = 1000 and then Seg (1) = 0 and then Seg (20) = 19,
             "take returns the bytes at SND.NXT");
      SQ.Acknowledge (A, P, 1016, R, Freed);
      Check (R = SQ.Advanced and then Freed = 16 and then Pool_Small.Held (P, 1) = 2,
             "an ACK past a whole chunk releases it");
      SQ.Take (A, P, 30, Seg, First, Got);
      Check (Got = 20 and then First = 1020 and then Seg (1) = 20 and then Seg (20) = 39,
             "the rest follows in order");
      SQ.Peek (A, P, 0, 64, Seg, Got);
      Check (Got = 24 and then Seg (1) = 16 and then Seg (24) = 39,
             "retransmission resends identical bytes");
      SQ.Acknowledge (A, P, 2000, R, Freed);
      Check (R = SQ.Unsent_Data and then SQ.Count (A) = 24, "ACK of unsent data refused");
      SQ.Take (B, P, 64, Seg, First, Got);
      Check (Got = 20 and then Seg (1) = 200 and then Seg (20) = 200,
             "the other queue's bytes are untouched");
      SQ.Acknowledge (A, P, 1040, R, Freed);
      Check (Freed = 24 and then SQ.Count (A) = 0 and then Pool_Small.Held (P, 1) = 1,
             "all acknowledged: only the partly used chunk is kept");
      SQ.Initialize (A, P, 3, 0);
      SQ.Push (A, P, [for I in 1 .. 48 => 7], Got);
      Check (Got = 48 and then Pool_Small.Free_Count (P) = 2, "a third queue takes what it needs");
      SQ.Release_All (B, P);
      Check (Pool_Small.Held (P, 2) = 0 and then SQ.Count (B) = 0 and then Pool_Small.Free_Count (P) = 4,
             "closing a queue returns all its chunks");
   end;

   --  IPv4 headers: each acceptance rule, then random bytes against an
   --  independent restatement of the rule.
   declare
      use IPv4_Header;
      Base : constant IPv4_Header.Bytes (0 .. 39) :=
        [16#45#, 0, 0, 40, 0, 1, 16#40#, 0, 64, 6, 0, 0, 10, 0, 2, 2, 10, 0, 2, 15,
         others => 16#AA#];
      H : Header;
      function Mutated (I : Natural; V : Unsigned_8) return IPv4_Header.Bytes is
         X : IPv4_Header.Bytes := Base;
      begin
         X (I) := V;
         return X;
      end Mutated;
      Seed : Unsigned_32 := 99;
      Agree : Boolean := True;
   begin
      Check (Well_Formed (Base), "IPv4 plain header accepted");
      Parse (Base, H);
      Check (H.Size = 20 and then H.Total_Length = 40 and then H.Protocol = 6 and then
             H.Source = [10, 0, 2, 2] and then H.Destination = [10, 0, 2, 15], "IPv4 fields");
      Check (not Well_Formed (Mutated (0, 16#65#)), "IPv4 version 6 rejected");
      Check (not Well_Formed (Mutated (0, 16#44#)), "IPv4 IHL 4 rejected");
      Check (Well_Formed (Mutated (0, 16#46#)), "IPv4 with options accepted");
      Check (not Well_Formed (Mutated (0, 16#4B#)), "IPv4 header longer than the packet rejected");
      Check (not Well_Formed (Mutated (3, 41)), "IPv4 truncated datagram rejected");
      Check (Well_Formed (Mutated (3, 30)), "IPv4 link padding allowed");
      Check (not Well_Formed (Mutated (3, 19)), "IPv4 total length below the header rejected");
      Check (not Well_Formed (Mutated (6, 16#20#)), "IPv4 MF rejected (no reassembly)");
      Check (not Well_Formed (Mutated (7, 1)), "IPv4 fragment offset rejected");
      Check (not Well_Formed (Mutated (6, 16#80#)), "IPv4 reserved flag rejected");
      Check (not Well_Formed (Base (0 .. 18)), "IPv4 short packet rejected");
      for N in 1 .. 200_000 loop
         declare
            Len : Natural;
            X : IPv4_Header.Bytes (0 .. 63);
         begin
            Seed := Seed * 1_103_515_245 + 12_345;
            Len := Natural (Shift_Right (Seed, 16) mod 65);
            for I in X'Range loop
               Seed := Seed * 1_103_515_245 + 12_345;
               X (I) := Unsigned_8 (Shift_Right (Seed, 16) and 16#FF#);
            end loop;
            X (0) := (if N mod 2 = 0 then 16#45# else X (0));
            X (2) := 0;
            X (3) := (if N mod 3 = 0 then Unsigned_8 (Len) else X (3));
            X (6) := (if N mod 5 /= 0 then X (6) and 16#40# else X (6));
            X (7) := (if N mod 5 /= 0 then 0 else X (7));
            declare
               P : constant IPv4_Header.Bytes := X (0 .. Len - 1);
               Expected : constant Boolean :=
                 Len >= 20 and then P (0) / 16 = 4 and then Natural (P (0) mod 16) * 4 >= 20 and then
                 Natural (P (2)) * 256 + Natural (P (3)) >= Natural (P (0) mod 16) * 4 and then
                 Natural (P (2)) * 256 + Natural (P (3)) <= Len and then
                 P (6) / 32 mod 2 = 0 and then P (6) / 128 = 0 and then P (6) mod 32 = 0 and then
                 P (7) = 0;
            begin
               if Well_Formed (P) /= Expected then
                  Agree := False;
               end if;
            end;
         end;
      end loop;
      Check (Agree, "IPv4 acceptance matches its restatement on 200,000 random packets");
   end;

   --  ARP cache: the spoofing attempts.
   declare
      use ARP_Cache;
      use type ARP_Packet.MAC;
      T : Table;
      HW : ARP_Packet.MAC;
      Found : Boolean;
      Gateway : constant ARP_Packet.IPv4 := [10, 0, 2, 2];
      Real    : constant ARP_Packet.MAC := [16#52#, 16#55#, 10, 0, 2, 2];
      Evil    : constant ARP_Packet.MAC := [16#02#, 16#66#, 6, 6, 6, 6];
      function Pkt (Op : ARP_Packet.Operation; IP : ARP_Packet.IPv4; M : ARP_Packet.MAC) return ARP_Packet.Packet is
        ((Op => Op, Sender_HW => M, Sender_IP => IP, Target_HW => [others => 0],
          Target_IP => [10, 0, 2, 15]));
   begin
      Learn (T, Pkt (ARP_Packet.Reply, Gateway, Evil), Ours => True, Now => 1);
      Lookup (T, Gateway, HW, Found);
      Check (not Found, "ARP: unsolicited reply ignored (no poisoning)");
      ARP_Cache.Request (T, Gateway, 2);
      Learn (T, Pkt (ARP_Packet.Reply, Gateway, Real), Ours => True, Now => 3);
      Lookup (T, Gateway, HW, Found);
      Check (Found and then HW = Real, "ARP: the answer to our request resolves");
      Learn (T, Pkt (ARP_Packet.Reply, Gateway, Evil), Ours => True, Now => 4);
      Lookup (T, Gateway, HW, Found);
      Check (Found and then HW = Real, "ARP: a later forged reply changes nothing");
      Learn (T, Pkt (ARP_Packet.Request, Gateway, Evil), Ours => True, Now => 5);
      Lookup (T, Gateway, HW, Found);
      Check (Found and then HW = Real, "ARP: a forged request cannot rewrite a resolved address");
      Learn (T, Pkt (ARP_Packet.Request, [10, 0, 2, 7], [16#02#, 0, 0, 0, 0, 7]), Ours => True, Now => 6);
      Lookup (T, [10, 0, 2, 7], HW, Found);
      Check (Found, "ARP: a request for us teaches its sender (RFC 826 merge)");
      Learn (T, Pkt (ARP_Packet.Request, [10, 0, 2, 8], [16#02#, 0, 0, 0, 0, 8]), Ours => False, Now => 7);
      Lookup (T, [10, 0, 2, 8], HW, Found);
      Check (not Found, "ARP: a request for someone else teaches nothing");
      Learn (T, Pkt (ARP_Packet.Request, [224, 0, 0, 1], [16#02#, 0, 0, 0, 0, 9]), Ours => True, Now => 8);
      Learn (T, Pkt (ARP_Packet.Request, [10, 0, 2, 9], [16#01#, 0, 0, 0, 0, 9]), Ours => True, Now => 9);
      Learn (T, Pkt (ARP_Packet.Request, [0, 0, 0, 0], [16#02#, 0, 0, 0, 0, 9]), Ours => True, Now => 10);
      Lookup (T, [224, 0, 0, 1], HW, Found);
      Check (not Found, "ARP: multicast sender IP never learned");
      Lookup (T, [10, 0, 2, 9], HW, Found);
      Check (not Found, "ARP: multicast sender hardware address never learned");

      --  Aging: a doubted mapping stays usable while it is asked again, and
      --  only the answer to that question may change it (a replaced
      --  router); unanswered, it is given up.
      declare
         New_Router : constant ARP_Packet.MAC := [16#52#, 16#55#, 10, 0, 2, 99];
      begin
         Reconfirm (T, Gateway, 100);
         Lookup (T, Gateway, HW, Found);
         Check (Found and then HW = Real, "ARP: a doubted mapping stays usable");
         Learn (T, Pkt (ARP_Packet.Request, Gateway, Evil), Ours => True, Now => 101);
         Lookup (T, Gateway, HW, Found);
         Check (Found and then HW = Real, "ARP: a request cannot rewrite a doubted mapping");
         Learn (T, Pkt (ARP_Packet.Reply, Gateway, New_Router), Ours => True, Now => 102);
         Lookup (T, Gateway, HW, Found);
         Check (Found and then HW = New_Router, "ARP: the answer to our doubt may change the address");
         Learn (T, Pkt (ARP_Packet.Reply, Gateway, Evil), Ours => True, Now => 103);
         Lookup (T, Gateway, HW, Found);
         Check (Found and then HW = New_Router, "ARP: once reconfirmed, unsolicited replies are ignored again");
         Reconfirm (T, Gateway, 200);
         Expire (T, 202, 3);
         Lookup (T, Gateway, HW, Found);
         Check (Found, "ARP: a doubt is not given up early");
         Expire (T, 203, 3);
         Lookup (T, Gateway, HW, Found);
         Check (not Found, "ARP: an unanswered doubt is given up");
      end;
   end;

   --  Descriptor ownership against a device returning ids at random,
   --  including out-of-range ids, free ones and repeats.
   declare
      use Descriptors_80;
      P : Pool;
      Owned : array (Id) of Boolean := [others => False];
      Seed : Unsigned_32 := 77;
      D : Id;
      OK, Double, Lost : Boolean := False;
   begin
      Initialize (P);
      for Round in 1 .. 200_000 loop
         Seed := Seed * 1_103_515_245 + 12_345;
         if Seed mod 3 /= 0 then
            Take (P, D, OK);
            if OK then
               Double := Double or else Owned (D);
               Owned (D) := True;
            end if;
         else
            declare
               Raw : constant Unsigned_32 := Shift_Right (Seed, 8) mod 100;
               Was : constant Boolean := Raw < 80 and then Owned (Natural (Raw));
            begin
               Give_Back (P, Raw, OK);
               Lost := Lost or else OK /= Was;
               if OK then
                  Owned (Natural (Raw)) := False;
               end if;
            end;
         end if;
      end loop;
      Check (not Double and then not Lost,
             "descriptors: none handed out twice; device ids accepted exactly when in flight");
   end;

   --  Channel arenas: fit, claims against a model, owners and stale handles.
   declare
      use Channel_Arenas;
      T : Table;
      A, B, Old : Handle;
      AI, BI, I : Arena_Index;
      Slot : Buffer_Index;
      Offset : Span_Bytes;
      OK, Found, Double, Lost : Boolean := False;
      Size : constant Buffer_Bytes := 4_096 + 2 * 16_384;
      Model : array (Buffer_Index) of Boolean := [others => False];
      Seed : Unsigned_32 := 91;
      Gone : Arena_List;
   begin
      Check (Fits (Minimum_Buffer, 1_024) and then not Fits (Minimum_Buffer, 0) and then
             Fits (Buffer_Bytes'Last, 7) and then not Fits (Buffer_Bytes'Last, 8),
             "arenas: a grant holds what fits in 16 MiB and no more");
      Register (T, 7, Size, 400, A, AI, OK);
      Check (OK, "arenas: register");
      Register (T, 7, Size, 500, Old, I, OK);
      Check (not OK, "arenas: 500 buffers of 36 KiB exceed one grant");
      Register (T, 8, Size, 4, B, BI, OK);
      Check (OK and then B /= A, "arenas: second owner");
      Claim (T, 8, A, 0, I, Slot, Offset, OK);
      Check (not OK, "arenas: another owner's arena refused");
      Claim (T, 7, A, 400, I, Slot, Offset, OK);
      Check (not OK, "arenas: buffer past the count refused");
      for Round in 1 .. 100_000 loop
         Seed := Seed * 1_103_515_245 + 12_345;
         declare
            Raw : constant Unsigned_64 := Unsigned_64 (Shift_Right (Seed, 8) mod 420);
         begin
            if Seed mod 3 /= 0 then
               Claim (T, 7, A, Raw, I, Slot, Offset, OK);
               if Raw < 400 then
                  Double := Double or else (OK and then Model (Natural (Raw)));
                  Lost := Lost or else (OK = Model (Natural (Raw)));
                  if OK then
                     Model (Slot) := True;
                     Lost := Lost or else Offset /= Slot * Size or else I /= AI;
                  end if;
               else
                  Lost := Lost or else OK;
               end if;
            elsif Raw < 400 then
               Release (T, AI, Natural (Raw));
               Model (Natural (Raw)) := False;
            end if;
         end;
      end loop;
      Check (not Double and then not Lost,
             "arenas: a buffer is claimed only when free, at its offset");
      Model (0) := True;
      Claim (T, 7, A, 0, I, Slot, Offset, Found);
      Unregister (T, 7, A, I, OK);
      Check (not OK, "arenas: unregister refused while buffers are held");
      for S in Buffer_Index range 0 .. 399 loop
         Release (T, AI, S);
      end loop;
      Unregister (T, 7, A, I, OK);
      Check (OK, "arenas: idle arena unregisters");
      Claim (T, 7, A, 0, I, Slot, Offset, OK);
      Check (not OK, "arenas: stale handle names nothing");
      Register (T, 7, Size, 2, Old, I, OK);
      Check (OK and then Old /= A, "arenas: handles are not reused");
      Release_Owner (T, 7, Gone);
      Check (Gone (I) and then not Gone (BI), "arenas: owner exit releases only its arenas");
      Find (T, 8, B, I, Found);
      Check (Found, "arenas: other owner's arena survives");
   end;

   --  A device's used index: taken only up to what the device holds.
   declare
      use Virtqueue_Index;
      Bad : Boolean := False;
   begin
      for Last in Index'Range loop
         for Step in 0 .. 300 loop
            declare
               Device : constant Index := Last + Index (Step mod 2 ** 16);
               Got : constant Natural := New_Entries (Last, Device, Step mod 257);
            begin
               Bad := Bad or else Got /= (if Step <= Step mod 257 then Step else 0);
            end;
         end loop;
         exit when Last > 2_000;
      end loop;
      Check (not Bad and then New_Entries (65_535, 3, 4) = 4 and then
             New_Entries (10, 9, 256) = 0,
             "virtqueue: used index taken only within what the device holds, across wrap");
   end;


   --  IPv6 headers: a real packet, round trips, and the mapped-address rule.
   declare
      use IPv6_Header;
      Packet : IPv6_Header.Bytes (0 .. 59) := [others => 0];
      H, Back : Header;
      Mapped_Peer : constant Address := [0 .. 9 => 0, 10 => 16#FF#, 11 => 16#FF#,
                                         12 => 10, 13 => 0, 14 => 2, 15 => 2];
      Doc : constant Address := [0 => 16#20#, 1 => 16#01#, 2 => 16#0D#, 3 => 16#B8#,
                                 15 => 1, others => 0];
      Seed : Unsigned_32 := 11;
      Bad : Boolean := False;
   begin
      H := (Traffic_Class => 16#B8#, Flow_Label => 16#1_2345#, Payload_Length => 20,
            Next_Header => 6, Hop_Limit => 64, Source => Doc,
            Destination => [0 => 16#FE#, 1 => 16#80#, 15 => 7, others => 0]);
      Build (H, Packet);
      Check (Packet (0) = 16#6B# and then Packet (1) = 16#81# and then
             Packet (2) = 16#23# and then Packet (3) = 16#45#,
             "ipv6: version, traffic class and flow label on the wire");
      Parse (Packet, Back);
      Check (Back = H, "ipv6: build then parse round-trips");
      Packet (8 .. 23) := IPv6_Header.Bytes (Mapped_Peer);
      Check (not Well_Formed (Packet), "ipv6: mapped IPv4 source refused");
      Packet (8 .. 23) := IPv6_Header.Bytes (Doc);
      Packet (24 .. 39) := IPv6_Header.Bytes (Mapped_Peer);
      Check (not Well_Formed (Packet), "ipv6: mapped IPv4 destination refused");
      Packet (24 .. 39) := [others => 0];
      Check (not Well_Formed (Packet), "ipv6: unspecified destination refused");
      Packet (24 .. 39) := IPv6_Header.Bytes (Doc);
      Packet (8) := 16#FF#;
      Check (not Well_Formed (Packet), "ipv6: multicast source refused");
      Packet (8 .. 23) := IPv6_Header.Bytes (Doc);
      Packet (4 .. 5) := [0, 21];
      Check (not Well_Formed (Packet), "ipv6: payload past the bytes received refused");
      Packet (4 .. 5) := [0, 20];
      Packet (0) := 16#4B#;
      Check (not Well_Formed (Packet), "ipv6: version 4 refused");
      --  Random bytes: whatever parses obeys the rules.
      for Round in 1 .. 200_000 loop
         for I in Packet'Range loop
            Seed := Seed * 1_103_515_245 + 12_345;
            Packet (I) := Unsigned_8 (Shift_Right (Seed, 16) and 16#FF#);
         end loop;
         if Round mod 2 = 0 then
            Packet (0) := 16#60# or (Packet (0) and 16#0F#);
            Packet (4) := 0;
            Packet (5) := Packet (5) mod 21;
         end if;
         if Well_Formed (Packet) then
            Parse (Packet, Back);
            Bad := Bad or else Is_Mapped (Back.Source) or else Is_Mapped (Back.Destination)
              or else Size + Back.Payload_Length > Packet'Length;
         end if;
      end loop;
      Check (not Bad, "ipv6: random packets accepted only by the rules");
   end;

   --  Neighbor Discovery: messages, then the hardened cache.
   declare
      use ND_Message;
      Msg : IPv6_Header.Bytes (0 .. 31) := [others => 0];
      M : Message;
      OK : Boolean;
      Peer : constant IPv6_Header.Address :=
        [0 => 16#FE#, 1 => 16#80#, 15 => 16#22#, others => 0];
      Peer_MAC : constant MAC := [16#52#, 16#54#, 0, 16#12#, 16#34#, 16#56#];
      Forged   : constant MAC := [16#02#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#];
      T : Neighbor_Cache.Table;
      Link : MAC;
      Found : Boolean;
   begin
      --  A solicitation with a source link-layer option.
      Msg (0) := Solicitation_Type;
      Msg (8 .. 23) := IPv6_Header.Bytes (Peer);
      Msg (24 .. 31) := [Source_Link_Option, 1, 16#52#, 16#54#, 0, 16#12#, 16#34#, 16#56#];
      Parse (Msg, 255, M, OK);
      Check (OK and then M.Of_Kind = Solicitation and then M.Has_Link and then
             M.Link = Peer_MAC, "nd: solicitation with its link address");
      Parse (Msg, 254, M, OK);
      Check (not OK, "nd: hop limit other than 255 refused (off-link)");
      Msg (25) := 0;
      Parse (Msg, 255, M, OK);
      Check (not OK, "nd: zero-length option refused (the walk would not end)");
      Msg (25) := 2;
      Parse (Msg, 255, M, OK);
      Check (not OK, "nd: option past the message refused");
      Msg (25) := 1;
      Msg (8) := 16#FF#;
      Parse (Msg, 255, M, OK);
      Check (not OK, "nd: multicast target refused");

      --  The cache.
      Neighbor_Cache.Learn
        (T, (Of_Kind => Advertisement, Solicited => False, Peer => Peer,
             Has_Link => True, Link => Forged, others => <>), 1);
      Neighbor_Cache.Lookup (T, Peer, Link, Found);
      Check (not Found, "nd cache: unsolicited advertisement learns nothing");
      Neighbor_Cache.Learn
        (T, (Of_Kind => Advertisement, Solicited => True, Peer => Peer,
             Has_Link => True, Link => Forged, others => <>), 2);
      Neighbor_Cache.Lookup (T, Peer, Link, Found);
      Check (not Found, "nd cache: solicited advertisement we never asked for learns nothing");
      Neighbor_Cache.Solicit (T, Peer, 3);
      Neighbor_Cache.Learn
        (T, (Of_Kind => Advertisement, Solicited => True, Peer => Peer,
             Has_Link => True, Link => Peer_MAC, others => <>), 4);
      Neighbor_Cache.Lookup (T, Peer, Link, Found);
      Check (Found and then Link = Peer_MAC, "nd cache: answer to our solicitation resolves");
      Neighbor_Cache.Learn
        (T, (Of_Kind => Advertisement, Solicited => False, Peer => Peer,
             Has_Link => True, Link => Forged, others => <>), 5);
      Neighbor_Cache.Learn
        (T, (Of_Kind => Solicitation, Ours => True, Peer => Peer,
             Has_Link => True, Link => Forged, others => <>), 6);
      Neighbor_Cache.Lookup (T, Peer, Link, Found);
      Check (Found and then Link = Peer_MAC,
             "nd cache: neither an override nor a solicitation rewrites a resolved address");
      Neighbor_Cache.Learn
        (T, (Of_Kind => Solicitation, Ours => True,
             Peer => [0 .. 9 => 0, 10 => 16#FF#, 11 => 16#FF#, 12 => 10, 13 .. 14 => 0, 15 => 9],
             Has_Link => True, Link => Peer_MAC, others => <>), 7);
      Neighbor_Cache.Lookup
        (T, [0 .. 9 => 0, 10 => 16#FF#, 11 => 16#FF#, 12 => 10, 13 .. 14 => 0, 15 => 9], Link, Found);
      Check (not Found, "nd cache: an IPv4-mapped sender is never learned");
   end;

   --  SLAAC: router advertisements, then addresses and their lifetimes.
   declare
      RA : IPv6_Header.Bytes (0 .. 47) := [others => 0];
      Router : constant IPv6_Header.Address := [0 => 16#FE#, 1 => 16#80#, 15 => 1, others => 0];
      Adv : RA_Message.Advertisement;
      OK : Boolean;
      T : SLAAC_Table.Table;
      Doc_64 : constant IPv6_Header.Address :=
        [0 => 16#20#, 1 => 16#01#, 2 => 16#0D#, 3 => 16#B8#, others => 0];
      Key : constant SipHash.Key := (K0 => 16#0706050403020100#, K1 => 16#0F0E0D0C0B0A0908#);
      ID : SLAAC_Table.Interface_ID;
      use type SLAAC_Table.State;
      use type IPv6_Header.Address;
      use type SLAAC_Table.Interface_ID;
   begin
      RA (0) := RA_Message.Advertisement_Type;
      RA (6 .. 7) := [0, 30];                                --  router lifetime
      RA (16 .. 19) := [RA_Message.Prefix_Option, 4, 64, 16#C0#];   --  L and A
      RA (20 .. 23) := [0, 0, 16#1C#, 16#20#];               --  valid 7200 s
      RA (24 .. 27) := [0, 0, 16#0E#, 16#10#];               --  preferred 3600 s
      RA (32 .. 39) := [16#20#, 16#01#, 16#0D#, 16#B8#, 0, 0, 0, 0];
      RA_Message.Parse (RA, 255, Router, Adv, OK);
      Check (OK and then Adv.Count = 1 and then Adv.Prefixes (1).Valid = 7_200 and then
             Adv.Prefixes (1).Network = Doc_64, "ra: one autonomous /64");
      RA_Message.Parse (RA, 254, Router, Adv, OK);
      Check (not OK, "ra: hop limit other than 255 refused");
      RA_Message.Parse (RA, 255, Doc_64, Adv, OK);
      Check (not OK, "ra: a source that is not link-local refused");
      RA (24 .. 27) := [0, 0, 16#FF#, 16#FF#];              --  preferred > valid
      RA_Message.Parse (RA, 255, Router, Adv, OK);
      Check (OK and then Adv.Count = 0, "ra: preferred longer than valid ignored");
      RA (24 .. 27) := [0, 0, 16#0E#, 16#10#];
      RA (32 .. 33) := [16#FE#, 16#80#];
      RA_Message.Parse (RA, 255, Router, Adv, OK);
      Check (OK and then Adv.Count = 0, "ra: link-local prefix ignored");

      --  Addresses.
      ID := SLAAC_Table.Stable_ID (Key, Doc_64, 0);
      Check (not SLAAC_Table.Reserved (ID) and then
             SLAAC_Table.Stable_ID (Key, Doc_64, 0) = ID and then
             SLAAC_Table.Stable_ID (Key, Doc_64, 1) /= ID,
             "slaac: stable identifier, new one per counter");
      SLAAC_Table.Advertised (T, (Network => Doc_64, Valid => 7_200, Preferred => 3_600), ID, 100);
      Check (T (0).St = SLAAC_Table.Tentative and then
             not SLAAC_Table.Preferred_Source (T, T (0).Addr), "slaac: new address is tentative");
      SLAAC_Table.Tick (T, 100);
      Check (T (0).St = SLAAC_Table.Tentative, "slaac: still tentative before detection ends");
      SLAAC_Table.Tick (T, 101);
      Check (SLAAC_Table.Preferred_Source (T, T (0).Addr), "slaac: preferred after detection");
      --  A forged advertisement with a 10-second lifetime, when the
      --  address has under two hours left (7,100 s): nothing changes.
      SLAAC_Table.Advertised (T, (Network => Doc_64, Valid => 10, Preferred => 10), ID, 200);
      Check (T (0).Valid_Until = 100 + 7_200,
             "slaac: two-hour rule, under two hours left: a shorter lifetime is ignored");
      --  With a day left, the same forgery leaves exactly two hours.
      SLAAC_Table.Advertised (T, (Network => Doc_64, Valid => 86_400, Preferred => 3_600), ID, 300);
      SLAAC_Table.Advertised (T, (Network => Doc_64, Valid => 10, Preferred => 10), ID, 400);
      Check (T (0).Valid_Until = 400 + SLAAC_Table.Two_Hours,
             "slaac: two-hour rule, a day left: cut to two hours, not ten seconds");
      --  Detection conflict on a second prefix.
      declare
         Other : IPv6_Header.Address := Doc_64;
      begin
         Other (7) := 1;
         SLAAC_Table.Advertised (T, (Network => Other, Valid => 7_200, Preferred => 3_600),
                                 SLAAC_Table.Stable_ID (Key, Other, 0), 500);
         SLAAC_Table.Conflict (T, T (1).Addr);
         SLAAC_Table.Tick (T, 600);
         Check (T (1).St = SLAAC_Table.Duplicate and then
                not SLAAC_Table.Preferred_Source (T, T (1).Addr),
                "slaac: a claimed address is never used");
      end;
   end;

   --  DNS responses: a CNAME chain then an A record, and damaged forms.
   declare
      use DNS_Response;
      use type DNS_Name.Bytes;
      use type DNS_Response.IPv4;
      Question : constant Bytes :=
        [16#12#, 16#34#, 16#81#, 16#80#, 0, 1, 0, 2, 0, 0, 0, 0,
         3, Character'Pos ('w'), Character'Pos ('w'), Character'Pos ('w'),
         1, Character'Pos ('x'), 0, 0, 1, 0, 1];
      CNAME : constant Bytes :=
        [16#C0#, 12, 0, 5, 0, 1, 0, 0, 0, 60, 0, 4,
         1, Character'Pos ('y'), 16#C0#, 16];
      A : constant Bytes :=
        [16#C0#, 28, 0, 1, 0, 1, 0, 0, 0, 60, 0, 4, 93, 184, 216, 34];
      Whole : constant Bytes := Question & CNAME & A;
      R : Response;
      OK : Boolean;
      function Slid (B : Bytes) return Bytes is
        (Bytes'[for I in 0 .. B'Length - 1 => B (B'First + I)]);
   begin
      Parse (Whole, R, OK);
      Check (OK and then R.Id = 16#1234# and then R.Has_Address and then
             R.Address = [93, 184, 216, 34] and then
             R.Address_At = Question'Length + CNAME'Length + 12,
             "dns: A record after a CNAME");
      Parse (Slid (Whole (0 .. Whole'Last - 1)), R, OK);
      Check (OK and then not R.Has_Address, "dns: truncated A record gives no address");
      declare
         Query : Bytes := Whole;
      begin
         Query (2) := 1;   --  QR clear: a query, not a response
         Parse (Query, R, OK);
         Check (not OK, "dns: a query is not a response");
         Query := Whole;
         Query (7) := 16#FF#;   --  255 answers claimed, two present
         Parse (Query, R, OK);
         Check (OK and then R.Has_Address, "dns: answer count past the data");
         Query := Whole;
         Query (Question'Length + CNAME'Length + 11) := 16;   --  RDLENGTH past the end
         Parse (Query, R, OK);
         Check (OK and then not R.Has_Address, "dns: data length past the end");
         Query := Whole;
         Query (Question'Length - 3) := 28;   --  AAAA question
         Parse (Query, R, OK);
         Check (not OK, "dns: only A questions");
      end;
      --  Any bytes at all: the proof says no fault; run some anyway.
      declare
         Seed : Unsigned_32 := 99;
         Noise : Bytes (0 .. 511);
      begin
         for Round in 1 .. 20_000 loop
            for I in Noise'Range loop
               Seed := Seed * 1_103_515_245 + 12_345;
               Noise (I) := Unsigned_8 (Shift_Right (Seed, 16) and 16#FF#);
            end loop;
            Noise (0 .. Question'Length - 1) := Question;
            Parse (Noise (0 .. Natural (Seed mod 512)), R, OK);
         end loop;
         Check (True, "dns: random tails parse without fault");
      end;
   end;

   --  IPv4 headers written by Build: checksum verifies, parses back.
   declare
      use type IPv4_Header.Address;
      use type IPv4_Header.Bytes;
      use type IPv4_Header.Header;
      Seed : Unsigned_32 := 5;
      function Next return Unsigned_8 is
      begin
         Seed := Seed * 1_103_515_245 + 12_345;
         return Unsigned_8 (Shift_Right (Seed, 16) and 16#FF#);
      end Next;
      All_OK : Boolean := True;
   begin
      for Round in 1 .. 10_000 loop
         declare
            Length : constant Natural := 20 + Natural (Next) * 5;
            B : IPv4_Header.Bytes (0 .. Length - 1) := [others => Next];
            Payload : constant IPv4_Header.Bytes := B (20 .. B'Last);
            H : constant IPv4_Header.Header :=
              (Size => 20, Total_Length => Length, Protocol => Next, TTL => Next,
               Source => [Next, Next, Next, Next], Destination => [Next, Next, Next, Next]);
            P : IPv4_Header.Header;
         begin
            IPv4_Header.Build (H, Round mod 2 = 0, B);
            if Internet_Checksum.Of_Bytes (Internet_Checksum.Bytes (B (0 .. 19))) /= 0 or else
              not IPv4_Header.Well_Formed (B) or else B (20 .. B'Last) /= Payload
            then
               All_OK := False;
            else
               IPv4_Header.Parse (B, P);
               All_OK := All_OK and then P = H;
            end if;
         end;
      end loop;
      Check (All_OK, "ipv4: built headers verify and parse back (10,000 random)");
   end;

   --  ICMP errors: what they say and which packet they quote.
   declare
      use ICMPv4_Error;
      use type IPv4_Header.Address;
      --  Fragmentation needed, MTU 1400, quoting 10.0.2.15:49152 ->
      --  93.184.216.34:443 TCP, sequence 16#01020304#.
      Too_Big_Message : constant IPv4_Header.Bytes :=
        [3, 4, 0, 0, 0, 0, 16#05#, 16#78#,
         16#45#, 0, 5, 16#DC#, 0, 0, 16#40#, 0, 64, 6, 0, 0,
         10, 0, 2, 15, 93, 184, 216, 34,
         16#C0#, 0, 1, 16#BB#, 1, 2, 3, 4];
      E : Error;
      OK : Boolean;
   begin
      Parse (Too_Big_Message, E, OK);
      Check (OK and then E.Of_Kind = Too_Big and then E.Next_Hop_MTU = 1400 and then
             E.Protocol = 6 and then E.Source = [10, 0, 2, 15] and then
             E.Destination = [93, 184, 216, 34] and then E.Source_Port = 49152 and then
             E.Destination_Port = 443 and then E.Sequence = 16#0102_0304#,
             "icmp: fragmentation needed quotes our segment");
      declare
         M : IPv4_Header.Bytes := Too_Big_Message;
      begin
         M (1) := Port_Unreachable;
         Parse (M, E, OK);
         Check (OK and then E.Of_Kind = Hard, "icmp: port unreachable is hard");
         M (1) := 1;
         Parse (M, E, OK);
         Check (OK and then E.Of_Kind = Soft, "icmp: host unreachable is soft");
         M (0) := 11;
         Parse (M, E, OK);
         Check (OK and then E.Of_Kind = Soft, "icmp: time exceeded is soft");
         M := Too_Big_Message;
         M (8) := 16#46#;   --  a 24-byte quoted header, but only 20 + 8 quoted
         Parse (M, E, OK);
         Check (not OK, "icmp: quote shorter than its header refused");
         M := Too_Big_Message;
         M (0) := 8;   --  an echo request is not an error
         Parse (M, E, OK);
         Check (not OK, "icmp: only errors");
      end;
      Parse (Too_Big_Message (0 .. Too_Big_Message'Last - 1), E, OK);
      Check (not OK, "icmp: truncated quote refused");
   end;

   --  Resets for segments that belong to no connection (RFC 9293 3.10.7.1).
   declare
      use TCP_Reset;
      R : Reply;
   begin
      R := For_Closed ((SYN => True, Seq_No => 1000, others => <>));
      Check (R.Send and then R.With_ACK and then R.Seq_No = 0 and then R.Ack_No = 1001,
             "rst: a SYN to a closed port is refused, acknowledging the SYN");
      R := For_Closed ((ACK => True, Seq_No => 5, Ack_No => 77, Length => 10, others => <>));
      Check (R.Send and then not R.With_ACK and then R.Seq_No = 77,
             "rst: a stale ACK is reset at its acknowledgement number");
      R := For_Closed ((FIN => True, Seq_No => 16#FFFF_FFFF#, Length => 2, others => <>));
      Check (R.Send and then R.Ack_No = 2, "rst: SEG.LEN counts data and FIN, wrapping");
      R := For_Closed ((RST => True, ACK => True, others => <>));
      Check (not R.Send, "rst: a reset is never answered");
   end;

   --  EVENT_IDX: Needs_Event is Linux's vring_need_event.
   declare
      use Virtqueue_Index;
      Agree : Boolean := True;
   begin
      for New_Index in Index'(0) .. 3 loop
         for Event in Index loop
            for Old_Index in Index loop
               Agree := Agree and then
                 Needs_Event (Event, New_Index * 21_845, Old_Index) =
                   (New_Index * 21_845 - Event - 1 < New_Index * 21_845 - Old_Index);
            end loop;
         end loop;
      end loop;
      Check (Agree, "virtio: Needs_Event equals vring_need_event (every Event and Old, four New)");
   end;

   --  Channel servicing: netstack's proved kick, close and second-look rules.
   declare
      use Channel_Service;
   begin
      Check (Kicks (True, 0, 5, 0, True) = 0, "channel: a failed channel asks for no kick");
      Check (Kicks (False, 0, 0, 9, True) = Kick_On_Send, "channel: kick on send once all was taken");
      Check (Kicks (False, 1, 0, 9, True) = 0, "channel: no send kick while data waits");
      Check (Kicks (False, 1, 5, 0, True) = Kick_On_Receive,
             "channel: kick on receive when data waits and the ring is full");
      Check (Kicks (False, 0, 5, 0, True) = (Kick_On_Send or Kick_On_Receive), "channel: both kicks");
      Check (Kicks (False, 1, 5, 1, True) = 0, "channel: no receive kick while the ring has space");
      Check (Kicks (False, 1, 5, 0, False) = 0, "channel: no receive kick without a connection");
      Check (Close_Due (False, 0, True, False), "channel: write-shutdown closes");
      Check (not Close_Due (True, 0, True, False), "channel: write-shutdown closes only once");
      Check (not Close_Due (False, 3, True, False), "channel: close waits for the client's data");
      Check (not Close_Due (False, 0, False, False), "channel: no close unless asked");
      Check (not Close_Due (False, 0, True, True), "channel: no close during the handshake");
      Check (not Look_Again (0, 1, 2, 3, 4), "channel: no second look without a kick request");
      Check (not Look_Again (Kick_On_Send, 7, 7, 1, 2), "channel: no second look when the client sent nothing");
      Check (Look_Again (Kick_On_Send, 7, 8, 1, 1), "channel: second look after the client sent");
      Check (Look_Again (Kick_On_Receive, 7, 7, 1, 2), "channel: second look after the client read");
      Check (not Look_Again (Kick_On_Receive, 7, 8, 1, 1), "channel: a send ignored when only a receive kick was asked");
   end;

   Put_Line (if Failures = 0 then "NET-TCP: PASS" else "NET-TCP: FAIL");
end Main;
