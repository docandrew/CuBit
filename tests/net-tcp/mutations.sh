#!/usr/bin/env bash
# Mutation check for the TCP proofs: each mutant is a plausible bug; gnatprove
# must fail to prove it (a mutant that still proves is a weak specification).
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
src=$here/../../userspace/net/src
# Work under build/, not /tmp: each mutant gets its own object directory,
# and a run of them once filled a size-limited /tmp.
mkdir -p "$here/build"
work=$(mktemp -d "$here/build/mutations.XXXXXX")
# The provers' scratch files go under build/ too (TMPDIR is /tmp otherwise).
export TMPDIR="$work/tmp"
mkdir -p "$TMPDIR"
trap 'rm -rf "$work"' EXIT
killed=0; total=0
mutant() {   # name file old new [unit to prove]
    total=$((total + 1))
    rm -rf "$work/src" && cp -r "$src" "$work/src"
    python3 - "$work/src/$2" "$3" "$4" <<'PY'
import sys
p, old, new = sys.argv[1:]
s = open(p).read()
assert s.count(old) == 1, (p, old)
open(p, 'w').write(s.replace(old, new))
PY
    [ $? -eq 0 ] || { echo "BADTEXT  $1"; return; }
    # DRY=1: only check that each mutant's text is found exactly once.
    if [ -n "${DRY:-}" ]; then echo "applies  $1"; return; fi
    # --checks-as-errors: any unproved check fails the run (nonzero exit).
    # Each mutant has its own object directory (no stale proof state).
    local out=$work/prove.out
    if (cd "$here" && gnatprove -P net_tcp_tests.gpr -XNET_SRC="$work/src" \
          -XNET_OBJ="$work/obj-$total" -j0 --level=1 \
          --checks-as-errors=on -u "${5:-$2}" >"$out" 2>&1); then
        echo "SURVIVED $1"
    elif grep -qE "(medium|high): " "$out"; then
        echo "killed   $1: $(grep -m1 -oE '(medium|high): [^[]*' "$out")"
        killed=$((killed + 1))
    else
        echo "ERROR    $1 (gnatprove failed without an unproved check):"
        tail -5 "$out"
    fi
    rm -rf "$work/obj-$total"
}
mutant "no-op (control: must survive)" tcp_rto.adb \
    "E.RTO := Unsigned_32'Min (Backoff_Factor * E.RTO, Maximum_RTO);" \
    "E.RTO := Unsigned_32'Min (E.RTO * Backoff_Factor, Maximum_RTO);"
mutant "skip measured the wrong way" tcp_acceptance.adb \
    "Skip   := Distance (Seg_Seq, Rcv_Nxt);" "Skip   := Distance (Rcv_Nxt, Seg_Seq);"
mutant "window room ignores the offset" tcp_acceptance.adb \
    "Remaining := Rcv_Wnd - Offset;" "Remaining := Rcv_Wnd;"
mutant "backoff can shrink the RTO" tcp_rto.adb \
    "E.RTO := Unsigned_32'Min (Backoff_Factor * E.RTO, Maximum_RTO);" \
    "E.RTO := Unsigned_32'Min (E.RTO / Backoff_Factor + Minimum_RTO, Maximum_RTO);"
mutant "RTTVAR weight overflows its range" tcp_rto.adb \
    "E.RTTVAR := ((Beta_Divisor - 1) * E.RTTVAR + Deviation) / Beta_Divisor;" \
    "E.RTTVAR := (Beta_Divisor - 1) * E.RTTVAR + Deviation;"
SQ=send_queue_64.ads
mutant "acknowledging data never sent" tcp_send_queue.adb \
    "if Gap <= Long_Long_Integer (Q.In_Flight) then" \
    "if Gap <= Long_Long_Integer (Q.Length) then" $SQ
mutant "segment read one byte off" tcp_send_queue.adb \
    "Data (K) := Q.Data ((Q.Head + Q.In_Flight + K - 1) mod Capacity);" \
    "Data (K) := Q.Data ((Q.Head + Q.In_Flight + K) mod Capacity);" $SQ
mutant "push writes one slot off" tcp_send_queue.adb \
    "Q.Data ((Q.Head + Q.Length + K) mod Capacity) := Data (Data'First + K);" \
    "Q.Data ((Q.Head + Q.Length + K + 1) mod Capacity) := Data (Data'First + K);" $SQ
mutant "rewind does not rewind" tcp_send_queue.adb \
    "Q.In_Flight := 0;" "Q.In_Flight := Q.In_Flight;" $SQ
TC=tcp_connection.adb
mutant "RST accepted anywhere in the window (blind reset)" tcp_connection.adb \
    "if S.Seq_No = C.Rcv_Nxt then" "if True then" $TC
mutant "SYN resets a synchronized connection (blind SYN)" tcp_connection.adb \
    "      if S.SYN then
         O.Answer := Send_Challenge_Ack;" "      if S.SYN then
         C.St := Closed;" $TC
mutant "out-of-order data delivered" tcp_connection.adb \
    "if First = C.Rcv_Nxt then" "if True then" $TC
mutant "acknowledgement beyond SND.NXT accepted" tcp_connection.adb \
    "(Distance (C.Snd_Una, Ack) in 1 .. Distance (C.Snd_Una, C.Snd_Nxt));" \
    "(Distance (C.Snd_Una, Ack) in 1 .. Distance (C.Snd_Una, C.Snd_Nxt) + 1);" $TC
mutant "illegal transition FIN-WAIT-2 -> CLOSE-WAIT" tcp_connection.adb \
    "when Fin_Wait_2 => C.St := Time_Wait;" "when Fin_Wait_2 => C.St := Close_Wait;" $TC
RQ=receive_queue_64.ads
mutant "second slice of wrapped text not marked present" tcp_receive_queue.adb \
    "         Q.Have (0 .. N2 - 1) := [others => True];" "         null;" $RQ
mutant "in-order text moves RCV.NXT one byte too far" tcp_receive_queue.adb \
    "         Q.Count := Offset + L;" "         Q.Count := Natural'Min (Offset + L + 1, Capacity);" $RQ
mutant "RCV.NXT jumps over a gap" tcp_receive_queue.adb \
    "exit when C >= Capacity or else not Q.Have (Slot (Q, C));" \
    "exit when C >= Capacity or else (not Q.Have (Slot (Q, C)) and then C = 0);" $RQ
mutant "insert writes one slot off" tcp_receive_queue.adb \
    "      Q.Data (P .. P + N1 - 1) := Raw (Data (1 .. N1));" \
    "      Q.Data (P .. P + N1 - 1) := Raw (Data (1 .. N1));
      if N1 > 1 then Q.Data (P) := Data (2); end if;" $RQ
mutant "read leaves consumed bits set" tcp_receive_queue.adb \
    "      Q.Have (0 .. N2 - 1) := [others => False];" "      null;" $RQ
mutant "read returns bytes one off" tcp_receive_queue.adb \
    "Output (N1 + 1 .. Got) := Second_Part (Q.Data (0 .. N2 - 1));" \
    "Output (N1 + 1 .. Got) := Second_Part (Q.Data (1 .. N2));" $RQ
mutant "read head does not wrap" tcp_receive_queue.adb \
    "Q.Head := (if Q.Head + Got < Capacity then Q.Head + Got else Q.Head + Got - Capacity);" \
    "Q.Head := (if Q.Head + Got < Capacity then Q.Head + Got else 0);" $RQ
CC=tcp_congestion.adb
mutant "fast retransmit on the second duplicate" tcp_congestion.adb \
    "elsif C.Dup_Acks = Duplicate_Threshold and then" \
    "elsif C.Dup_Acks = Duplicate_Threshold - 1 and then" $CC
mutant "no recover guard (fast retransmit for pre-loss data)" tcp_congestion.adb \
    "and then Ge (Snd_Una, C.Recover) then" "then" $CC
mutant "loss halves cwnd instead of FlightSize" tcp_congestion.adb \
    "C.Ssthresh := Loss_Threshold (C.SMSS, Flight);
         --  RFC 5681" \
    "C.Ssthresh := Loss_Threshold (C.SMSS, C.Cwnd);
         --  RFC 5681" $CC
mutant "repeated timeout lowers ssthresh again" tcp_congestion.adb \
    "      if First then
         C.Ssthresh" "      if True then
         C.Ssthresh" $CC
mutant "congestion avoidance grows on every ACK" tcp_congestion.adb \
    "if Total >= Old then" "if True then" $CC
mutant "slow start grows by the whole ACK" tcp_congestion.adb \
    "Old + Natural'Min (Acked, C.SMSS)" "Old + Acked" $CC
mutant "partial ACK ends recovery" tcp_congestion.adb \
    "            Action := Retransmit_First;" \
    "            Action := Retransmit_First;
            C.In_Recovery := False;" $CC
mutant "partial ACK always adds an SMSS back" tcp_congestion.adb \
    "(if Acked >= C.SMSS then C.SMSS else 0)" "C.SMSS" $CC
mutant "full ACK keeps the inflated window" tcp_congestion.adb \
    "C.Cwnd := Natural'Min
              (C.Ssthresh, Natural'Max (Flight, C.SMSS) + C.SMSS);" "null;" $CC
mutant "recover stops following ACKs (breaks after 2 GiB)" tcp_congestion.adb \
    "if Ge (Ack, C.Recover) then
            C.Recover := Ack;" "if False then
            C.Recover := Ack;" $CC
mutant "allowance ignores the peer's window" tcp_congestion.ads \
    "is (if Flight >= Natural'Min (C.Cwnd, Peer_Window) then 0
       else Natural'Min (C.Cwnd, Peer_Window) - Flight)" \
    "is (if Flight >= C.Cwnd then 0 else C.Cwnd - Flight)" $CC
OP=tcp_options.adb
mutant "window scaling on our offer alone" tcp_options.adb \
    "A.Scaling := Ours.Window_Scale and then Peer.Has_Window_Scale;" \
    "A.Scaling := Ours.Window_Scale;" $OP
mutant "peer shift of 15 accepted" tcp_options.adb \
    "if Peer.Shift_Count > Maximum_Shift then" \
    "if Peer.Shift_Count > Maximum_Shift + 1 then" $OP
mutant "SACK without the peer's permission" tcp_options.adb \
    "A.SACK := Ours.SACK_Permitted and then Peer.SACK_Permitted;" \
    "A.SACK := Ours.SACK_Permitted;" $OP
mutant "MSS ignores the timestamp option" tcp_options.adb \
    "else Limit - Overhead);" "else Limit);" $OP
mutant "no MSS floor" tcp_options.adb \
    "if Limit <= Minimum_MSS + Overhead then Minimum_MSS" \
    "if Limit < Overhead then Minimum_MSS" $OP
mutant "advertised window rounds up" tcp_options.ads \
    "(Unsigned_16 (Unsigned_32'Min (Shift_Right (Space, S), Maximum_Window_Field)))" \
    "(Unsigned_16 (Unsigned_32'Min (Shift_Right (Space + Shift_Left (1, S) - 1, S), Maximum_Window_Field)))" $OP
TS=tcp_timestamps.adb
mutant "PAWS refuses a RST" tcp_timestamps.adb \
    "if not RST and then Trusted and then" "if Trusted and then" $TS
mutant "TS.Recent never expires" tcp_timestamps.adb \
    "Trusted : constant Boolean := Fresh (S, Now);" \
    "Trusted : constant Boolean := S.Recent_Valid;" $TS
mutant "TS.Recent from a segment past Last.ACK.sent" tcp_timestamps.adb \
    "if Le (Seg_Seq, Last_Ack_Sent) and then" "if True and then" $TS
mutant "TS.Recent moves backwards" tcp_timestamps.adb \
    "(not S.Recent_Valid or else Ge (TSval, S.Recent))" "True" $TS
SB=scoreboard_8.ads
mutant "merge drops the tail of the last range" tcp_scoreboard.adb \
    "Stop  => Offset'Max (Stop, B.R (H).Stop));" "Stop  => Stop);" $SB
mutant "a range ending at the block is not merged" tcp_scoreboard.adb \
    "while L < B.N and then B.R (L + 1).Stop < First loop" \
    "while L < B.N and then B.R (L + 1).Stop <= First loop" $SB
mutant "a range starting at the block's end is not merged" tcp_scoreboard.adb \
    "while H < B.N and then B.R (H + 1).First <= Stop loop" \
    "while H < B.N and then B.R (H + 1).First < Stop loop" $SB
mutant "insert into a full board" tcp_scoreboard.adb \
    "         if B.N < Max_Ranges then
            Insert_At" "         if True then
            Insert_At" $SB
mutant "advance keeps a range ending at the new SND.UNA" tcp_scoreboard.adb \
    "while L < B.N and then B.R (L + 1).Stop <= By loop" \
    "while L < B.N and then B.R (L + 1).Stop < By loop" $SB
mutant "advance does not shift" tcp_scoreboard.adb \
    "         B.R (K) := Shifted (B.R (K), By);" "         null;" $SB
mutant "next unSACKed returns the range start" tcp_scoreboard.adb \
    "return B.R (I).Stop;" "return B.R (I).First;" $SB
SC=tcp_syn_cookies.adb
mutant "cookie counter overlaps the MSS bits" tcp_syn_cookies.ads \
    "Shift_Left (Counter (Now_Seconds), Counter_Shift)" \
    "Shift_Left (Counter (Now_Seconds), Counter_Shift - 1)" $SC
mutant "cookie lives three periods" tcp_syn_cookies.ads \
    "mod Counter_Steps <= Periods_Valid" "mod Counter_Steps <= Periods_Valid + 1" $SC
mutant "cookie MSS above the client's" tcp_syn_cookies.adb \
    "if MSS_Table (I) <= MSS then" "if MSS_Table (I) <= MSS + 100 then" $SC
PL=pool_small.ads
mutant "release does not clear the chunk" chunk_pool.adb \
    "P.Store (C) := [others => 0];" "null;" $PL
mutant "allocate does not charge the owner" chunk_pool.adb \
    "P.Counter (O) := P.Counter (O) + 1;" "null;" $PL
mutant "allocated chunk stays on the free stack" chunk_pool.adb \
    "P.Top := P.Top - 1;" "null;" $PL
mutant "release does not record the stack position" chunk_pool.adb \
    "P.Pos (C) := P.Top;" "null;" $PL
mutant "write lands in another chunk" chunk_pool.adb \
    "P.Store (C) (I) := V;" "P.Store (C mod Chunk_Count + 1) (I) := V;" $PL
mutant "slice write starts one byte late" chunk_pool.adb \
    "P.Store (C) (At_Index .. At_Index + Bytes'Length - 1) := Bytes;" \
    "if Bytes'Length > 0 and then At_Index + Bytes'Length <= Chunk_Size then
         P.Store (C) (At_Index + 1 .. At_Index + Bytes'Length) := Bytes;
      end if;" $PL
mutant "slice read starts one byte late" chunk_pool.adb \
    "Into := P.Store (C) (At_Index .. At_Index + Into'Length - 1);" \
    "Into := (if Into'Length > 0 and then At_Index + Into'Length <= Chunk_Size
              then P.Store (C) (At_Index + 1 .. At_Index + Into'Length)
              else P.Store (C) (At_Index .. At_Index + Into'Length - 1));" $PL
CT=table_small.ads
mutant "remove keeps the generation (stale handles live)" connection_table.adb \
    "T.Gen (S) := T.Gen (S) + 1;" "null;" $CT
mutant "find scans the wrong bucket" connection_table.adb \
    "B : constant Bucket_Id := Bucket_Of (T.Secret, E);" \
    "B : constant Bucket_Id := (Bucket_Of (T.Secret, E) + 1) mod Bucket_Count;" $CT
mutant "insert skips the duplicate check" connection_table.adb \
    "if Find (T, E) /= No_Slot then" "if False then" $CT
mutant "remove leaves its bucket entry" connection_table.adb \
    "T.Buckets (T.Home (S)) (T.Place (S)) := No_Slot;" "null;" $CT
mutant "insert overwrites an occupied bucket entry" connection_table.adb \
    "if T.Buckets (B) (Q) = No_Slot then" "if True then" $CT
TH=timers_small.ads
mutant "sift-up stops at a later parent" timer_heap.adb \
    "exit when K = 1 or else Key (H, K / 2) <= Key (H, K);" \
    "exit when K <= 2 or else Key (H, K / 2) <= Key (H, K);" $TH
mutant "sift-down picks the later child" timer_heap.adb \
    "if M + 1 <= H.Size and then Key (H, M + 1) < Key (H, M) then" \
    "if M + 1 <= H.Size and then Key (H, M + 1) > Key (H, M) then" $TH
mutant "re-arming does not move the timer" timer_heap.adb \
    "         if Earlier then
            pragma Assert (Up_OK (H, K));
            Sift_Up (H, K);" \
    "         if Earlier then
            null;" $TH
mutant "cancel leaves the timer armed" timer_heap.adb \
    "      H.Pos (T) := Not_Armed;" "      null;" $TH
mutant "swap forgets a back-pointer" timer_heap.adb \
    "      H.Pos (B) := I;" "      null;" $TH
SC2=send_chunked_small.ads
mutant "take reads from SND.UNA instead of SND.NXT" chunked_send_queue.adb \
    "Read_Slice_At (Q, P, Q.In_Flight + Done," "Read_Slice_At (Q, P, Done," $SC2
mutant "ACK releases a chunk still in use" chunked_send_queue.adb \
    "Drop := New_Off / Chunk_Bytes;" "Drop := (New_Off + Chunk_Bytes - 1) / Chunk_Bytes;" $SC2
mutant "released chunk not returned to the pool" chunked_send_queue.adb \
    "         Chunks.Release (P, Q.Me, Q.Ids (J));" "         null;" $SC2
mutant "grow reuses a held chunk" chunked_send_queue.adb \
    "Q.Ids (Q.Held) := C;" "Q.Ids (Q.Held) := Q.Ids (0);" $SC2
mutant "ACK does not advance SND.UNA" chunked_send_queue.adb \
    "      Q.Start := Ack;" "      null;" $SC2
EP=endpoint_small.ads
mutant "queue ignores accepted ACKs" tcp_endpoint.adb \
    "      Sends.Acknowledge (S, P, A, R, Freed);" "      null;" $EP
mutant "an acknowledged FIN is freed as data" tcp_endpoint.adb \
    "A := (if C.Fin_Sent and then C.Snd_Una = C.Snd_Nxt then C.Snd_Una - 1" \
    "A := (if False then C.Snd_Una - 1" $EP
mutant "FIN sent before the queued data" tcp_endpoint.adb \
    "if E.C.Fin_Pending and then Sends.Sent (E.S) = Sends.Count (E.S) then" \
    "if E.C.Fin_Pending then" $EP
mutant "timeout keeps the retransmission point" tcp_endpoint.adb \
    "      if E.C.St in Established .. Time_Wait then
         E.Rtx := E.C.Snd_Una;" "      if E.C.St in Established .. Time_Wait then
         null;" $EP
mutant "resend runs past SND.NXT" tcp_endpoint.adb \
    "            E.Rtx := E.Rtx + Seq (Taken);" "            E.Rtx := E.Rtx + Seq (Taken) + 1;" $EP
mutant "a reset keeps its chunks" tcp_endpoint.adb \
    "--  Reset: the connection's chunks go back to the pool.
         Sends.Release_All (E.S, P);" "--  Reset: the connection's chunks go back to the pool.
         null;" $EP
mutant "passive open starts data at the ISS" tcp_endpoint.adb \
    "Sends.Initialize (E.S, P, Me, E.C.Snd_Una + 1);" "Sends.Initialize (E.S, P, Me, E.C.Snd_Una);" $EP
FL=flow_small.ads
mutant "segment larger than the MSS" tcp_flow.adb \
    "Natural'Min (Natural'Min (Allow, Data'Length), F.CC.SMSS);" "Natural'Min (Allow, Data'Length);" $FL
mutant "retransmission timer not armed for new data" tcp_flow.adb \
    "      if In_Flight (F) and then not F.Armed then
         F.Armed := True;" "      if In_Flight (F) and then not F.Armed then
         F.Armed := False;" $FL
mutant "an ACK for data stops the timer covering our FIN" tcp_flow.adb \
    "         if In_Flight (F) then
            F.Armed := True;" "         if Outstanding (F) > 0 then
            F.Armed := True;" $FL
mutant "a timeout does not restart the timer" tcp_flow.adb \
    "      F.Retries := Natural'Min (F.Retries + 1, Maximum_Retries);
      F.Armed := True;" "      F.Retries := Natural'Min (F.Retries + 1, Maximum_Retries);
      F.Armed := False;" $FL
mutant "progress does not reset the retry count" tcp_flow.adb \
    "         F.Retries := 0;
" "" $FL
mutant "a passive open's SYN-ACK is not timed" tcp_flow.adb \
    "         if F.E.C.St = Syn_Received and then not F.Armed then" \
    "         if False then" $FL
mutant "parse reads the window from the checksum bytes" tcp_header.adb \
    "            Window         => U16 (B, 14)," "            Window         => U16 (B, 16),"
mutant "writer drops the FIN flag" tcp_header.adb \
    "Flag (H.SYN, 1) or Flag (H.FIN, 0);" "Flag (H.SYN, 1);"
mutant "writer puts the data offset in the wrong nibble" tcp_header.adb \
    "B (12) := Shift_Left (Unsigned_8 (H.Size / 4), 4) or Flag (H.NS, 0);" \
    "B (12) := Unsigned_8 (H.Size / 4) or Flag (H.NS, 0);"
mutant "writer swaps the sequence and acknowledgement numbers" tcp_header.adb \
    "      Put32 (B, 4, H.Seq_No);
      Put32 (B, 8, H.Ack_No);" "      Put32 (B, 4, H.Ack_No);
      Put32 (B, 8, H.Seq_No);"
mutant "MSS option with the wrong length" tcp_header.adb \
    "      B (21) := 4;" "      B (21) := 3;"
mutant "ARP accepts an unsolicited reply" arp_cache.adb \
    "            if Pos >= 0 and then T (Pos).St = Pending then
               T (Pos) := (St => Resolved, IP => P.Sender_IP, HW => P.Sender_HW, Since => Now);
            end if;
         when Request =>" "            if Pos >= 0 then
               T (Pos) := (St => Resolved, IP => P.Sender_IP, HW => P.Sender_HW, Since => Now);
            end if;
         when Request =>"
mutant "ARP lets a request rewrite a resolved address" arp_cache.adb \
    "            elsif T (Pos).HW = P.Sender_HW then
               T (Pos).Since := Now;" "            else
               T (Pos).HW := P.Sender_HW;"
mutant "ARP learns from a request for someone else" arp_cache.adb \
    "            if not Ours then
               return;
            end if;" "            null;"
mutant "IPv4 protocol read from the TTL byte" ipv4_header.adb \
    "            Protocol     => B (9)," "            Protocol     => B (8),"
mutant "ARP sender address read one byte off" arp_packet.adb \
    "            Sender_IP => [B (14), B (15), B (16), B (17)]," \
    "            Sender_IP => [B (15), B (15), B (16), B (17)],"
DP=descriptors_80.ads
mutant "device may return a free descriptor" descriptor_pool.adb \
    "OK := Raw < Unsigned_32 (Count) and then P.In_Flight (Natural (Raw)) and then" \
    "OK := Raw < Unsigned_32 (Count) and then" $DP
mutant "device id not range-checked" descriptor_pool.adb \
    "OK := Raw < Unsigned_32 (Count) and then P.In_Flight (Natural (Raw)) and then" \
    "OK := P.In_Flight (Natural (Raw mod Unsigned_32 (Count))) and then" $DP
mutant "taken descriptor not marked in flight" descriptor_pool.adb \
    "      P.In_Flight (D) := True;" "      null;" $DP
mutant "free list may overflow" descriptor_pool.adb \
    "            P.Top < Count;" "            True;" $DP
mutant "returned descriptor stays in flight" descriptor_pool.adb \
    "         P.In_Flight (Natural (Raw)) := False;" "         null;" $DP
mutant "frame slot may be 0 (the counts slot)" frame_ring.ads \
    "     (Natural (Long_Long_Integer (N) mod Long_Long_Integer (Slots)) + 1)" \
    "     (Natural (Long_Long_Integer (N) mod Long_Long_Integer (Slots)))" frame_ring.ads
mutant "arena buffer claimed while held" channel_arenas.adb \
    "      if Item.Entries (Index).Used (Slot) then
         return;" "      if False then
         return;" channel_arenas.adb
mutant "arena buffer index not bounded by the count" channel_arenas.adb \
    "      if Slot >= Item.Entries (Index).Count then" \
    "      if Slot > Item.Entries (Index).Count then" channel_arenas.adb
mutant "arena may span more than one grant" channel_arenas.ads \
    "     (Count in 1 .. Maximum_Span / Size);" \
    "     (Count in 1 .. Maximum_Span / Size + 1);" channel_arenas.adb
mutant "another owner's arena found" channel_arenas.adb \
    "         if Item.Entries (I).Id = Arena and then Item.Entries (I).Owner = Owner" \
    "         if Item.Entries (I).Id = Arena" channel_arenas.adb
mutant "released buffer stays held" channel_arenas.adb \
    "         Item.Entries (Index).Used (Slot) := False;" \
    "         null;" channel_arenas.adb
mutant "arena unregistered while buffers are held" channel_arenas.adb \
    "      if Found and then Idle (Item, Index) then" \
    "      if Found then" channel_arenas.adb
mutant "used index trusted beyond what the device holds" virtqueue_index.ads \
    "   is (if Distance (Last, Device) <= Outstanding then Distance (Last, Device) else 0)" \
    "   is (Distance (Last, Device))" virtqueue_index.ads
mutant "used slot not reduced to the queue" virtqueue_index.ads \
    "   function Slot (I : Index) return Natural is (Natural (I mod Queue_Size))" \
    "   function Slot (I : Index) return Natural is (Natural (I))" virtqueue_index.ads
mutant "ipv6 source may be IPv4-mapped" ipv6_header.ads \
    "     (not Is_Mapped (A) and then not Is_Multicast (A));" \
    "     (not Is_Multicast (A));" ipv6_header.adb
mutant "ipv6 destination may be IPv4-mapped" ipv6_header.ads \
    "     (not Is_Mapped (A) and then not Is_Unspecified (A));" \
    "     (not Is_Unspecified (A));" ipv6_header.adb
mutant "ipv6 payload past the bytes received" ipv6_header.ads \
    "      Size + Natural (U16 (B, 4)) <= B'Length and then" \
    "      Natural (U16 (B, 4)) <= B'Length and then" ipv6_header.adb
mutant "nd hop limit not checked" nd_message.adb \
    "      if Hop_Limit /= Required_Hop_Limit or else B'Length < Fixed_Size or else" \
    "      if B'Length < Fixed_Size or else" nd_message.adb
mutant "nd zero-length option walked" nd_message.adb \
    "         if B'Length - P < 2 or else B (P + 1) = 0 then" \
    "         if B'Length - P < 2 then" nd_message.adb
mutant "nd unsolicited advertisement learned" neighbor_cache.adb \
    "            if E.Solicited and then Pos >= 0 and then T (Pos).St = Pending then" \
    "            if Pos >= 0 and then T (Pos).St = Pending then" neighbor_cache.adb
mutant "nd mapped sender learned" neighbor_cache.ads \
    "not Is_Multicast (E.Peer) and then not Is_Mapped (E.Peer) and then" \
    "not Is_Multicast (E.Peer) and then" neighbor_cache.adb
mutant "nd resolved link rewritten" neighbor_cache.adb \
    "            elsif T (Pos).Link = E.Link then
               T (Pos).Since := Now;" "            else
               T (Pos).Link := E.Link;" neighbor_cache.adb
mutant "slaac two-hour rule dropped" slaac_table.adb \
    "            T (Pos).Valid_Until := Now + Two_Hours;" \
    "            T (Pos).Valid_Until := New_Valid;" slaac_table.adb
mutant "slaac duplicate revived" slaac_table.adb \
    "         elsif T (I).St = Tentative and then T (I).DAD_Until <= Now then" \
    "         elsif T (I).St in Tentative | Duplicate and then T (I).DAD_Until <= Now then" slaac_table.adb
mutant "slaac detection not awaited" slaac_table.adb \
    "         elsif T (I).St = Tentative and then T (I).DAD_Until <= Now then" \
    "         elsif T (I).St = Tentative then" slaac_table.adb
mutant "ra hop limit not checked" ra_message.adb \
    "      if Hop_Limit /= Required_Hop_Limit or else not Is_Link_Local (Source) or else" \
    "      if not Is_Link_Local (Source) or else" ra_message.adb
mutant "ra link-local prefix used" ra_message.ads \
    "     (not Is_Link_Local (P.Network) and then" "     (True and then" ra_message.adb
mutant "dns data length unchecked" dns_response.adb \
    "         if Data > Message'Length - Off - Answer_Fixed then" \
    "         if False then" dns_response.adb
mutant "dns address one byte late" dns_response.adb \
    "            R.Address_At := Off + Answer_Fixed;" \
    "            R.Address_At := Off + Answer_Fixed + 1;" dns_response.adb
mutant "dns answer type not checked" dns_response.adb \
    "         if U16 (Message, Off) = Type_A and then U16 (Message, Off + 2) = Class_IN and then" \
    "         if U16 (Message, Off + 2) = Class_IN and then" dns_response.adb
mutant "dns pointer runs off the end" dns_response.adb \
    "            if Pos >= Message'Length then
               return;
            end if;
            Pos := Pos + 1;" "            Pos := Pos + 1;" dns_response.adb
mutant "ipv4 build writes source for destination" ipv4_header.adb \
    "         B (16 + K) := H.Destination (K);" \
    "         B (16 + K) := H.Source (K);" ipv4_header.adb
mutant "ipv4 build sets MF" ipv4_header.adb \
    "      Flags : constant Unsigned_16 := (if DF then Dont_Fragment else 0);" \
    "      Flags : constant Unsigned_16 := (if DF then Dont_Fragment else 16#2000#);" ipv4_header.adb
mutant "icmp quote bound off by the transport" icmpv4_error.adb \
    "        Quoted_At + Size + Quoted_Transport > Message'Length" \
    "        Quoted_At + Size > Message'Length" icmpv4_error.adb
mutant "icmp port unreachable treated as soft" icmpv4_error.ads \
    "        Code in Protocol_Unreachable | Port_Unreachable | Network_Prohibited |" \
    "        Code in Protocol_Unreachable | Network_Prohibited |" icmpv4_error.adb
mutant "pmtu reduction grows the window" tcp_congestion.adb \
    "      C.SMSS := SMSS;
   end Reduce_Segment_Size;" "      C.SMSS := SMSS;
      C.Cwnd := C.Cwnd + SMSS;
   end Reduce_Segment_Size;" tcp_congestion.adb
mutant "rst answers a reset" tcp_reset.adb \
    "      if A.RST then
         return (others => <>);
      elsif A.ACK then" "      if A.ACK then" tcp_reset.adb
mutant "rst ignores FIN in SEG.LEN" tcp_reset.ads \
    "(if A.SYN then 1 else 0) + (if A.FIN then 1 else 0));" "(if A.SYN then 1 else 0));" tcp_reset.adb
mutant "window updated only by new data" tcp_connection.adb \
    "      if Updates_Window (C, S) then" \
    "      if Updates_Window (C, S) and then C.Snd_Una /= C.Snd_Wl2 then" tcp_connection.adb
mutant "older segment shrinks the window" tcp_connection.ads \
    "       (C.Snd_Wl1 = S.Seq_No and then Le (C.Snd_Wl2, S.Ack_No))));" \
    "       True));" tcp_connection.adb
mutant "persist never gives up" tcp_flow.adb \
    "      Give_Up := F.Probes >= Maximum_Probes;" "      Give_Up := False;" flow_small.ads
echo "mutants killed: $killed/$((total - 1)) (plus one control that must survive)"
