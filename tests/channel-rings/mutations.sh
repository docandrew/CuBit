#!/usr/bin/env bash
# Mutation check for CuBit.Channel_Rings: each mutant is a plausible bug;
# gnatprove must fail to prove it (a mutant that still proves is a weak
# specification). Run inside the Nix shell from anywhere.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
runtime=$here/../../userspace/runtime/gnat
mkdir -p "$here/build"
work=$(mktemp -d "$here/build/mutations.XXXXXX")
export TMPDIR="$work/tmp"
mkdir -p "$TMPDIR"
trap 'rm -rf "$work"' EXIT
killed=0; total=0
mutant() {   # name file old new [unit to prove]
    total=$((total + 1))
    rm -rf "$work/src" && mkdir -p "$work/src"
    cp "$runtime"/cubit.ads "$runtime"/cubit-channel_rings.ad[sb] "$runtime"/cubit-datagram_rings.ad[sb] "$runtime"/cubit-slot_rings.ad[sb] "$runtime"/cubit-frame_rings.ads "$runtime"/cubit-submission_queues.ad[sb] "$work/src"
    python3 - "$work/src/$2" "$3" "$4" <<'PY'
import sys
p, old, new = sys.argv[1:]
s = open(p).read()
assert s.count(old) == 1, (p, old)
open(p, 'w').write(s.replace(old, new))
PY
    [ $? -eq 0 ] || { echo "BADTEXT  $1"; return; }
    if [ -n "${DRY:-}" ]; then echo "applies  $1"; return; fi
    local out=$work/prove.out
    if (cd "$here" && gnatprove -P rings.gpr -XRING_SRC="$work/src" \
          -XRING_OBJ="$work/obj-$total" -j0 --level=1 \
          --checks-as-errors=on -u "${5:-cubit-channel_rings.adb}" >"$out" 2>&1); then
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
B=cubit-channel_rings.adb
mutant "no-op (control: must survive)" $B \
    "      P.Fill := P.Fill + N;" "      P.Fill := N + P.Fill;"
mutant "position ignores the size" $B \
    "(Natural (Value and Index (Size - 1)));" "(Natural (Value and Index (Size)));"
mutant "peer may release unproduced bytes" $B \
    "OK := Freed <= Count (P.Fill);" "OK := Freed <= Count (P.Size);"
mutant "release not subtracted" $B \
    "         P.Fill := P.Fill - Natural (Freed);" "         null;"
mutant "peer may overfill the ring" $B \
    "OK := Ahead <= Count (C.Size) and then Ahead >= Count (C.Available);" \
    "OK := Ahead >= Count (C.Available);"
mutant "peer may take back produced bytes" $B \
    "OK := Ahead <= Count (C.Size) and then Ahead >= Count (C.Available);" \
    "OK := Ahead <= Count (C.Size);"
mutant "commit forgets the fill" $B \
    "      P.Fill := P.Fill + N;" "      null;"
mutant "consume forgets the index" $B \
    "      C.Consumed := C.Consumed + Index (N);" "      null;"
mutant "free space runs past the end" $B \
    "First_Length := Natural'Min (Free, P.Size - First);" "First_Length := Free;"
mutant "readable bytes run past the end" $B \
    "First_Length := Natural'Min (Ready, C.Size - First);" "First_Length := Ready;"
mutant "write overfills" $B \
    "Written := Natural'Min (Data'Length, First_Length + Second_Length);" \
    "Written := Data'Length;"
mutant "read past the data" $B \
    "Copied := Natural'Min (Data'Length, First_Length + Second_Length);" \
    "Copied := Data'Length;"
mutant "second slice not at the start" $B \
    "         Ring (0 .. N2 - 1) := Data (D + N1" "         Ring (First .. First + N2 - 1) := Data (D + N1"
mutant "read commits less than copied" $B \
    "      Consume (C, Copied);" "      Consume (C, N1);"
G=cubit-datagram_rings.adb
mutant "record may run past the ring's end" $G \
    "      if L1 >= Needed then" "      if L1 >= Header_Bytes then" $G
mutant "pad without room for the record" $G \
    "        L2 >= Needed and then L1 >= Header_Bytes" "        L1 >= Header_Bytes" $G
mutant "record length taken on trust" $G \
    "            if Needed > L1 then" "            if Needed > C.Size then" $G
mutant "take ignores the caller's buffer" $G \
    "            Length := Natural'Min (Header_Length, Into'Length);" \
    "            Length := Header_Length;" $G
S=cubit-slot_rings.adb
SS=cubit-slot_rings.ads
mutant "slot ring: slot ignores the mask" $SS \
    "   function Slot_Of (I : Index) return Slot is (Slot (I and Mask));" \
    "   function Slot_Of (I : Index) return Slot is (Slot (I mod Index (Slots - 1)));" slot_ring_small.ads
mutant "slot ring: peer may release unpushed elements" $S \
    "      OK := Freed <= Count (P.Fill);" "      OK := Freed <= Count (Slots);" slot_ring_small.ads
mutant "slot ring: release not subtracted" $S \
    "         P.Fill := P.Fill - Natural (Freed);" "         null;" slot_ring_small.ads
mutant "slot ring: peer may overfill the ring" $S \
    "      OK := Ahead <= Count (Slots) and then Ahead >= Count (C.Available);" \
    "      OK := Ahead >= Count (C.Available);" slot_ring_small.ads
mutant "slot ring: peer may take back pushed elements" $S \
    "      OK := Ahead <= Count (Slots) and then Ahead >= Count (C.Available);" \
    "      OK := Ahead <= Count (Slots);" slot_ring_small.ads
mutant "slot ring: push writes the wrong slot" $S \
    "      R (Next_Slot (P)) := E;" "      R (Slot_Of (P.Produced + 1)) := E;" slot_ring_small.ads
mutant "slot ring: commit forgets the fill" $S \
    "      P.Fill := P.Fill + 1;" "      null;" slot_ring_small.ads
mutant "slot ring: take reads the wrong slot" $S \
    "      E := R (Head_Slot (C));" "      E := R (Slot_Of (C.Consumed + Index (C.Available) - 1));" slot_ring_small.ads
mutant "slot ring: release forgets the index" $S \
    "      C.Consumed := C.Consumed + 1;" "      null;" slot_ring_small.ads
Q=cubit-submission_queues.adb
QS=cubit-submission_queues.ads
mutant "queue: service takes without a completion slot" $QS \
    "      S.Owed < Completions.Space (S.Answers));" \
    "      S.Owed <= Completions.Space (S.Answers));" queue_small.ads
mutant "queue: take forgets the answer owed" $Q \
    "      S.Owed := S.Owed + 1;" "      null;" queue_small.ads
mutant "queue: complete forgets it answered" $Q \
    "      S.Owed := S.Owed - 1;" "      null;" queue_small.ads
mutant "queue: client may exceed the completion slots" $QS \
    "      C.Pending < Completion_Slots);" "      True);" queue_small.ads
mutant "queue: unsolicited answer counted" $Q \
    "      OK := C.Pending > 0;
      if OK then" "      OK := True;
      if C.Pending > 0 then" queue_small.ads
mutant "queue: answer loses its token" $Q \
    "      Completions.Push (S.Answers, Ring, (Tag => Tag, Answer => Answer));" \
    "      Completions.Push (S.Answers, Ring, (Tag => 0, Answer => Answer));" queue_small.ads
echo "$killed of $((total - 1)) mutants killed (plus the control)."
