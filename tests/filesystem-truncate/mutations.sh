#!/usr/bin/env bash
# Mutation check for the ext2 block map, block cache, JBD2 journal and
# namespace code. Each mutant is a plausible bug in a private copy of the
# filesystem sources. Kinds:
#   prove          SPARK unit: must fail the level-1 proof.
#   test           Ext2: must fail a hosted production-path test here.
#   journal        Ext2 journal/namespace: must fail tests/filesystem-journal
#                  (replay vs e2fsck, power cuts, namespace ops, pressure).
#   prove+test     the proof may survive (the contract is self-referential,
#   prove+journal  e.g. the mutated function also defines the property);
#                  then the tests must kill it.
# A surviving mutant marks a weak specification or test. Run in the Nix
# shell. JOURNAL=0 skips the (slow) journal mutants; ONLY=text selects.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
src=$here/../../userspace/services/filesystem
journal=$here/../filesystem-journal
mkdir -p "$here/build"
work=$(mktemp -d "$here/build/mutations.XXXXXX")
export TMPDIR="$work/tmp"
mkdir -p "$TMPDIR"
trap 'rm -rf "$work"' EXIT
killed=0; total=0; skipped=0
TESTS="triple_resize sector_counts indirect_reads path_decoding path_reads double_resize main resizing batched_io object_admission"
mutant() {   # kind name file old new
    # ONLY=text runs just the mutants whose name contains text.
    case "$2" in *"${ONLY:-}"*) ;; *) return ;; esac
    total=$((total + 1))
    rm -rf "$work/src" && cp -r "$src" "$work/src" && rm -rf "$work/src/build"
    python3 - "$work/src/$3" "$4" "$5" <<'PY'
import sys
p, old, new = sys.argv[1:]
s = open(p).read()
assert s.count(old) == 1, (p, old)
open(p, 'w').write(s.replace(old, new))
PY
    [ $? -eq 0 ] || { echo "BADTEXT  $2"; return; }
    if [ -n "${DRY:-}" ]; then echo "applies  $2"; return; fi
    case "$1" in
        journal|prove+journal)
            if [ "${JOURNAL:-1}" = 0 ]; then
                echo "skipped  $2"; skipped=$((skipped + 1)); return
            fi ;;
    esac
    local out=$work/out obj=$work/obj-$total
    case "$1" in
        prove|prove+test|prove+journal)
            if (cd "$here" && gnatprove -P accounting_proof.gpr -XFS_SRC="$work/src" \
                  -XFS_OBJ="$obj" -j0 --level=1 --timeout=5 --checks-as-errors=on \
                  -u "${3%.ad?}.adb" >"$out" 2>&1); then
                case "$1" in
                    prove) echo "SURVIVED $2" ;;
                    prove+test) run_tests "$2 (proof survives)" ;;
                    prove+journal) run_journal "$2 (proof survives)" ;;
                esac
            elif grep -qE "(medium|high): " "$out"; then
                echo "killed   $2: $(grep -m1 -oE '(medium|high): [^[]*' "$out")"
                killed=$((killed + 1))
            else
                echo "ERROR    $2 (gnatprove failed without an unproved check):"
                tail -5 "$out"
            fi ;;
        test) run_tests "$2" ;;
        journal) run_journal "$2" ;;
    esac
    rm -rf "$obj"
}
run_tests() {   # label; builds and runs the hosted tests on $work/src
    local out=$work/out obj=$work/obj-$total failed=""
    if ! (cd "$here" && gprbuild -q -p -P truncate_tests.gpr -XFS_SRC="$work/src" \
            -XFS_OBJ="$obj" >"$out" 2>&1); then
        echo "ERROR    $1 (did not compile):"; tail -5 "$out"; return
    fi
    for t in $TESTS; do
        if ! timeout 300 "$obj/$t" >"$out" 2>&1; then failed=$t; break; fi
    done
    if [ -n "$failed" ]; then
        echo "killed   $1: $failed: $(grep -m1 -E 'raised|ASSERT' "$out")"
        killed=$((killed + 1))
    else
        echo "SURVIVED $1"
    fi
}
run_journal() {   # label; the journal suites (sampled) on $work/src
    local out=$work/out obj=$work/obj-$total failed=""
    if ! (cd "$journal" && gprbuild -q -p -P crash.gpr -XFS_SRC="$work/src" \
            -XFS_OBJ="$obj/crash" >"$out" 2>&1 &&
          gprbuild -q -p -P journal.gpr -XFS_SRC="$work/src" \
            -XFS_OBJ="$obj/journal" >"$out" 2>&1); then
        echo "ERROR    $1 (did not compile):"; tail -5 "$out"; return
    fi
    export JOURNAL_REPLAY=$obj/journal/replay JOURNAL_WORKLOAD=$obj/crash/workload
    export NAMESPACE_WORKLOAD=$obj/crash/namespace PRESSURE_WORKLOAD=$obj/crash/pressure
    export IO_COUNTS=$obj/crash/io_counts
    for suite in "run.py" "crash.py --stride 9" "namespace.py --stride 29" \
                 "pressure.py --cuts 12" "io_counts.py"; do
        # shellcheck disable=SC2086
        if ! (cd "$journal" && timeout 1200 python3 $suite >"$out" 2>&1); then
            failed=${suite%% *}; break
        fi
    done
    if [ -n "$failed" ]; then
        echo "killed   $1: $failed: $(grep -m1 -E 'Error|raised|FAIL' "$out" | cut -c1-150)"
        killed=$((killed + 1))
    else
        echo "SURVIVED $1"
    fi
}

# --- Block paths and mappings (SPARK) ---------------------------------------
P=block_paths.adb
mutant prove "triple middle slot unreduced" $P \
    "Middle_Slot => Within / 1024 mod 1024," "Middle_Slot => Within mod 1024,"
mutant prove "triple top slot one level short" $P \
    "Top_Slot => Within / 256 / 256," "Top_Slot => Within / 256,"
mutant prove "double leaf slot unreduced" $P \
    "Root_Slot => Within / 512, Leaf_Slot => Within mod 512);" \
    "Root_Slot => Within / 512, Leaf_Slot => Within mod 1024);"
mutant prove+test "triple extent too short" block_paths.ads \
    "(First_Triple (Sectors) + Middle_Span (Sectors) * Pointer_Count (Sectors));" \
    "(First_Triple (Sectors) + Middle_Span (Sectors));"
T=triple_mappings.adb
mutant prove+test "new root not counted" triple_mappings.ads \
    "     (1 + (if Current.tripleIndirectBlock = 0 then 1 else 0) +" "     (1 + 0 +"
mutant prove "root not published" $T \
    "      Updated.tripleIndirectBlock := Root;" "      null;"
mutant prove "accounting overflow ignored" $T \
    "      if not Count_Fits then return; end if;" "      null;"
mutant prove+test "existing root replaced" triple_mappings.ads \
    "       else Current.tripleIndirectBlock = Root));" "       else True));"
mutant prove "unlinked-open admitted by name" ext2_support.ads \
    "      (Item.numHardLinks = 1 or (Unlinked_Allowed and Item.numHardLinks = 0)) and" \
    "      (Item.numHardLinks = 1 or Item.numHardLinks = 0) and"

# --- Block cache index (SPARK) ----------------------------------------------
C=block_cache_index.adb
mutant prove "claim evicts a dirty way" $C \
    "            if not Table.Dirty (Slot_Of (Set, Way)) then
               if Table.Referenced" \
    "            if True then
               if Table.Referenced"
mutant prove "mark clean keeps the block dirty" $C \
    "      Table.Dirty (Slot) := False;
   end Mark_Clean;" "      null;
   end Mark_Clean;"
mutant prove "find misses a way" $C \
    "         if Holds (Table, Slot_Of (Set, Way), Key) then
            Found := True;" \
    "         if Way > 0 and then Holds (Table, Slot_Of (Set, Way), Key) then
            Found := True;"

# --- Name cache (SPARK) ------------------------------------------------------
N=dentry_cache.adb
mutant prove "forgotten name still cached" $N \
    "            Cache.Used (Slot_Of (Set, Way)) := False;" "            null;"
mutant prove "insert over the wrong way" $N \
    "      Cache.Keys (Slot_Of (Set, Target)) := Key;" \
    "      Cache.Keys (Slot_Of ((Set + 1) mod Sets, Target)) := Key;"
mutant prove "displaced name leaves its directory complete" $N \
    "            Mark_Incomplete (Cache, Displaced_From.Volume, Displaced_From.Parent);" \
    "            null;"
mutant prove "volume discard keeps entries" $N \
    "         if Cache.Used (Slot) and then Cache.Keys (Slot).Volume = Volume then
            Cache.Used (Slot) := False;" \
    "         if Cache.Used (Slot) and then Cache.Keys (Slot).Volume = Volume + 1 then
            Cache.Used (Slot) := False;"

# --- JBD2 codecs and revokes (SPARK) ------------------------------------------
J=jbd2_format.adb
mutant prove "superblock accepts first = 0" $J \
    "        Super.First < 1 or else Super.First >= Super.Max_Length or else" \
    "        Super.First >= Super.Max_Length or else"
mutant prove "UUID may run past the descriptor" $J \
    "         Offset := Natural'Min (Offset + UUID_Bytes, Limit);" \
    "         Offset := Offset + UUID_Bytes;"
mutant prove "64-bit revoke records counted as 32-bit" $J \
    "         return Natural (Area / 8);" "         return Natural (Area / 4);"
mutant journal "crc32c polynomial wrong" $J \
    "   Crc32c_Polynomial : constant Unsigned_32 := 16#82F6_3B78#;" \
    "   Crc32c_Polynomial : constant Unsigned_32 := 16#82F6_3B79#;"
mutant prove+journal "tid_gt inverted" jbd2_format.ads \
    "     (A /= B and then A - B < 2 ** 31);" "     (A /= B and then B - A < 2 ** 31);"
R=jbd2_revokes.adb
mutant prove "replay writes one block past the volume" $R \
    "      if Home >= Unsigned_64 (Filesystem_Blocks) then" \
    "      if Home > Unsigned_64 (Filesystem_Blocks) then"
mutant prove "revoke stored in a full table" $R \
    "      Stored := Revokes.Count < Capacity;" "      Stored := Revokes.Count <= Capacity;"
mutant prove "revoke ignored" $R \
    "            return Skip_Revoked;" "            return Write_Home;"
mutant journal "recovery reuses the torn sequence" jbd2_recovery.adb \
    "      Next_Sequence := End_Sequence + 1;" "      Next_Sequence := End_Sequence;"

# --- Directory records (SPARK) -----------------------------------------------
D=directory_blocks.adb
mutant prove+journal "removed record merged with a wrong span" $D \
    "         Set_Span (Data, Match_Previous, Match_End - Match_Previous);" \
    "         Set_Span (Data, Match_Previous, Match_End - Match_Start);"
mutant prove+journal "first record not cleared" $D \
    "         Put_Inode (Data, Match_Start, 0);" "         null;"
mutant prove+journal "children not counted" $D \
    "            Children := Children + 1;" "            null;"
mutant prove+journal "'..' span short" $D \
    "      Set_Span (Data, Dot_Span, Size - Dot_Span);" \
    "      Set_Span (Data, Dot_Span, Size - Dot_Span - 4);"

# --- Ext2 block map (hosted tests) --------------------------------------------
E=ext2.adb
mutant test "new leaf linked at the root slot" $E \
    "            when Block_Paths.Triple_Indirect => middleBuf (path.Middle_Slot) := leaf;" \
    "            when Block_Paths.Triple_Indirect => middleBuf (path.Top_Slot) := leaf;"
mutant test "new middle never published" $E \
    "         if newMiddle then
            Publish (root, rootBuf, Root_Pointers);" \
    "         if False then
            Publish (root, rootBuf, Root_Pointers);"
mutant test "shrink leaks an emptied middle" $E \
    "         Retire (top (topSlot));
         top (topSlot) := 0;" "         top (topSlot) := 0;"
mutant test "surviving middle not republished" $E \
    "                     Publish (top (outerSlot), middle, Middle_Pointers);" \
    "                     null;"
mutant test "initially empty middle kept" $E \
    "         if top (topSlot) /= 0 and then Empty (middle) then" \
    "         if False then"
mutant test "shrink skips a partially retained middle" $E \
    "         if firstLogical + Block_Paths.Middle_Span (sectors) <= keepBlocks then" \
    "         if firstLogical < keepBlocks then"
mutant test "control: equivalent rewrite (must survive)" $E \
    "            leafBuf (leafSlot + Natural (index)) := Data_Block (index);" \
    "            leafBuf (leafSlot + Natural (index)) := Data_Block (index) + 0;"

# --- Ext2 journal and namespace operations (journal suites) --------------------
mutant journal "commit before its transaction is durable" $E \
    "         --  (4) the commit block, after its transaction is durable.
         barrier (fs, ok);" \
    "         --  (4) the commit block, after its transaction is durable.
         null;"
mutant journal "released blocks reusable before the commit" $E \
    "      for block of blocks loop
         fs.pendingCount := fs.pendingCount + 1;
         fs.pending (fs.pendingCount) := block;
      end loop;" \
    "      releaseBlocks (fs, blocks, status);"
mutant journal "journaled resize never frees" $E \
    "            if writeStatus = Write_Complete and then count > 0 then
               deferRelease (fs, retired (1 .. count), writeStatus);" \
    "            if False then
               deferRelease (fs, retired (1 .. count), writeStatus);"
mutant journal "handle never closes" $E \
    "      fs.journal.Handle_Depth := fs.journal.Handle_Depth - 1;
      if fs.journal.Handle_Depth = 0 and then" \
    "      null;
      if fs.journal.Handle_Depth = 0 and then"
mutant journal "run inode not in its operation" $E \
    "                        writeInode (fs, inodeNum, published, dataStatus);" \
    "                        dataStatus := Write_Complete;"
mutant journal "unlink keeps the link" $E \
    "      ino.numHardLinks := 0;
      writeInode (fs, target, ino, writeStatus);" \
    "      writeInode (fs, target, ino, writeStatus);"
mutant journal "freed inode not counted free" $E \
    "         bgd.numFreeInodes := bgd.numFreeInodes + 1;" "         null;"
mutant journal "mkdir leaves the parent link count" $E \
    "         parent.numHardLinks := parent.numHardLinks + 1;
         grownParent.numHardLinks := parent.numHardLinks;" \
    "         grownParent.numHardLinks := parent.numHardLinks;"
mutant journal "rmdir of a non-empty directory" $E \
    "            elsif children /= 0 then" "            elsif False then"
mutant journal "rmdir leaves the parent link count" $E \
    "         parent.numHardLinks := parent.numHardLinks - 1;" "         null;"
mutant journal "mkdir not counted as a directory" $E \
    "                           updatedBGD.numDirectories :=
                             updatedBGD.numDirectories + 1;" \
    "                           null;"
mutant journal "rename leaves the directory complete" $E \
    "      --  The new name is not cached: the directory is no longer complete.
      uncertainDirectory (fs, dirInodeNum);" \
    "      null;"
mutant journal "unlink keeps its cached name" $E \
    "         forgetName (fs, parentNum, leaf);
         removeEntry (fs, dir, leaf, target, status);" \
    "         removeEntry (fs, dir, leaf, target, status);"
mutant test "admission keeps cached names" $E \
    "      Dentry_Cache.Discard_Volume (Names, capSlot);" "      null;"
mutant journal "spill copy not consulted by writes" $E \
    "            if Spill_Used > 0 and then spillSlot (key) >= 0 then
               --  Already spilled" \
    "            if False then
               --  Already spilled"
echo "MUTATIONS: $killed of $total killed, $skipped skipped (one control is expected to survive)"
