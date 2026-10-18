#!/usr/bin/env bash
# Mutation checks (tests/text-editor/README.md): each mutant changes one
# thing in a copy of the editor core; the scripted tests must catch it (a
# test fails or the program dies on a contract). A mutant that survives is a
# hole in the tests. Run inside nix develop: run.sh --mutate.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
core="$root/userspace/lib/text"
mutant="$here/build/mutant"
# file|exact text|replacement|what the mutation breaks
mutations=(
  "text_pieces.adb|if Offset < Start + Byte_Count (Current.Items (K).Length) or else K = Current.Count then|if Offset <= Start + Byte_Count (Current.Items (K).Length) or else K = Current.Count then|Locate picks the piece that ends at the offset"
  "text_pieces.adb|              and then Prior.Length <= MAXIMUM_PIECE_BYTES - New_Piece.Length|              and then Prior.Length <= MAXIMUM_PIECE_BYTES|typing extends a piece past its bound"
  "text_pieces.adb|               Current.Count := Current.Count - Removed;|               Current.Count := Current.Count - Removed + 1;|delete keeps one piece too many"
  "text_stores.adb|               Result := Result + Boolean'Pos (C = ASCII.LF);|               Result := Result + Boolean'Pos (C = ASCII.CR);|line feeds counted wrong"
  "text_history.adb|            if R.Kind = Inserted and then R.Offset <= MAXIMUM_BYTES - R.Length|            if R.Offset <= MAXIMUM_BYTES - R.Length|an insert merges into a removal record"
  "text_history.adb|            and then Now_Us >= G.Time_Us and then Now_Us - G.Time_Us < COALESCE_GAP_US|            and then Now_Us >= G.Time_Us|typing never splits on a pause"
  "text_cursors.adb|              Last_Of (Prior) > First_Of (Item)|              Last_Of (Prior) >= First_Of (Item)|touching selections merge"
  "text_utf8.adb|      if Value < Natural (Minimum) or else Value > MAXIMUM_CODE_POINT or else Value in 16#D800# .. 16#DFFF# then|      if Value > MAXIMUM_CODE_POINT or else Value in 16#D800# .. 16#DFFF# then|overlong UTF-8 accepted"
  "text_utf8.adb|         elsif Regional and then Is_Regional_Indicator (Next_Code) then|         elsif Is_Regional_Indicator (Next_Code) then|flags join without limit"
  "text_layout.adb|                     Break := (if After_Space > Row_Start then After_Space else Index);|                     Break := Index;|wrap ignores word breaks"
  "text_search.adb|               if Advance and then T.PC < N then|               if Advance and then T.PC < N - 1 then|the VM drops the last step"
  "text_search.adb|                              Stack (Top + 1) := (I.Y, T.Start);\n                              Stack (Top + 2) := (I.X, T.Start);|                              Stack (Top + 1) := (I.X, T.Start);\n                              Stack (Top + 2) := (I.Y, T.Start);|split priorities swapped (lazy/greedy)"
  "text_search.adb|     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);|     (C);|case folding off"
  "text_syntax.adb|               if State.Depth = 0 or else not Nested then|               if not Nested then|nested comments never close"
  "text_documents.adb|         return Line_End (D, L - 1);\n      elsif H > Line_End (D, L) then|         return Line_End (D, L - 1) + 1;\n      elsif H > Line_End (D, L) then|left across a line end lands wrong"
  "text_marks.adb|               if M.First < First and then First < M.Last then|               if M.First < First and then First <= M.Last then|inserting after a region is refused"
  "text_documents.adb|                     if Rest > 0 and then (After < Rest|                     if Rest > 0 and then (After <= Rest|a line start found past the piece's last feed"
  "text_documents.adb|(if Feed > Start and then Byte_At (D, Feed - 1) = ASCII.CR then Feed - 1 else Feed)|(if Feed > Start and then Byte_At (D, Feed - 1) = ASCII.CR then Feed else Feed)|a CR LF line ends after its CR"
  "text_editors.adb|               E.Paint (Slot) := Content_Width'Max (Width, E.Previous_Width (Slot));|               E.Paint (Slot) := Width;|a shrinking row leaves its old text"
  "text_editors.adb|                 or else Lit or else E.Previous_Lit (Slot);|                 or else Lit;|the old current line keeps its highlight"
)
mkdir -p "$here/build"
survived=0
killed=0
for m in "${mutations[@]}"; do
  IFS='|' read -r file from to why <<< "$m"
  rm -rf "$mutant" && mkdir -p "$mutant" && cp "$core"/*.ad? "$mutant/"
  if ! python3 -I - "$mutant/$file" "$from" "$to" <<'EOF'
import sys
path, old, new = sys.argv[1], sys.argv[2].replace("\\n", "\n"), sys.argv[3].replace("\\n", "\n")
text = open(path).read()
if text.count(old) != 1:
    sys.exit(f"mutation site not found exactly once in {path}: {old[:60]}")
open(path, "w").write(text.replace(old, new))
EOF
  then echo "SKIP  $why (site moved)"; continue; fi
  if ! (cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -XTEXT_MODE=mutant -P "$here/text_editor.gpr" \
          text_core_tests.adb text_view_tests.adb > "$here/build/mutant.log" 2>&1); then
    echo "KILLED $why (does not compile)"; killed=$((killed + 1)); continue
  fi
  if (cd "$here" && timeout 600 build/text_core_tests_mutant > build/mutant-run.log 2>&1 &&
      timeout 600 build/text_view_tests_mutant >> build/mutant-run.log 2>&1); then
    echo "SURVIVED $why"; survived=$((survived + 1))
  else
    echo "KILLED $why ($(grep -m1 'FAIL\|raised' "$here/build/mutant-run.log" | cut -c1-90))"; killed=$((killed + 1))
  fi
done
rm -rf "$mutant"
echo "$killed killed, $survived survived"
[ "$survived" -eq 0 ]
