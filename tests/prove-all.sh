#!/usr/bin/env bash
#  Run every SPARK proof target listed in tests/prove-all.targets and print
#  one summary table: target, level, checks, unproved, justified, pragma
#  Assume count, result. This is the proof checkpoint in one command.
#
#    tests/prove-all.sh                     every target, one at a time
#    tests/prove-all.sh --group desktop     only one group (repeatable)
#    tests/prove-all.sh --only qoi          only named targets (repeatable)
#    tests/prove-all.sh --parallel 3        up to three groups at once
#    tests/prove-all.sh --list              print the targets and stop
#    tests/prove-all.sh --budget            per-unit proof budget check (below)
#
#  Options: --timeout SECONDS per target (default 5400), --log-dir DIR
#  (default build/prove-all). Each target's full output is in
#  <log-dir>/<name>.log; the table is also written to <log-dir>/summary.tsv.
#  Exit status is 0 only when every selected target proved with nothing
#  unproved.
#
#  Counts come from the gnatprove.out summaries each target logs ("Summary
#  logged in ..."); a target that runs several gnatprove commands is the sum
#  of them. Targets whose script runs hosted tests first also fail on a
#  failed test. Run inside `nix develop` (the script re-enters it if
#  gnatprove is not on PATH); export TMPDIR to a directory with space first.
#  Every target runs under `nice -n 19`; per-target gnatprove -j is set in
#  the targets file. Heavy targets (kernel, ccl-*) take minutes each.
#
#  --budget (CODE-002): instead of the targets as written, prove every unit
#  a gnatprove target names with -u alone, at the target's level and -j2,
#  each in its own `systemd-run --user --scope` capped at --budget-memory
#  (default 8G, no swap) and --budget-time seconds (default 600). Prints
#  one row per unit (peak memory from the scope's memory.peak, seconds,
#  unproved) and OVER-MEMORY / OVER-TIME for units over budget; exits 1 if
#  any is. Split a unit over budget; do not raise the budget. Targets that
#  run a script or make recipe are listed as not checked. Each unit is
#  analysed afresh (gnatprove -f).
set -uo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)
targets_file="$here/prove-all.targets"

if ! command -v gnatprove >/dev/null 2>&1; then
    exec nix develop "$root" -c bash "$0" "$@"
fi

groups=() only=() parallel=1 timeout_s=5400 log_dir="$root/build/prove-all" list=0
budget=0 budget_memory=8G budget_time=600
while (($#)); do
    case $1 in
        --group) groups+=("$2"); shift 2 ;;
        --only) only+=("$2"); shift 2 ;;
        --parallel) parallel=$2; shift 2 ;;
        --timeout) timeout_s=$2; shift 2 ;;
        --log-dir) log_dir=$2; shift 2 ;;
        --list) list=1; shift ;;
        --budget) budget=1; shift ;;
        --budget-memory) budget_memory=$2; shift 2 ;;
        --budget-time) budget_time=$2; shift 2 ;;
        -h|--help) sed -n '2,34p' "$0"; exit 0 ;;
        *) echo "prove-all: unknown option $1" >&2; exit 2 ;;
    esac
done

trim() { local s=$1; s=${s#"${s%%[![:space:]]*}"}; printf '%s' "${s%"${s##*[![:space:]]}"}"; }

contains() {
    local needle=$1; shift
    local item
    for item in "$@"; do [[ $item == "$needle" ]] && return 0; done
    return 1
}

names=() group_of=() dir_of=() cmd_of=()
while IFS= read -r line; do
    [[ -z ${line//[[:space:]]/} || $line == \#* ]] && continue
    IFS='|' read -r name group dir cmd <<<"$line"
    name=$(trim "$name"); group=$(trim "$group"); dir=$(trim "$dir"); cmd=$(trim "$cmd")
    # A command may itself contain '|': keep everything after the third field.
    cmd=$(trim "${line#*|*|*|}")
    ((${#groups[@]})) && ! contains "$group" "${groups[@]}" && continue
    ((${#only[@]})) && ! contains "$name" "${only[@]}" && continue
    names+=("$name"); group_of+=("$group"); dir_of+=("$dir"); cmd_of+=("$cmd")
done <"$targets_file"

if ((list)); then
    for i in "${!names[@]}"; do
        printf '%-28s %-8s %-22s %s\n' "${names[i]}" "${group_of[i]}" "${dir_of[i]}" "${cmd_of[i]}"
    done
    exit 0
fi
((${#names[@]})) || { echo "prove-all: no targets selected" >&2; exit 2; }

mkdir -p "$log_dir"


#  The proof levels a target asks for: its own --level flags, plus those in
#  the make recipe or script it runs.
levels_of() {
    local dir=$1 cmd=$2 text target script
    text=$cmd
    if [[ $cmd =~ ^make\ ([a-zA-Z0-9_-]+) ]]; then
        target=${BASH_REMATCH[1]}
        text+=$'\n'$(awk -v t="$target" '
            $0 ~ "^"t":" {p=1; next} p && /^\t/ {print; next} {p=0}' "$root/$dir/Makefile")
        # prove-x: prove-y dependencies (prove-ccl-cbor) and scripts they call.
        local dep
        for dep in $(awk -v t="$target" -F: '$1==t {print $2}' "$root/$dir/Makefile"); do
            [[ $dep == prove-* ]] && text+=$'\n'$(awk -v t="$dep" '
                $0 ~ "^"t":" {p=1; next} p && /^\t/ {print; next} {p=0}' "$root/$dir/Makefile")
        done
    fi
    for script in $(grep -oE '[^ ]+\.sh' <<<"$text"); do
        [[ -f $root/$dir/$script ]] && text+=$'\n'$(cat "$root/$dir/$script")
    done
    local found
    found=$(grep -oE -- '--level[= ]?[0-4]' <<<"$text" | grep -oE '[0-4]$' | sort -u | paste -sd, -)
    printf '%s' "${found:-0}"
}

#  The units a gnatprove command names with -u (empty for make and scripts).
units_of() {
    local in_u=0 tok
    [[ $1 == *gnatprove* ]] || return 0
    for tok in $1; do
        if [[ $tok == -u ]]; then in_u=1; continue; fi
        if ((in_u)); then
            if [[ $tok == -* || $tok == *';'* || $tok == '&&' ]]; then in_u=0; continue; fi
            printf '%s\n' "$tok"
        fi
    done
}

#  The command with its -u list replaced by Unit, its -jN by -j2, and -f
#  (analyse again: an earlier run's results must not hide the cost).
single_unit() {
    local cmd=$1 unit=$2 in_u=0 tok out=()
    for tok in $cmd; do
        if [[ $tok == -u ]]; then in_u=1; out+=(-u "$unit"); continue; fi
        if ((in_u)); then
            if [[ $tok == -* || $tok == *';'* || $tok == '&&' ]]; then in_u=0; else continue; fi
        fi
        [[ $tok =~ ^-j[0-9]+$ ]] && tok=-j2
        out+=("$tok")
        [[ $tok == gnatprove ]] && out+=(-f)
    done
    printf '%s' "${out[*]}"
}

if ((budget)); then
    command -v systemd-run >/dev/null || { echo "prove-all: --budget needs systemd-run" >&2; exit 2; }
    budget_file="$log_dir/budget.tsv"
    printf 'target\tunit\tlevel\tpeak_mb\tseconds\tunproved\tresult\n' >"$budget_file"
    over=0
    for i in "${!names[@]}"; do
        mapfile -t units < <(units_of "${cmd_of[i]}")
        if ((${#units[@]} == 0)); then
            printf '%s\t-\t-\t-\t-\t-\tnot-checked\n' "${names[i]}" >>"$budget_file"
            continue
        fi
        for unit in "${units[@]}"; do
            log="$log_dir/budget-${names[i]}-$unit.log"
            echo "prove-all: budget ${names[i]} $unit ..." >&2
            t0=$(date +%s)
            (cd "$root/${dir_of[i]}" && systemd-run --user --scope -q \
                -p MemoryMax="$budget_memory" -p MemorySwapMax=0 \
                bash -c 'cg=/sys/fs/cgroup$(cut -d: -f3 /proc/self/cgroup)
                         timeout --kill-after=30 "$1" nice -n 19 bash -c "$2"; rc=$?
                         echo "# peak_bytes: $(cat "$cg/memory.peak" 2>/dev/null || echo 0)"
                         echo "# oom_kills: $(awk "/^oom_kill /{print \$2}" "$cg/memory.events")"
                         exit $rc' _ "$budget_time" "$(single_unit "${cmd_of[i]}" "$unit")") >"$log" 2>&1
            status=$?
            seconds=$(( $(date +%s) - t0 ))
            peak=$(( $(sed -n 's/^# peak_bytes: *//p' "$log" | tail -1) / 1048576 ))
            kills=$(sed -n 's/^# oom_kills: *//p' "$log" | tail -1)
            unproved=$(grep -cE '^[^ :]+:[0-9]+:[0-9]+: (error: )?(low|medium|high): ' "$log")
            if ((${kills:-0} > 0)) || ((status == 137)); then result=OVER-MEMORY
            elif ((status == 124)); then result=OVER-TIME
            elif ((unproved > 0)); then result=UNPROVED
            elif ((status != 0)); then result=FAIL
            else result=OK
            fi
            [[ $result == OVER-* ]] && over=$((over + 1))
            printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "${names[i]}" "$unit" \
                "$(levels_of "${dir_of[i]}" "${cmd_of[i]}")" "$peak" "$seconds" "$unproved" "$result" >>"$budget_file"
        done
    done
    column -t -s $'\t' "$budget_file"
    echo
    echo "prove-all: budget $budget_memory / ${budget_time}s per unit at -j2; $over unit(s) over budget; table in $budget_file"
    ((over == 0))
    exit
fi

run_one() {
    local i=$1 name=${names[$1]} log="$log_dir/${names[$1]}.log" status
    rm -f "$log_dir/$name".*.gnatprove.out
    {
        echo "# target: $name"
        echo "# dir:    ${dir_of[i]}"
        echo "# cmd:    ${cmd_of[i]}"
        echo "# start:  $(date -Is)"
    } >"$log"
    (cd "$root/${dir_of[i]}" && nice -n 19 timeout --kill-after=60 "$timeout_s" \
        bash -c "${cmd_of[i]}") >>"$log" 2>&1
    status=$?
    #  Keep this run's summaries: a later run of the same project overwrites
    #  gnatprove.out in place.
    local n=0 out
    while IFS= read -r out; do
        [[ -f $out ]] || continue
        n=$((n + 1))
        cp "$out" "$log_dir/$name.$n.gnatprove.out"
    done < <(grep -oE 'Summary logged in .*gnatprove\.out' "$log" | sed 's/^Summary logged in //')
    echo "# exit:   $status" >>"$log"
    echo "# end:    $(date -Is)" >>"$log"
}

#  Sum the gnatprove.out summaries a target's run saved. Prints:
#  summaries checks unproved justified assumes
summarise() {
    local log=$1 out total=0 unproved=0 justified=0 assumes=0 count=0
    while IFS= read -r out; do
        [[ -f $out ]] || continue
        count=$((count + 1))
        read -r t j u < <(awk '
            /^Total/ {
                t = $2
                # Columns after Total: Flow, Provers, Justified, Unproved;
                # a column is "." or "N" or "N (x%)". Split on runs of 2+ spaces.
                line = $0; sub(/^Total +/, "", line)
                n = split(line, f, /  +/)
                just = f[n-1]; unp = f[n]
                sub(/ .*/, "", just); sub(/ .*/, "", unp)
                if (just == ".") just = 0
                if (unp == ".") unp = 0
                print t, just, unp; exit
            }' "$out")
        total=$((total + ${t:-0})); justified=$((justified + ${j:-0}))
        unproved=$((unproved + ${u:-0}))
        assumes=$((assumes + $(grep -oE '[0-9]+ pragma Assume statements' "$out" |
            awk '{s += $1} END {print s + 0}')))
    done < <(ls "${log%.log}".*.gnatprove.out 2>/dev/null)
    #  Unproved check messages in the output itself, in case a summary is
    #  missing (gnatprove stopped after reporting them).
    local messages
    messages=$(grep -cE '^[^ :]+:[0-9]+:[0-9]+: (error: )?(low|medium|high): ' "$log")
    ((messages > unproved)) && unproved=$messages
    echo "$count $total $unproved $justified $assumes"
}

start=$(date +%s)
#  Targets in one group share proof projects and object directories, so a
#  group always runs in order; --parallel runs up to N groups at once.
run_group() {
    local g=$1 i
    for i in "${!names[@]}"; do
        [[ ${group_of[i]} == "$g" ]] || continue
        echo "prove-all: ${names[i]} ..." >&2
        run_one "$i"
    done
}
mapfile -t group_order < <(printf '%s\n' "${group_of[@]}" | awk '!seen[$0]++')
running=0
for g in "${group_order[@]}"; do
    if ((parallel > 1)); then
        run_group "$g" &
        running=$((running + 1))
        if ((running >= parallel)); then wait -n; running=$((running - 1)); fi
    else
        run_group "$g"
    fi
done
wait

summary="$log_dir/summary.tsv"
printf 'target\tgroup\tlevel\tchecks\tunproved\tjustified\tassume\tseconds\tresult\n' >"$summary"
failures=0
for i in "${!names[@]}"; do
    log="$log_dir/${names[i]}.log"
    exit_code=$(sed -n 's/^# exit: *//p' "$log" | tail -1)
    t0=$(date -d "$(sed -n 's/^# start: *//p' "$log")" +%s)
    t1=$(date -d "$(sed -n 's/^# end: *//p' "$log")" +%s)
    read -r count checks unproved justified assumes < <(summarise "$log")
    if ((exit_code == 124 || exit_code == 137)); then result=TIMEOUT
    elif ((count == 0)); then result=NO-PROOF
    elif ((unproved > 0)); then result=UNPROVED
    elif ((exit_code != 0)); then result=FAIL
    else result=PASS
    fi
    [[ $result == PASS ]] || failures=$((failures + 1))
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "${names[i]}" "${group_of[i]}" \
        "$(levels_of "${dir_of[i]}" "${cmd_of[i]}")" "$checks" "$unproved" "$justified" \
        "$assumes" "$((t1 - t0))" "$result" >>"$summary"
done

column -t -s $'\t' "$summary"
echo
echo "prove-all: ${#names[@]} targets, $failures not PASS, $(( $(date +%s) - start )) s; logs in $log_dir"
echo "prove-all: NO-PROOF = the target exited before any gnatprove summary (build or test failure); see its log"
((failures == 0))
