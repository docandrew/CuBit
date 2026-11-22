#!/usr/bin/env bash
#  Mutation checks for the HDMI modeset sequence and its plan: each mutant
#  must make tests/display-modeset fail (the register model's ordering rules,
#  the rollback checks or the watermark/plan assertions). Run in the Nix
#  shell after run.sh has staged build/source.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
src="$here/build/source"
cd "$here/../../kernel"
killed=0; total=0
mutate() {
  local name=$1 file=$2 from=$3 to=$4
  total=$((total + 1))
  cp "$src/$file" "$src/$file.orig"
  python3 - "$src/$file" "$from" "$to" <<'EOF'
import sys
p, a, b = sys.argv[1:]
s = open(p).read()
assert a in s, "pattern not found: " + a
open(p, "w").write(s.replace(a, b, 1))
EOF
  if alr exec -- gprbuild -p -q -P "$here/display_modeset.gpr" >/dev/null 2>&1 &&
     "$here/build/modeset_tests" >/dev/null 2>&1 && "$here/build/intel_tests" >/dev/null 2>&1; then
    echo "SURVIVED: $name"
  else
    echo "killed: $name"; killed=$((killed + 1))
  fi
  mv "$src/$file.orig" "$src/$file"
}
mutate "DDI buffer before pipe" intel_gpu_hdmi_modeset.adb \
  "      Put (State, Function_On, TB + R.TRANS_DDI_FUNC_CTL, Wanted.Function_Word);" \
  "      Put (State, Buffer_On, R.DDI_Buffer_Control (Item.Port), Wanted.Buffer_Control or R.DDI_Buffer_Enable);
      Put (State, Function_On, TB + R.TRANS_DDI_FUNC_CTL, Wanted.Function_Word);"
mutate "no PLL power before CFGCR" intel_gpu_hdmi_modeset.adb \
  "      Put (State, PLL_Power_On, R.DPLL_Enable (Item.PLL), Read (R.DPLL_Enable (Item.PLL)) or R.PLL_Power_Enable);" \
  "      null;"
mutate "PLL select and ungate in one write" intel_gpu_hdmi_modeset.adb \
  "(Routing and not Select_Mask (Item)) or Select_Value (Item) or Clock_Off_Bit (Item));" \
  "((Routing and not Select_Mask (Item)) or Select_Value (Item)) and not Clock_Off_Bit (Item));"
mutate "timings while the pipe runs" intel_gpu_hdmi_modeset.adb \
  "      Wait_For (State, Pipe_Off, Transconf (Item), R.Transcoder_State, False, Pipe_Deadline_Us);" \
  "      Wait_For (State, Pipe_Off, Transconf (Item), R.Transcoder_State, False, Pipe_Deadline_Us);
      Put (State, Timings, TB + R.TRANS_HTOTAL, Current.Timing.H_Total);
      Put (State, Pipe_Off, Transconf (Item), Current.Transcoder_Configuration);"
mutate "no rollback" intel_gpu_hdmi_modeset.adb \
  "      Back.Force := True;" \
  "      Result := Rolled_Back; return;"
mutate "lock wait ignored" intel_gpu_hdmi_modeset.adb \
  "      Wait_For (State, PLL_Lock, R.DPLL_Enable (Item.PLL), R.PLL_Lock, True, PLL_Deadline_Us);" \
  "      null;"
mutate "underrun not checked" intel_gpu_hdmi_modeset.adb \
  "         if (Read (Pipe_IIR (Item)) and FIFO_Underrun) /= 0 then" \
  "         if False then"
mutate "SCDC never written" intel_gpu_hdmi_modeset.adb \
  "      if Wanted.Write_SCDC then" \
  "      if False then"
mutate "watermark = whole allocation" intel_gpu_modeset_plan.adb \
  "      return Natural'Max (1, Blocks);" \
  "      return Size;"
mutate "scrambling threshold off" intel_gpu_modeset_plan.adb \
  "      Scrambled : constant Boolean := Clock > 340_000;" \
  "      Scrambled : constant Boolean := Clock > 600_000;"
mutate "WRPLL farthest from the middle" intel_gpu_combo_wrpll.adb \
  "abs (DCO - DCO_Mid_kHz) < Long_Long_Integer (Best_Centrality)" \
  "abs (DCO - DCO_Mid_kHz) >= 0"
mutate "fraction workaround dropped" intel_gpu_combo_wrpll.adb \
  "         Result.Fraction := (if Halved then (Fraction + 1) / 2 else Fraction);" \
  "         Result.Fraction := Fraction;"
mutate "GMBUS no STOP" intel_gpu_gmbus.adb \
  "         Put (R.GMBUS1, Cycle_Stop or SW_Ready, Release);" \
  "         null;"
mutate "admission ignores the clock hole" intel_gpu_mode_admission.ads \
  "      elsif Pixel_kHz (Mode) > Combo_Hole_Low and then Pixel_kHz (Mode) < Combo_Hole_High" \
  "      elsif False"
alr exec -- gprbuild -p -q -P "$here/display_modeset.gpr" >/dev/null 2>&1
echo "mutations: $killed of $total killed"
[ "$killed" -eq "$total" ]
