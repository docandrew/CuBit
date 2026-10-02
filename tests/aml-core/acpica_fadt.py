#!/usr/bin/env python3
"""Compile ACPICA's FADT template and check its fields through ACPI_FADT.

Run inside nix develop. Generated sources/objects are isolated from the normal
hosted suite. Expected values come from iASL's labeled template, not byte offsets
copied from the CuBit decoder. This tests decoding, not register access.
"""
from pathlib import Path
import argparse
import re
import subprocess
import tempfile
def run(command, cwd):
    result = subprocess.run(command, cwd=cwd, capture_output=True, text=True)
    if result.returncode:
        raise RuntimeError(f"{command!r} failed:\n{result.stdout}\n{result.stderr}")


parser = argparse.ArgumentParser()
parser.add_argument('--tools', type=Path, required=True)
args = parser.parse_args()
repo = Path(__file__).resolve().parents[2]
(repo / 'tests/aml-core/build').mkdir(exist_ok=True)
workspace = tempfile.TemporaryDirectory(prefix='fadt-acpica-', dir=repo / 'tests/aml-core/build')
root = Path(workspace.name)
run([str(args.tools.resolve() / 'iasl'), '-T', 'FACP'], root)
s=root.joinpath('facp.asl').read_text()
scalar={'FACS Address':'FACS','DSDT Address':'DSDT','PM Profile':'Profile','SCI Interrupt':'SCI','SMI Command Port':'SMI_Command','ACPI Enable Value':'Enable','ACPI Disable Value':'Disable','S4BIOS Command':'S4_Request','P-State Control':'P_State_Control','GPE1 Base Offset':'GPE1_Base','_CST Support':'C_State_Control','C2 Latency':'C2_Latency','C3 Latency':'C3_Latency','CPU Cache Size':'Flush_Size','Cache Flush Stride':'Flush_Stride','Duty Cycle Offset':'Duty_Offset','Duty Cycle Width':'Duty_Width','RTC Day Alarm Index':'Day_Alarm','RTC Month Alarm Index':'Month_Alarm','RTC Century Index':'Century','Boot Flags (decoded below)':'IA_PC_Boot','Flags (decoded below)':'Flags','Value to cause reset':'Reset_Value','ARM Flags (decoded below)':'ARM_Boot','FADT Minor Revision':'Minor','Hypervisor ID':'Hypervisor.Value'}
blocks={'PM1A Event Block':'PM1A_Event','PM1B Event Block':'PM1B_Event','PM1A Control Block':'PM1A_Control','PM1B Control Block':'PM1B_Control','PM2 Control Block':'PM2_Control','PM Timer Block':'PM_Timer','GPE0 Block':'GPE0','GPE1 Block':'GPE1'}
gas={'Reset Register':'Reset','Sleep Control Register':'Sleep_Control','Sleep Status Register':'Sleep_Status',**{k:f'Extended ({v})' for k,v in blocks.items()}}
for k,v in blocks.items():scalar[k+' Address']=f'Legacy ({v})'
lengths={'PM1 Event Block Length':['PM1A_Event','PM1B_Event'],'PM1 Control Block Length':['PM1A_Control','PM1B_Control'],'PM2 Control Block Length':['PM2_Control'],'PM Timer Block Length':['PM_Timer'],'GPE0 Block Length':['GPE0'],'GPE1 Block Length':['GPE1']}
checks=[];current=None
for line in s.splitlines():
 m=re.match(r'\[(\d+)\]\s+(.+?)\s*:\s*(.*)',line)
 if not m:continue
 size,label,value=m.groups();size=int(size)
 if label in gas:
  current=gas[label];checks.append(f'Check (R.Value.{current}.Present);');continue
 if current and label in {'Space ID','Bit Width','Bit Offset','Encoded Access Width','Address'}:
  field={'Space ID':'Space','Bit Width':'Width','Bit Offset':'Bit_Offset','Encoded Access Width':'Access_Size','Address':'Address'}[label]
  checks.append(f'Check (R.Value.{current}.Value.{field} = 16#{value.split()[0]}#);')
  if label=='Address':current=None
  continue
 if label in lengths:
  for block in lengths[label]:checks.append(f'Check (R.Value.Lengths ({block}) = 16#{value.split()[0]}#);')
 elif label in scalar:
  field=scalar[label]
  if label in ['FACS Address','DSDT Address'] and size==8:
   field='X_'+field;checks.append(f'Check (R.Value.{field}.Present);');field+='.Value'
  checks.append(f'Check (R.Value.{field} = 16#{value.split()[0]}#);')
root.joinpath('probe.adb').write_text('''with Ada.Text_IO; with Ada.Command_Line; with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces; with Firmware_Tables; with ACPI_FADT; use ACPI_FADT;
procedure Probe is
 package IO renames Ada.Streams.Stream_IO;
 F : IO.File_Type; Checks : Natural := 0;
 procedure Check (OK : Boolean) is begin
 Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if;
 end Check;
begin
 IO.Open (F, IO.In_File, Ada.Command_Line.Argument (1));
 declare
 Data : Firmware_Tables.Bytes (1 .. Natural (IO.Size (F)));
 R : Result;
 begin
 for I in Data'Range loop Unsigned_8'Read (IO.Stream (F), Data (I)); end loop;
 IO.Close (F); R := Decode (Data); Check (R.Valid);
'''+ '\n'.join(checks)+'''
 Ada.Text_IO.Put_Line ("ACPICA-FADT-TEMPLATE: PASS" & Checks'Image);
 end;
end Probe;
''')
root.joinpath('probe.gpr').write_text('''project Probe is
 for Source_Dirs use (".", "REPO/userspace/lib/acpi", "REPO/shared/firmware");
 for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("probe.adb");
 package Compiler is for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato"); end Compiler;
end Probe;
'''.replace('REPO', str(repo)))
# A changed template must not silently shrink coverage.
if len(checks) != 112:
    raise SystemExit(f'Unexpected ACPICA FADT template shape: {len(checks)} checks')
run([str(args.tools.resolve() / 'iasl'), 'facp.asl'], root)
run(['alr', 'exec', '--', 'gprbuild', '-p', '-P', str(root / 'probe.gpr')], repo / 'kernel')
subprocess.run([str(root / 'probe'), str(root / 'facp.aml')], check=True)
workspace.cleanup()
