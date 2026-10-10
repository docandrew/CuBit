#!/usr/bin/env python3
"""Exercise the exact native work-drain predicate with controlled observations.

GPU-001 step 2: a session is drained when the queue service says it has no
job in flight (Session_Idle), on top of the deferred-publisher guards.

This is hosted decision coverage, not proof of hardware quiescence or authority.
The negative control removes a real deferred-publisher guard and must fail.
"""
from pathlib import Path
import hashlib
import json
import re
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / 'userspace/services/intel-gpu'
inputs = [source / 'main.adb']
original = {p: p.read_bytes() for p in inputs}
main = original[inputs[0]].decode()
start = main.index('   function Application_Work_Drained (Session : Unsigned_64) return Boolean is')
end = main.index('   end Application_Work_Drained;', start) + len('   end Application_Work_Drained;')
guard = main[start:end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
procedure Work_Drain_Tests is
   package Intel_GPU_Render_Sessions is
      Tag_Base : constant Unsigned_64 := 100;
      Capacity : constant := 4;
      subtype Slot_Index is Natural range 0 .. Capacity;
   end Intel_GPU_Render_Sessions;
   package Intel_GPU_Render_Control is
      function Storage_Index (Object : Natural; Tag : Unsigned_64)
        return Intel_GPU_Render_Sessions.Slot_Index is
        (if Object /= 1 then 0
         elsif Tag = 101 then 4 elsif Tag = 102 then 1
         elsif Tag = 103 then 2 else 0);
   end Intel_GPU_Render_Control;
   Render_Admission : Natural := 1;
   package Queue_Service is
      type Table is array (1 .. 4) of Boolean;   -- Session_Idle per index
      function Session_Idle (T : Table; Index : Positive) return Boolean is (T (Index));
   end Queue_Service;
   Queue_State : Queue_Service.Table := [others => True];
   Runtime_Fault : Boolean := False;
   Context_Owner : Boolean := True;
   Deferred : Boolean := False;
   function Deferred_Application_Work return Boolean is (Deferred);
   Cleaning : Boolean := False;
   function Cleanup_Pending (Session : Unsigned_64) return Boolean is (Cleaning);
   Application_Setup_Complete : array (1 .. 4) of Boolean := [others => False];
   Sessions : constant array (1 .. 9) of Unsigned_64 :=
     [0, 99, 100, 101, 102, 103, 104, 105, Unsigned_64'Last];
   Checks : Natural := 0;
   Expected : Boolean;
'''
suffix = '''
begin
   for Mask in Unsigned_32 range 0 .. 15 loop
      Runtime_Fault := (Mask and 1) /= 0;
      Context_Owner := (Mask and 2) = 0;
      Deferred := (Mask and 4) /= 0;
      Cleaning := (Mask and 8) /= 0;
      for Session of Sessions loop
         for Setup in Boolean loop
            for Idle in Boolean loop
               Application_Setup_Complete := [others => False];
               Queue_State := [others => False];
               if Session in 101 .. 103 then
                  declare
                     Index : constant Positive :=
                       (case Session is when 101 => 4, when 102 => 1, when others => 2);
                  begin
                     Application_Setup_Complete (Index) := Setup;
                     Queue_State (Index) := Idle;
                  end;
               end if;
               Expected := Mask = 0 and then Session in 101 .. 103 and then Setup and then Idle;
               pragma Assert (Application_Work_Drained (Session) = Expected);
               Checks := Checks + 1;
            end loop;
         end loop;
      end loop;
   end loop;
   pragma Assert (Checks = 576);
   Ada.Text_IO.Put_Line ("Native work-drain predicate PASS" & Natural'Image (Checks));
end Work_Drain_Tests;
'''
output = root / 'tests/intel-gpu/build'
output.mkdir(exist_ok=True)
work = Path(tempfile.mkdtemp(prefix='work-drain.', dir=output))
for name, body in [('actual', guard), ('missing-deferred-publisher',
        guard.replace('Deferred_Application_Work or else ', '', 1)),
        ('missing-idle-check', guard.replace('Queue_Service.Session_Idle (Queue_State, Index)', 'True', 1)),
        ('tag-as-index', guard.replace('Positive := Stored;',
         'Positive := Positive (Session - Intel_GPU_Render_Sessions.Tag_Base);', 1))]:
    if name != 'actual' and body == guard:
        raise SystemExit('Negative-control mutation no longer matches native source')
    directory = work / name
    directory.mkdir()
    (directory / 'work_drain_tests.adb').write_text(prefix + body + suffix)
    (directory / 'test.gpr').write_text('''project Test is
      for Source_Dirs use (".");
      for Object_Dir use "obj";
      for Exec_Dir use ".";
      for Main use ("work_drain_tests.adb");
      package Compiler is
         for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
      end Compiler;
   end Test;
''')
    subprocess.run(['gprbuild', '-q', '-p', '-P', 'test.gpr'], cwd=directory, check=True)
    result = subprocess.run([str(directory / 'work_drain_tests')], capture_output=True, text=True)
    (directory / 'result.log').write_text(result.stdout + result.stderr)
    if name == 'actual':
        if result.returncode or 'PASS 576' not in result.stdout:
            raise SystemExit('Actual predicate failed: ' + result.stdout + result.stderr)
        print(result.stdout.strip())
    elif result.returncode == 0 or 'ASSERTION_ERROR' not in result.stderr.upper():
        raise SystemExit('Negative control did not fail the expected assertion')
    else:
        print('Negative control PASS:', name)
if any(p.read_bytes() != data for p, data in original.items()):
    raise SystemExit('Source changed during test')
(work / 'result.json').write_text(json.dumps({
    'hosted_only': True, 'actual_checks': 576, 'negative_control_detected': True,
    'source_sha256': {str(p): hashlib.sha256(data).hexdigest() for p, data in original.items()},
}, indent=2) + '\n')
print('Evidence:', work)
