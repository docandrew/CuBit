"""Extract native owner gate and metadata prefix; model IPC ownership/allocators.

Forwarding marks entry to the later provenance stage, not GPU publication.
"""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / 'userspace/services/intel-gpu/main.adb'
original = source.read_bytes()
text = original.decode()
start = text.index('   function Table_Ledger_Owner return Boolean')
helpers = text[start:text.index('   end Stop_Table_Ledger;', start) + len('   end Stop_Table_Ledger;')]
start = text.index('   procedure Grow_Table_Ledger is')
body = text[start:text.index('         Ledger_Growth.Step', start)]
body += '''         Forwarded := Forwarded + 1;
      else Stop_Table_Ledger ("owner unavailable");
      end if;
   end Grow_Table_Ledger;
'''
prefix = '''with Ada.Text_IO;
procedure Test is
   Fault, Ledger_Index, Ledger_Session, Forwarded, Calls, Logs, Last_Kind : Natural := 0;
   Publication_Owner_Ready : Boolean := True;
   Resolved : Natural := 1;
   Render_Admission : Natural := 0;
   package Application_Lifetime is type Phase is (Offline, Retired); end;
   use type Application_Lifetime.Phase;
   type Item is record Life : Application_Lifetime.Phase; end record;
   Private_Contexts : array (1 .. 1) of Item := [(Life => Application_Lifetime.Offline)];
   package Intel_GPU_Render_Control is
      function Storage_Index (Admission, Session : Natural) return Natural is (Resolved);
   end;
   package Application_State is Table_Pages : constant := 64; end;
   procedure Publish_Snapshot (Reason : String) is begin Logs := Logs + 1; end;
   function Table_Ledger_Busy return Boolean is (Ledger_Index /= 0);
   function Context_Descriptor_Capacity return Positive is (if Fault <= 3 then 4 else 64);
   function Context_Reference_Capacity return Positive is (if Fault >= 10 then 4 else 64);
   generic Kind : Positive;
   package Model is
      type Phase is (Growing, Idle, Failed);
      type View is record State : Phase; end record;
      function Snapshot (Object : Phase) return View is ((State => Object));
      procedure Step (Object : in out Phase);
   end;
   package body Model is
      procedure Step (Object : in out Phase) is
      begin
         Calls := Calls + 1;
         Last_Kind := Kind;
         if (Kind = 1 and Fault = 1) or (Kind = 2 and Fault = 4) then Publication_Owner_Ready := False; end if;
         if Kind = 1 and Fault = 2 then Resolved := 0; end if;
         if (Kind = 1 and Fault = 3) or (Kind = 2 and Fault = 5) then Object := Failed; end if;
         if Kind = 3 then
            case Fault is
               when 11 => Publication_Owner_Ready := False;
               when 12 => Resolved := 0;
               when 13 => Object := Failed;
               when others => null;
            end case;
         end if;
      end;
   end;
   package Context_Descriptor_Growth is new Model (1);
   package Context_Mirror_Growth is new Model (2);
   package Context_Reference_Growth is new Model (3);
   use type Context_Descriptor_Growth.Phase, Context_Mirror_Growth.Phase,
     Context_Reference_Growth.Phase;
   Descriptor_Controllers : array (1 .. 1) of Context_Descriptor_Growth.Phase;
   Mirror_Controllers : array (1 .. 1) of Context_Mirror_Growth.Phase;
   Reference_Controllers : array (1 .. 1) of Context_Reference_Growth.Phase;
'''
suffix = '''
begin
   for F in 0 .. 14 loop
      Fault := F; Ledger_Index := (if F = 7 then 0 else 1); Ledger_Session := 42;
      Forwarded := 0; Calls := 0; Logs := 0; Resolved := 1; Last_Kind := 0;
      Publication_Owner_Ready := F not in 6 | 14;
      Private_Contexts (1).Life := Application_Lifetime.Offline;
      Descriptor_Controllers (1) := Context_Descriptor_Growth.Growing;
      Reference_Controllers (1) := Context_Reference_Growth.Growing;
      Mirror_Controllers (1) := (if F in 4 | 5 | 9 then Context_Mirror_Growth.Growing else Context_Mirror_Growth.Idle);
      Grow_Table_Ledger;
      if F in 0 | 9 | 10 then
         pragma Assert (Calls = 1 and Forwarded = 0 and Ledger_Index = 1 and Logs = 0);
         pragma Assert (Last_Kind = (if F = 0 then 1 elsif F = 9 then 2 else 3));
      elsif F = 8 then
         pragma Assert (Calls = 0 and Forwarded = 1 and Logs = 0);
      elsif F = 7 then
         pragma Assert (Calls = 0 and Forwarded = 0 and Logs = 0);
      else
         pragma Assert (Forwarded = 0 and Ledger_Index = 0 and Ledger_Session = 0 and Logs = 1);
         pragma Assert (Private_Contexts (1).Life = Application_Lifetime.Retired);
         pragma Assert (Calls = (if F in 6 | 14 then 0 else 1));
         if F in 11 .. 13 then pragma Assert (Last_Kind = 3); end if;
         Grow_Table_Ledger;
         pragma Assert (Logs = 1 and Forwarded = 0);
         pragma Assert (Calls = (if F in 6 | 14 then 0 else 1));
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("Initial metadata owner PASS15: reference/descriptor/mirror callback revocation/remapping, failure retention, ordering and no replay");
end Test;
'''
out = Path(tempfile.mkdtemp(prefix='cubit-initial-owner.'))
(out/'test.gpr').write_text('''project P is
for Source_Dirs use ("."); for Object_Dir use "obj"; for Main use ("test.adb");
package Compiler is for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato"); end Compiler;
end P;''')
variants = {
    'native': helpers + body,
    'omit-post-owner': helpers + body.replace('if not Table_Ledger_Owner or else', 'if False or else'),
    'omit-session': helpers.replace('Intel_GPU_Render_Control.Storage_Index (Render_Admission, Ledger_Session) = Ledger_Index', 'True') + body,
    'omit-reference-post-owner': helpers + body.replace(
        'if not Table_Ledger_Owner or else\n              Context_Reference_Growth.',
        'if False or else\n              Context_Reference_Growth.'),
    'skip-reference-growth': helpers + body.replace(
        'if Context_Reference_Capacity < Application_State.Table_Pages then',
        'if False then'),
}
assert all(variant != variants['native'] for name, variant in variants.items()
           if name != 'native')
for name, variant in variants.items():
    (out/'test.adb').write_text(prefix + variant + suffix)
    build = subprocess.run(['gprbuild', '-f', '-p', '-P', str(out/'test.gpr')], capture_output=True, text=True)
    assert build.returncode == 0, (build.stderr, out)
    run = subprocess.run([str(out/'obj/test')], capture_output=True, text=True)
    (out/(name+'.log')).write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == 'native'), (name, run.stdout, run.stderr, out)
    print(name, run.returncode, run.stdout.strip())
assert source.read_bytes() == original
print('Evidence:', out)
