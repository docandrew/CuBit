from pathlib import Path
import subprocess
import hashlib
import json
import tempfile

# Extract the production routine; mock only its external dependencies.
root = Path(__file__).resolve().parents[2]
output_root = root / 'tests/intel-gpu/build'
output_root.mkdir(parents=True, exist_ok=True)
work = Path(tempfile.mkdtemp(prefix='teardown-gate-', dir=output_root))
source = root / 'userspace/services/intel-gpu/main.adb'
original = source.read_bytes()
text = original.decode()
start = text.index('   function Teardown_Buffer_Owner (')
end = text.index('   end Teardown_Buffer_Owner;', start) + len('   end Teardown_Buffer_Owner;')
body = text[start:end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
procedure Gate_Test is
   Fault, Metadata_Calls : Natural := 0;
   Runtime_Fault, Render_Backend_Ready : Boolean;
   function Context_Owner return Boolean is (Fault /= 2);
   package Intel_GPU_Render_Sessions is subtype Slot_Index is Natural range 0 .. 2; end;
   Render_Admission : Boolean := True;
   package Intel_GPU_Render_Control is
      function Storage_Index (Object : Boolean; Session : Unsigned_64)
        return Intel_GPU_Render_Sessions.Slot_Index is (if Fault = 4 then 0 else 1);
      function Issued_Tag (Object : Boolean; Index : Natural) return Unsigned_64 is
        (if Fault = 6 then 102 else 101);
   end;
   package Application_Lifetime is type Phase is (Active, Retired); end;
   use type Application_Lifetime.Phase;
   type Backing is record Ready : Boolean := False; end record;
   type Context is record
      Life : Application_Lifetime.Phase := Application_Lifetime.Retired;
      Parent : Backing;
      Source : Natural := 1;
   end record;
   Private_Contexts : array (1 .. 2) of Context;
   Context_Retirement_Attempted : array (1 .. 2) of Boolean := [others => True];
   Current_Table_Ticket : array (1 .. 2) of Unsigned_64 := [others => 0];
   package Live_Snapshots is
      function Retired (Source : Natural) return Boolean is (Fault /= 10);
   end;
   package Image_Retirement is type Result is (Rejected, Address_Released); end;
   Image_Retirement_Results : array (1 .. 2) of Image_Retirement.Result;
   Contexts, Application_Map_State, Application_Buffer_State : Boolean := True;
   package Context_Drain is
      type Retirement_State is (Deregistered, Uncertain);
      function Observe (Object : Boolean; Session : Unsigned_64) return Retirement_State is
        (if Fault = 13 then Uncertain else Deregistered);
   end;
   package Application_Maps is
      type Retirement_State is (Clear, Uncertain);
      function Observe_Retirement (Object : Boolean; Session : Unsigned_64) return Retirement_State is
        (if Fault = 14 then Uncertain else Clear);
   end;
   package Application_Buffers is
      function Can_Retire (Object : Boolean; Session, Ticket : Unsigned_64) return Boolean;
   end;
   package body Application_Buffers is
      function Can_Retire (Object : Boolean; Session, Ticket : Unsigned_64) return Boolean is
      begin
         Metadata_Calls := Metadata_Calls + 1;
         return Fault /= 15 and Session = 101 and Ticket = 55;
      end;
   end;
   Session, Ticket : Unsigned_64;
'''
suffix = '''
begin
   for Case_ID in 0 .. 17 loop
      Fault := Case_ID; Metadata_Calls := 0;
      Runtime_Fault := Fault = 1; Render_Backend_Ready := Fault /= 3;
      Session := (if Fault = 5 then 0 elsif Fault = 17 then 102 else 101);
      Ticket := (if Fault = 16 then 56 else 55);
      Private_Contexts := [others => (others => <>)];
      if Fault = 7 then Private_Contexts (1).Life := Application_Lifetime.Active; end if;
      Private_Contexts (1).Parent.Ready := Fault = 8;
      Context_Retirement_Attempted := [others => Fault /= 9];
      Current_Table_Ticket := [others => (if Fault = 11 then 55 else 0)];
      Image_Retirement_Results := [others =>
        (if Fault = 12 then Image_Retirement.Rejected else Image_Retirement.Address_Released)];
      pragma Assert (Teardown_Buffer_Owner (Session, Ticket) = (Fault = 0));
      pragma Assert (Metadata_Calls = (if Fault in 0 | 15 | 16 then 1 else 0));
   end loop;
   Ada.Text_IO.Put_Line ("Teardown gate PASS18: each missing prerequisite rejects before reuse eligibility");
end Gate_Test;
'''
(work/'gate_test.adb').write_text(prefix+body+suffix)
(work/'gate.gpr').write_text('''project Gate is
 for Source_Dirs use (".");
 for Object_Dir use "gate-obj";
 for Main use ("gate_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Gate;
''')
subprocess.run(['gprbuild','-p','-P',str(work/'gate.gpr')],check=True)
result=subprocess.run([str(work/'gate-obj/gate_test')],text=True,capture_output=True)
assert result.returncode==0,(result.stdout,result.stderr)
assert source.read_bytes()==original
(work/'gate-result.json').write_text(json.dumps({'source_sha256':hashlib.sha256(original).hexdigest(),'stdout':result.stdout}))
print(result.stdout)
