#!/usr/bin/env python3
"""Exercise production ordinal-to-provenance resolution with mocked authority."""
from pathlib import Path
import hashlib
import json
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / 'userspace/services/intel-gpu/main.adb'
original = source.read_bytes()
text = original.decode()
start = text.index('   function Initial_Table_Mapping (Index : Positive; Session : Unsigned_64;',
                   text.index('   end Grow_Table_Ledger;'))
end = text.index('   end Replacement_Table_Mapping;', start) + len('   end Replacement_Table_Mapping;')
body = text[start:end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_References;
procedure Reference_Test is
   package R is new Intel_GPU_Table_References (8);
   package Application_VM is subtype Page_Number is Positive range 1 .. 8; end;
   package Intel_GPU_Buffer_Backing is subtype Slot is Positive; end;
   package Intel_GPU_Table_Provenance is
      type Mapping is record Ticket, Offset, CPU, DMA : Unsigned_64 := 0; end record;
   end;
   type Context is limited record
      Table_Owners : Boolean := True;
      Table_Generation : Unsigned_64 := 9;
      Table_IDs : R.Map;
   end record;
   Private_Contexts : array (1 .. 2) of Context;
   package Application_State is
      package Table_References renames R;
      subtype Update_Record is Context;
      type Update_Access is access all Update_Record;
      Item : aliased Update_Record;
      function Has_Update (Slot : Positive) return Boolean is (Slot = 3);
      function Updates (Slot : Positive) return Update_Access is (Item'Access);
   end;
   Calls : Natural := 0;
   Revoked : Boolean := False;
   package Table_Authority is
      function Lookup (Object : Boolean; Session, Generation : Unsigned_64;
                       Index : Positive) return Intel_GPU_Table_Provenance.Mapping;
   end;
   package body Table_Authority is
      function Lookup (Object : Boolean; Session, Generation : Unsigned_64;
                       Index : Positive) return Intel_GPU_Table_Provenance.Mapping is
      begin
         Calls := Calls + 1;
         if not Object or Revoked or Session /= 42 or Generation /= 9 then
            return (others => 0);
         end if;
         return (Unsigned_64 (Index), 0, 4096, Unsigned_64 (Index) * 4096);
      end;
   end;
   M : Intel_GPU_Table_Provenance.Mapping;
   type Bytes is array (1 .. 4096) of Unsigned_8;
   Initial_RAM, Replacement_RAM : Bytes := [others => 0] with Alignment => 4096;
   OK : Boolean;
'''
suffix = '''
begin
   -- Model independently confirmed retirement; the map grants no authority.
   for E in Unsigned_64 range 1 .. 8 loop
      R.Reopen (Private_Contexts (1).Table_IDs, E, E + 1, OK);
      pragma Assert (OK);
      R.Reopen (Application_State.Item.Table_IDs, E, E + 1, OK);
      pragma Assert (OK);
   end loop;
   R.Put (Private_Contexts (1).Table_IDs, 9, 5, 65, OK);
   pragma Assert (not OK);
   M := Initial_Table_Mapping (1, 42, 5); pragma Assert (M.Ticket = 0);
   pragma Assert (Calls = 0);
   R.Extend (Private_Contexts (1).Table_IDs,
     Unsigned_64 (To_Integer (Initial_RAM'Address)), 4096, OK);
   pragma Assert (OK);
   R.Extend (Application_State.Item.Table_IDs,
     Unsigned_64 (To_Integer (Replacement_RAM'Address)), 4096, OK);
   pragma Assert (OK);
   -- The adopted page is ordinal5 but immutable provenance ID65.
   R.Put (Private_Contexts (1).Table_IDs, 9, 5, 65, OK);
   pragma Assert (OK);
   R.Put (Application_State.Item.Table_IDs, 9, 5, 97, OK);
   pragma Assert (OK);
   M := Initial_Table_Mapping (1, 42, 5); pragma Assert (M.Ticket = 65);
   M := Replacement_Table_Mapping (3, 42, 5); pragma Assert (M.Ticket = 97);
   pragma Assert (Calls = 2);
   M := Initial_Table_Mapping (3, 42, 5); pragma Assert (M.Ticket = 0);
   M := Initial_Table_Mapping (1, 42, 9); pragma Assert (M.Ticket = 0);
   M := Initial_Table_Mapping (1, 42, 4); pragma Assert (M.Ticket = 0);
   M := Replacement_Table_Mapping (2, 42, 5); pragma Assert (M.Ticket = 0);
   M := Replacement_Table_Mapping (3, 42, 9); pragma Assert (M.Ticket = 0);
   M := Replacement_Table_Mapping (3, 42, 4); pragma Assert (M.Ticket = 0);
   pragma Assert (Calls = 2); -- absent references never reach authority
   M := Initial_Table_Mapping (1, 43, 5); pragma Assert (M.Ticket = 0);
   M := Replacement_Table_Mapping (3, 43, 5); pragma Assert (M.Ticket = 0);
   pragma Assert (Calls = 4);
   Private_Contexts (1).Table_Generation := 8;
   Application_State.Item.Table_Generation := 8;
   M := Initial_Table_Mapping (1, 42, 5); pragma Assert (M.Ticket = 0);
   M := Replacement_Table_Mapping (3, 42, 5); pragma Assert (M.Ticket = 0);
   pragma Assert (Calls = 4); -- stale captured generations stop before authority
   Private_Contexts (1).Table_Generation := 9;
   Application_State.Item.Table_Generation := 9;
   Revoked := True;
   M := Initial_Table_Mapping (1, 42, 5); pragma Assert (M.Ticket = 0);
   M := Replacement_Table_Mapping (3, 42, 5); pragma Assert (M.Ticket = 0);
   pragma Assert (Calls = 6);
   Revoked := False;
   R.Reopen (Application_State.Item.Table_IDs, 9, 10, OK);
   pragma Assert (OK);
   M := Replacement_Table_Mapping (3, 42, 5); pragma Assert (M.Ticket = 0);
   Application_State.Item.Table_Generation := 10;
   M := Replacement_Table_Mapping (3, 42, 5); pragma Assert (M.Ticket = 0);
   pragma Assert (Calls = 6); -- reopen hides prior generation's retained entries
   Ada.Text_IO.Put_Line ("Table references PASS: growth, nonidentity IDs, missing references, stale generation, reopen and revoked ownership");
end Reference_Test;
'''
work = Path(tempfile.mkdtemp(prefix='cubit-table-resolution.'))
bad = body.replace('Application_State.Table_References.Get\n'
                   '             (Item.Table_IDs, Item.Table_Generation, Page)', 'Page')
assert bad != body
results = {}
for name, implementation in [('native', body), ('ordinal-as-id', bad)]:
    variant = work / name
    variant.mkdir()
    (variant / 'reference_test.adb').write_text(prefix + implementation + suffix)
    with (variant / 'build.log').open('w') as log:
        subprocess.run(['gnatmake', '-q', '-gnat2022', '-gnata', '-gnato',
                        '-I' + str(root / 'userspace/services/intel-gpu'),
                        'reference_test.adb'], cwd=variant, stdout=log,
                       stderr=subprocess.STDOUT, check=True)
    result = subprocess.run([str(variant / 'reference_test')],
                            text=True, capture_output=True)
    (variant / 'run.log').write_text(result.stdout + result.stderr)
    assert (result.returncode == 0) == (name == 'native'), (name, result)
    results[name] = {'returncode': result.returncode, 'stdout': result.stdout}
assert source.read_bytes() == original
(work / 'result.json').write_text(json.dumps({
    'source_sha256': hashlib.sha256(original).hexdigest(), 'variants': results}))
print(results['native']['stdout'])
print('Ordinal-as-ID negative control rejected; evidence:', work)
