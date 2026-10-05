#!/usr/bin/env python3
"""Hosted extraction of native capture admission; not GPU visibility proof."""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / 'userspace/services/intel-gpu/main.adb'
original = source.read_text()
start = original.index('      Ticket : Application_Buffers.Ticket;', original.index('   procedure Capture_Removal'))
end = original.index('      Removal_Backing := Backing;', start)
helpers = original[original.index('   function Captured_Mapping_Ready'):original.index('   procedure Remove_Leaf', original.index('   function Captured_Mapping_Ready'))]
capture_type = original[original.index('   type Mapping_Capture is record'):original.index('   Removal_GPU,', original.index('   type Mapping_Capture is record'))]
body = helpers + '   procedure Capture (Revision : Unsigned_64; Accepted : out Boolean) is\n' + original[start:end] + '      Accepted := Captured_Mapping_Ready;\n   end Capture;\n'
prefix = r'''
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Table_Provenance;
procedure Capture_Test is
   package Application_VM is new Intel_GPU_VM_Image (8);
   package Application_Buffers is
      subtype Ticket is Unsigned_64;
      function Ticket_Slot (T : Ticket) return Positive is (1);
   end Application_Buffers;
   package Intel_GPU_Buffer_Backing is subtype Slot is Positive; end;
begin
 for Case_ID in 0 .. 17 loop
  declare
   type Context is limited record
      Source : Application_VM.Image;
      Table_Generation : Unsigned_64 := 1;
   end record;
   Private_Contexts : array (1 .. 1) of Context;
   Update_Index : constant Positive := 1;
   Update_Session : constant Unsigned_64 := 99;
   Update_Identity : Unsigned_64 := 123;
   Owner : Boolean := True;
   In_Place_Active : Boolean := True;
   function Update_Exclusive return Boolean is (Owner);
   Current_Table_Ticket : array (1 .. 1) of Unsigned_64 :=
     [1 => (if Case_ID in 1 | 6 .. 9 then 55 else 0)];
   type Replacement_Record is record
      Ticket : Unsigned_64 := 55;
      Session : Unsigned_64 := (if Case_ID = 9 then 100 else 99);
      Root : Unsigned_64 := (if Case_ID = 6 then 8192 else 4096);
      Superseded : Boolean := Case_ID = 7;
   end record;
   Replacement_Tables : Natural := 0;
   package Replacement_Records is
      function Get (Unused : Natural; Slot : Positive) return Replacement_Record is ((others => <>));
   end;
   package Application_State is
      function Has_Update (Slot : Positive) return Boolean is (Case_ID /= 8);
      type Update_Record is record Table_Generation : Unsigned_64 := 1; end record;
      Updates : array (1 .. 1) of Update_Record;
   end;
   Calls : Natural := 0;
   Mutate_Lookup : Boolean := False;
   function Initial_Table_Mapping (Index : Positive; Session : Unsigned_64;
      P : Positive) return Intel_GPU_Table_Provenance.Mapping is
   begin
      pragma Assert (Index = 1 and Session = 99);
      Calls := Calls + 1;
      if Case_ID = 5 and P = 2 then Owner := False; end if;
      if Mutate_Lookup then
         if Case_ID = 16 then Owner := False; end if;
         if Case_ID = 17 then Private_Contexts (1).Table_Generation := 2; end if;
      end if;
      return (Ticket => (if Case_ID = 4 and P = 2 then 0 else 55),
              Offset => Unsigned_64 (P - 1) * 4096,
              CPU => 16#100000# + Unsigned_64 (P) * 4096,
              DMA => Unsigned_64 (P) * 4096 + (if Case_ID = 3 and P = 2 then 4096 else 0));
   end;
   function Replacement_Table_Mapping (Index : Positive; Session : Unsigned_64;
      P : Positive) return Intel_GPU_Table_Provenance.Mapping renames Initial_Table_Mapping;
'''
suffix = r'''
   OK : Boolean;
   M : Intel_GPU_Table_Provenance.Mapping;
  begin
   Application_VM.Initialize (Private_Contexts (1).Source,
     [4096, 8192, 12288, 16384, others => 0], OK, Backing_Count => 4);
   pragma Assert (OK);
   Application_VM.Map_Page (Private_Contexts (1).Source, 4096, 16#200000#,
     Write_Back, Read_Write, OK); pragma Assert (OK);
   if Case_ID /= 2 then
      Application_VM.Seal (Private_Contexts (1).Source, OK); pragma Assert (OK);
   end if;
   Capture (Application_VM.Revision (Private_Contexts (1).Source), OK);
   pragma Assert (OK = (Case_ID in 0 .. 1 | 10 .. 17));
   pragma Assert (Calls = (if Case_ID in 0 .. 1 | 10 .. 17 then 4 elsif Case_ID in 3 .. 5 then 2 else 0));
   if OK then
      -- The leaf lookup must authenticate again, not use a captured address.
      M := Captured_Leaf_Mapping (16384);
      pragma Assert (M.Ticket /= 0 and M.DMA = 16384 and Calls = 5);
      M := Captured_Leaf_Mapping (4096); -- root is not a leaf
      pragma Assert (M.Ticket = 0 and Calls = 5);
      case Case_ID is
         when 10 => Private_Contexts (1).Table_Generation := 2;
         when 11 => Update_Identity := 124;
         when 12 => Current_Table_Ticket (1) := 55;
         when 13 => Removal_Capture.Root := 8192;
         when 14 => Removal_Capture.Revision := Removal_Capture.Revision + 1;
         when 15 => Owner := False;
         when 16 .. 17 => Mutate_Lookup := True;
         when others => null;
      end case;
      M := Captured_Leaf_Mapping (16384);
      pragma Assert ((M.Ticket /= 0) = (Case_ID in 0 .. 1));
      pragma Assert (Calls = (if Case_ID in 0 .. 1 | 16 .. 17 then 6 else 5));
   end if;
  end;
 end loop;
 Ada.Text_IO.Put_Line ("Native mapping capture PASS18: authenticated per-leaf lookup, sealed image, callback owner/generation loss and stale capture rejection");
end Capture_Test;
'''
out = Path(tempfile.mkdtemp(prefix='cubit-mapping-capture.'))
variants = {
    'native': body,
    'omit-owner': body.replace('not Update_Exclusive or else', 'False or else'),
    'omit-root': body.replace('Saved.Root /= Application_VM.Root_DMA (Private_Contexts (Update_Index).Source)', 'False').replace('Saved.Root = Removal_Capture.Root', 'True'),
    'omit-recheck': body.replace('M.Ticket = 0 or else not Captured_Mapping_Ready or else', 'M.Ticket = 0 or else'),
    'omit-generation': body.replace('Removal_Capture.Generation = Private_Contexts (Update_Index).Table_Generation', 'True'),
}
for name, variant in variants.items():
    work = out / name
    work.mkdir()
    (work / 'capture_test.adb').write_text(prefix + capture_type + variant + suffix)
    with (work / 'build.log').open('w') as log:
        subprocess.run(['gnatmake', '-q', '-gnat2022', '-gnata', '-gnato', '-O2',
                        '-I' + str(root / 'userspace/services/intel-gpu'), 'capture_test.adb'],
                       cwd=work, stdout=log, stderr=log, check=True)
    run = subprocess.run([str(work / 'capture_test')], capture_output=True, text=True)
    print(name, run.returncode, run.stdout.strip(), flush=True)
    (work / 'run.log').write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == 'native'), name
assert source.read_text() == original, 'source changed during test'
print('Evidence:', out)
