#!/usr/bin/env python3
"""Native bootstrap slices -> real provenance -> offline growth -> RAM writer."""
from pathlib import Path
import subprocess
import tempfile
import hashlib
import json

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("   Private_Table_Pages : constant :=")
constants = text[start:text.index("   function Table_Ledger_Busy", start)]
start = text.index("         Private_Contexts (Index).Context := Intel_GPU_Buffer_Reply.Slice")
body = text[start:text.index("         Private_Contexts (Index).Life := Application_Lifetime.Allocate", start)]
prefix = r'''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Materialize;
with Intel_GPU_PPGTT_Scratch;
with Intel_GPU_Submission_Image;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Table_Provenance;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure Bootstrap_Growth is
   package Application_VM is new Intel_GPU_VM_Image (8, 4, Bootstrap_Descriptors => 4);
   package Application_Lifetime is
      type Phase is (Offline, Retired);
   end Application_Lifetime;
   type Item is limited record
      Context, Tables, Scratch : Intel_GPU_Buffer_Reply.Backing;
      Source : Application_VM.Image;
      Life : Application_Lifetime.Phase := Application_Lifetime.Offline;
   end record;
   Private_Contexts : array (1 .. 1) of Item;
   type Page is array (Table_Index) of Unsigned_64;
   type Storage is array (0 .. 8) of Page;
   Sentinel : constant Unsigned_64 := 16#ABCDEF0123456789#;
   RAM : Storage := [others => [others => Sentinel]] with Alignment => 4096, Volatile;
   Mirror_Metadata : array (1 .. 4) of Page := [others => [others => 0]] with Alignment => 4096;
   Descriptor_Metadata : Page := [others => 0] with Alignment => 4096;
   Scratch_RAM : array (0 .. 3) of Page := [others => [others => Sentinel]]
     with Alignment => 4096, Volatile;
   Context_Bytes : constant Unsigned_64 := Intel_GPU_Submission_Image.Byte_Count;
   Parent_DMA : constant Unsigned_64 := 16#100000#;
   Child_DMA : constant Unsigned_64 := 16#300000#;
   function Owner return Boolean is (True);
   Flushes : Natural := 0;
   function Flush (CPU : Unsigned_64) return Boolean is
   begin
      Flushes := Flushes + 1;
      if Flushes <= 4 then
         pragma Assert (CPU = Unsigned_64 (To_Integer (Scratch_RAM (Flushes - 1)'Address)));
      else
         pragma Assert (CPU = Unsigned_64 (To_Integer (RAM (12 - Flushes)'Address)));
      end if;
      return True;
   end Flush;
   package Writer is new Intel_GPU_VM_Materialize (Application_VM, Owner, Flush);
   Ledger : Intel_GPU_Table_Provenance.Ledger;
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                      CPU, DMA : out Unsigned_64; OK : out Boolean) is
      Ordinal : Natural;
   begin
      OK := Session = 99 and then
        ((Ticket = 55 and then Offset >= Context_Bytes and then Offset < Context_Bytes + 4 * 4096)
         or else (Ticket = 56 and then Offset < 3 * 4096));
      CPU := 0; DMA := 0;
      if not OK then return; end if;
      if Ticket = 55 then
         Ordinal := 1 + Natural ((Offset - Context_Bytes) / 4096); DMA := Parent_DMA + Offset;
      else Ordinal := 5 + Natural (Offset / 4096); DMA := Child_DMA + Offset; end if;
      CPU := Unsigned_64 (To_Integer (RAM (Ordinal)'Address));
   end Resolve;
   package Authority is new Intel_GPU_Table_Provenance.Authority (Resolve);
   Index : constant := 1;
'''
middle = r'''
   Backing : Intel_GPU_Buffer_Reply.Backing;
   Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages;
   Initialized, OK : Boolean;
   First : Natural;
   Operation : Authority.Append_State;
   Map : Writer.Mappings := [others => (0, 0)];
   Scratch_Map : Writer.Scratch_Mappings;
   State : Writer.State;
   Root_DMA : Unsigned_64;
begin
   pragma Assert (Private_Table_Pages = 4);
   Backing := Intel_GPU_Buffer_Reply.From_Linear
     (Parent_DMA, Intel_GPU_Buffer_Backing.CPU_Base, Unsigned_64 (Private_Pages) * 4096, Parent_DMA);
   pragma Assert (Intel_GPU_Buffer_Reply.Valid (Backing));
'''
suffix = r'''
   pragma Assert (Initialized);
   pragma Assert (Private_Contexts (1).Context.Bytes = Context_Bytes);
   pragma Assert (Private_Contexts (1).Tables.Bytes = 16384 and Private_Contexts (1).Scratch.Bytes = 16384);
   pragma Assert (Intel_GPU_Buffer_Reply.Page_Address (Private_Contexts (1).Scratch, 0) =
     Parent_DMA + Context_Bytes + 16384);
   for P in 1 .. 4 loop
      pragma Assert (not Application_VM.DMA_Disjoint (Private_Contexts (1).Source,
        Parent_DMA + Context_Bytes + Unsigned_64 (P - 1) * 4096, 4096));
   end loop;
   pragma Assert (Application_VM.Backed_Tables (Private_Contexts (1).Source) = 4);
   pragma Assert (Application_VM.Metadata_Capacity (Private_Contexts (1).Source) = 4);
   Authority.Begin_Append (Operation, Ledger, 99, 1, 55, Context_Bytes, 4, OK); pragma Assert (OK);
   for P in 1 .. 4 loop Authority.Step (Operation, Ledger); end loop;
   pragma Assert (Authority.First_ID (Operation) = 1);
   Application_VM.Map_Page (Private_Contexts (1).Source, 4096, 16#900000#,
     Write_Back, Read_Write, OK); pragma Assert (OK);
   Application_VM.Map_Page (Private_Contexts (1).Source, 2 ** 39, 16#901000#,
     Write_Back, Read_Write, OK); pragma Assert (not OK);
   -- Scratch is retained in the parent and cannot be admitted as new table backing.
   Application_VM.Append_Offline_Backing (Private_Contexts (1).Source,
     [Scratch (0)], First, OK); pragma Assert (not OK);
   Application_VM.Append_Offline_Backing (Private_Contexts (1).Source,
     [Child_DMA, Child_DMA + 4096, Child_DMA + 8192], First, OK); pragma Assert (not OK);
   Application_VM.Extend_Metadata (Private_Contexts (1).Source,
     Unsigned_64 (To_Integer (Mirror_Metadata'Address)), 16384, OK); pragma Assert (OK);
   Application_VM.Append_Offline_Backing (Private_Contexts (1).Source,
     [Child_DMA, Child_DMA + 4096, Child_DMA + 8192], First, OK); pragma Assert (not OK);
   Application_VM.Extend_Descriptors (Private_Contexts (1).Source,
     Unsigned_64 (To_Integer (Descriptor_Metadata'Address)), 4096, OK); pragma Assert (OK);
   Application_VM.Append_Offline_Backing (Private_Contexts (1).Source,
     [Child_DMA, Child_DMA + 4096, Child_DMA + 8192], First, OK);
   pragma Assert (OK and First = 5);
   Authority.Rearm (Operation, Ledger, OK); pragma Assert (OK);
   Authority.Begin_Append (Operation, Ledger, 99, 1, 56, 0, 3, OK); pragma Assert (OK);
   for P in 1 .. 3 loop Authority.Step (Operation, Ledger); end loop;
   pragma Assert (Authority.First_ID (Operation) = 5);
   Application_VM.Map_Page (Private_Contexts (1).Source, 2 ** 39, 16#901000#,
     Write_Back, Read_Write, OK); pragma Assert (OK);
   pragma Assert (Application_VM.Used (Private_Contexts (1).Source) = 7);
   for P in 1 .. 7 loop
      declare M : constant Intel_GPU_Table_Provenance.Mapping := Authority.Lookup (Ledger, 99, 1, P); begin
         pragma Assert (M.Ticket = (if P <= 4 then 55 else 56));
         Map (P) := (M.CPU, M.DMA);
      end;
   end loop;
   Application_VM.Seal (Private_Contexts (1).Source, OK); pragma Assert (OK);
   for L in Scratch_Map'Range loop
      Scratch_Map (L) := (Unsigned_64 (To_Integer (Scratch_RAM (L)'Address)), Scratch (L));
   end loop;
   Writer.Prepare (State, Private_Contexts (1).Source, Map, Root_DMA, OK, Scratch_Map);
   pragma Assert (OK and Flushes = 11 and Root_DMA = Parent_DMA + Context_Bytes);
   for I in Table_Index loop
      pragma Assert (RAM (0) (I) = Sentinel and RAM (8) (I) = Sentinel);
      for L in Scratch_Map'Range loop
         pragma Assert (Scratch_RAM (L) (I) =
           (if L = 0 then 0 else Application_VM.Scratch_Entry (Private_Contexts (1).Source, L)));
      end loop;
      for P in 1 .. 7 loop
         pragma Assert (RAM (P) (I) = Application_VM.Entry_Value (Private_Contexts (1).Source, P, I));
      end loop;
   end loop;
   pragma Assert (Application_VM.Lookup (Private_Contexts (1).Source, 4096) = Encode_Leaf (16#900000#, Write_Back, Read_Write));
   pragma Assert (Application_VM.Lookup (Private_Contexts (1).Source, 2 ** 39) = Encode_Leaf (16#901000#, Write_Back, Read_Write));
   Ada.Text_IO.Put_Line ("Bootstrap growth PASS: native slices, parent/child provenance, scratch exclusion, seven-page RAM publication");
end Bootstrap_Growth;
'''
out = Path(tempfile.mkdtemp(prefix="cubit-bootstrap-growth."))
(out / "test.gpr").write_text(f'''project Test is
for Source_Dirs use (".", "{root / 'userspace/services/intel-gpu'}");
for Object_Dir use "obj";
for Main use ("bootstrap_growth.adb");
package Compiler is
for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
end Compiler;
end Test;
''')
variants = {"native": body, "overlap-scratch": body.replace("Intel_GPU_Submission_Image.Byte_Count + Private_Table_Pages * 4096", "Intel_GPU_Submission_Image.Byte_Count"),
            "omit-backed-count": body.replace("Backing_Count => Private_Table_Pages", "Backing_Count => 8")}
for name, variant in variants.items():
    assert name == "native" or variant != body
    (out / "bootstrap_growth.adb").write_text(prefix + constants + middle + variant + suffix)
    build = subprocess.run(["gprbuild", "-f", "-p", "-P", str(out / "test.gpr")], capture_output=True, text=True)
    (out / f"{name}-build.log").write_text(build.stdout + build.stderr)
    assert build.returncode == 0, (name, build.stderr, out)
    run = subprocess.run([str(out / "obj/bootstrap_growth")], capture_output=True, text=True)
    (out / f"{name}-run.log").write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == "native"), (name, run.stdout, run.stderr, out)
    print(name, run.returncode, run.stdout.strip())
assert source.read_bytes() == original
(out / "evidence.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(), "variants": list(variants)}, indent=2))
print("Evidence:", out)
