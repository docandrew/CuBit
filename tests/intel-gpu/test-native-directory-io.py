#!/usr/bin/env python3
"""Actual native directory address adapter, real provenance and CPU word access."""
from pathlib import Path
import hashlib
import json
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("   type Directory_Phase is")
end = text.index("   package Directory_Backing is", start)
body = text[start:end]
start = text.rindex("   function Initial_Table_Mapping (")
end = text.index("   end Initial_Table_Mapping;", start) + len("   end Initial_Table_Mapping;")
mapping = text[start:end]
prefix = r'''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.IO;
with Intel_GPU_Table_References;
procedure Native_Directory_IO_Test is
   package P renames Intel_GPU_Table_Provenance;
   type Words is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
   type Pages is array (1 .. 68) of Words;
   RAM : Pages := [others => [others => 16#CAFE#]] with Alignment => 4096, Volatile;
   type Bytes is array (1 .. 4096) of Unsigned_8;
   Metadata : Bytes := [others => 0] with Alignment => 4096;
   References : Bytes := [others => 0] with Alignment => 4096;
   Owner, Revoke, Move_DMA, Lose_During_Resolve, Lose_During_Flush : Boolean := False;
   Flush_OK : Boolean := True;
   Flushes : Natural := 0;
   package Application_VM is
      subtype Page_Number is Positive range 1 .. 64;
      type Image is record Count : Natural := 4; end record;
      function Used (Source : Image) return Natural is (Source.Count);
   end Application_VM;
   package Application_State is
      package Table_References is new Intel_GPU_Table_References (64);
   end;
   package R renames Application_State.Table_References;
   type Item is limited record
      Source : Application_VM.Image;
      Table_Owners : P.Ledger;
      Table_Generation : Unsigned_64 := 1;
      Table_IDs : R.Map;
   end record;
   Private_Contexts : array (1 .. 1) of Item;
   Current_Table_Ticket : array (1 .. 1) of Unsigned_64 := [others => 0];
   Update_Index : Natural := 1;
   Update_Session : Unsigned_64 := 7;
   Update_Pending : Unsigned_64 := 56;
   Update_Table_Pages : Natural := 3;
   function Update_Exclusive return Boolean is
     (Owner and then Update_Index in Private_Contexts'Range and then Update_Pending /= 0);
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                      CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
      Page : Natural;
   begin
      CPU := 0; DMA := 0; Accepted := False;
      if Session /= 7 or Revoke or Offset mod 4096 /= 0 or
        Ticket not in 55 .. 57 or Offset / 4096 >=
          (if Ticket = 55 then 64 elsif Ticket = 56 then 3 else 1)
      then return; end if;
      Page := (if Ticket = 55 then 1 elsif Ticket = 56 then 65 else 68) + Natural (Offset / 4096);
      CPU := Unsigned_64 (To_Integer (RAM (Page)'Address));
      DMA := Unsigned_64 (Page) * 4096 + (if Move_DMA then 4096 else 0);
      Accepted := True;
      if Lose_During_Resolve then Owner := False; end if;
   end Resolve;
   package Table_Authority is new P.Authority (Resolve);
   package Intel_GPU_DMA_Cache is
      function Flush_Range (CPU, Length : Unsigned_64) return Boolean;
   end Intel_GPU_DMA_Cache;
   package body Intel_GPU_DMA_Cache is
      function Flush_Range (CPU, Length : Unsigned_64) return Boolean is
      begin
         pragma Assert (Length = 4096 and CPU >= Unsigned_64 (To_Integer (RAM'Address)) and
           CPU < Unsigned_64 (To_Integer (RAM'Address)) + 68 * 4096);
         Flushes := Flushes + 1;
         if Lose_During_Flush then Owner := False; end if;
         return Flush_OK;
      end;
   end Intel_GPU_DMA_Cache;
'''
suffix = r'''
   OK : Boolean;
   Value : Unsigned_64;
   Saved_Flushes : Natural;
   procedure Reject (DMA : Unsigned_64) is
      Before : constant Pages := RAM;
      Prior_Flushes : constant Natural := Flushes;
   begin
      Read_Directory_Word (DMA, 9, Value, OK); pragma Assert (not OK and Value = 0);
      Write_Directory_Word (DMA, 9, 16#BAD#, OK); pragma Assert (not OK);
      pragma Assert (not Flush_Directory_Page (DMA));
      pragma Assert (RAM = Before and Flushes = Prior_Flushes);
   end Reject;
begin
   for I in 1 .. 4 loop
      R.Put (Private_Contexts (1).Table_IDs, 1, I, I, OK);
      pragma Assert (OK);
   end loop;
   P.Extend (Private_Contexts (1).Table_Owners,
     Unsigned_64 (To_Integer (Metadata'Address)), 4096, OK); pragma Assert (OK);
   for I in 1 .. 68 loop
      Table_Authority.Install (Private_Contexts (1).Table_Owners, 7, 1, I,
        (if I <= 64 then 55 elsif I <= 67 then 56 else 57),
        Unsigned_64 (if I <= 64 then I - 1 elsif I <= 67 then I - 65 else 0) * 4096, OK);
      pragma Assert (OK);
   end loop;
   Owner := True;
   Directory_Update := Publish_Directories; Directory_First_ID := 65;
   for I in 1 .. 4 loop pragma Assert (Directory_Table_ID (Unsigned_64 (I) * 4096) = I); end loop;
   for I in 65 .. 67 loop pragma Assert (Directory_Table_ID (Unsigned_64 (I) * 4096) = I); end loop;
   -- Reserved initial allocation pages are not live directory authority.
   Reject (5 * 4096); Reject (64 * 4096); Reject (0); Reject (999 * 4096);
   -- An adjacent ledger record belonging to another ticket is not staged authority.
   Update_Table_Pages := 4; Reject (68 * 4096); Update_Table_Pages := 3;
   Update_Pending := 57; Reject (65 * 4096); Update_Pending := 56;
   Directory_First_ID := 0; Reject (65 * 4096); Directory_First_ID := 65;
   Write_Directory_Word (65 * 4096, 9, 16#123456#, OK); pragma Assert (OK);
   pragma Assert (RAM (65) (9) = 16#123456# and RAM (5) (9) = 16#CAFE#);
   Read_Directory_Word (65 * 4096, 9, Value, OK); pragma Assert (OK and Value = 16#123456#);
   pragma Assert (Flush_Directory_Page (65 * 4096) and Flushes = 1);
   Update_Session := 8; Reject (4096); Reject (65 * 4096); Update_Session := 7;
   Private_Contexts (1).Table_Generation := 2; Reject (4096); Reject (65 * 4096);
   Private_Contexts (1).Table_Generation := 1;
   Current_Table_Ticket (1) := 99; Reject (4096); Current_Table_Ticket (1) := 0;
   Revoke := True; Reject (4096); Reject (65 * 4096); Revoke := False;
   Move_DMA := True; Reject (4096); Reject (65 * 4096); Move_DMA := False;
   Owner := False; Reject (4096); Owner := True;
   Directory_Update := No_Directory_Update; Reject (4096); Directory_Update := Publish_Directories;
   Update_Index := 0; Reject (4096); Update_Index := 1;
   -- Exclusion may disappear inside an authority callback; actual IO rechecks.
   Lose_During_Resolve := True; Reject (65 * 4096);
   Lose_During_Resolve := False; Owner := True;
   Flush_OK := False; Saved_Flushes := Flushes;
   pragma Assert (not Flush_Directory_Page (4096) and Flushes = Saved_Flushes + 1);
   Flush_OK := True; Lose_During_Flush := True;
   pragma Assert (not Flush_Directory_Page (4096) and not Owner);
   Lose_During_Flush := False; Owner := True;
   -- After adoption, image ordinal5 means ledger65, not reserved ledger5.
   Private_Contexts (1).Source.Count := 7;
   Directory_First_ID := 0; Update_Table_Pages := 0;
   R.Put (Private_Contexts (1).Table_IDs, 1, 5, 65, OK);
   pragma Assert (not OK); -- missing map capacity grants no IO authority
   Reject (65 * 4096);
   R.Extend (Private_Contexts (1).Table_IDs,
     Unsigned_64 (To_Integer (References'Address)), 4096, OK);
   pragma Assert (OK);
   for I in 5 .. 7 loop
      R.Put (Private_Contexts (1).Table_IDs, 1, I, I + 60, OK);
      pragma Assert (OK);
   end loop;
   pragma Assert (Directory_Table_ID (65 * 4096) = 65);
   Read_Directory_Word (65 * 4096, 9, Value, OK); pragma Assert (OK and Value = 16#123456#);
   Reject (5 * 4096);
   Directory_Invalidated := True; pragma Assert (Directory_TLB_Confirmed);
   Owner := False; pragma Assert (not Directory_TLB_Confirmed);
   Owner := True;
   R.Reopen (Private_Contexts (1).Table_IDs, 1, 2, OK);
   pragma Assert (OK);
   Reject (4096); Reject (65 * 4096); -- old captured epoch cannot use retained IDs
   Ada.Text_IO.Put_Line ("Native directory IO PASS: real volatile RAM and provenance; reserved/staged IDs, owner/generation, callback loss, adopted ordinals");
end Native_Directory_IO_Test;
'''
out = Path(tempfile.mkdtemp(prefix="cubit-native-directory-io."))
project = out / "test.gpr"
project.write_text(f'''project Test is
for Source_Dirs use ("{out}", "{root / 'userspace/services/intel-gpu'}");
for Object_Dir use "obj";
for Main use ("native_directory_io_test.adb");
package Compiler is
for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
end Compiler;
end Test;
''')
variants = {
    "native": body,
    "wrong-staged-ticket": body.replace("M.Ticket = Update_Pending", "M.Ticket /= 0"),
    "image-ordinal-as-id": body.replace(
        "return Application_State.Table_References.Get\n"
        "              (Private_Contexts (Update_Index).Table_IDs,\n"
        "               Private_Contexts (Update_Index).Table_Generation, P);", "return P;"),
}
assert all(variant != body for name, variant in variants.items() if name != "native")
for name, variant in variants.items():
    (out / "native_directory_io_test.adb").write_text(prefix + mapping + variant + suffix)
    built = subprocess.run(["gprbuild", "-f", "-p", "-P", str(project)], capture_output=True, text=True)
    (out / f"{name}-build.log").write_text(built.stdout + built.stderr)
    if built.returncode:
        raise SystemExit(f"compile failed: {out}/{name}-build.log\n{built.stdout}{built.stderr}")
    run = subprocess.run([str(out / "obj/native_directory_io_test")], capture_output=True, text=True)
    (out / f"{name}-run.log").write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == "native"), (name, run.stdout, run.stderr, out)
    print(name, run.returncode, run.stdout.strip())
assert source.read_bytes() == original
(out / "evidence.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(), "variants": list(variants)}, indent=2))
print("Evidence:", out)
