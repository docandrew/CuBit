"""Cross-language layout and native publication checks. Run inside Nix."""
from pathlib import Path
import hashlib
import json
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
native = root / "userspace/servo/native"
rust = root / "userspace/servo/overlay/ports/cubitshell/src"
out = Path(tempfile.mkdtemp(prefix="tab-projection-", dir=root / "tests/servo/build"))
inputs = [native / "servo_tab_projection.ads", native / "servo_tab_projection.adb",
          rust / "tab_model.rs", rust / "tab_projection.rs"]
hashes = {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs}
(out / "inputs.json").write_text(json.dumps(hashes, indent=2) + "\n")
(out / "writer.rs").write_text('''
#[path = "''' + str(rust / "tab_model.rs") + '''"] mod tab_model;
use std::io::Write;
fn main() {
    let mut tabs = tab_model::Tabs::default();
    for _ in 0..10_000 { tabs.insert("Penny tab").unwrap(); }
    let snapshot = tabs.snapshot(3, |value| value);
    // Every field has a defined byte representation and the production layout
    // asserts that there are no implicit padding bytes in either C structure.
    let bytes = unsafe { std::slice::from_raw_parts(
        &snapshot as *const _ as *const u8, std::mem::size_of_val(&snapshot)) };
    std::io::stdout().write_all(bytes).unwrap();
}
''')
(out / "projection_tests.adb").write_text('''
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Servo_Tab_Projection; use Servo_Tab_Projection;
with Interfaces; use Interfaces;
procedure Projection_Tests is
   package IO renames Ada.Streams.Stream_IO;
   File : IO.File_Type;
   Bytes : aliased Stream_Element_Array (1 .. 2584) with Alignment => 8;
   Last : Stream_Element_Offset;
   Incoming : Snapshot with Import, Address => Bytes'Address;
   Current, Bad, Previous : Snapshot;
   Accepted : Boolean;
   procedure Reject (Value : Snapshot) is
   begin
      Previous := Current;
      Publish (Current, Value, Accepted);
      pragma Assert (not Accepted and Current = Previous);
   end Reject;
begin
   pragma Assert (Row'Size = 640 and Snapshot'Size = 20_672);
   IO.Open (File, IO.In_File, "snapshot.bin");
   IO.Read (File, Bytes, Last);
   pragma Assert (Last = Bytes'Last and IO.End_Of_File (File));
   IO.Close (File);
   pragma Assert (Valid (Current) and Valid (Incoming));
   pragma Assert (Incoming.Total = 10_000 and Incoming.Active = 10_000);
   pragma Assert (Incoming.Count = 3 and Active_Row (Incoming) = 3);
   pragma Assert (ID_At (Incoming, 1) = 9_998 and ID_At (Incoming, 3) = 10_000);
   pragma Assert (ID_At (Incoming, 0) = 0 and ID_At (Incoming, 4) = 0);
   pragma Assert (ID_At (Incoming, Natural'Last) = 0);
   pragma Assert (Incoming.Items (1).Length = 9 and Incoming.Items (1).Text (1) = 80);
   Publish (Current, Incoming, Accepted);
   pragma Assert (Accepted and Current = Incoming);
   Bad := Incoming; Bad.Items (1).Text (1) := 81;
   pragma Assert (Same_Mapping (Current, Bad));
   Bad.Items (1).ID := 9_997;
   pragma Assert (not Same_Mapping (Current, Bad));
   Bad := Incoming; Bad.Count := 33; Reject (Bad);
   Bad := Incoming; Bad.Count := Unsigned_32'Last; Reject (Bad);
   Bad := Incoming; Bad.Total := 2; Reject (Bad);
   Bad := Incoming; Bad.Active := 1; Reject (Bad);
   Bad := Incoming; Bad.Active := 0; Reject (Bad);
   Bad := Incoming; Bad.Reserved := 1; Reject (Bad);
   for I in Slot loop
      Bad := Incoming; Bad.Items (I).Reserved := 1; Reject (Bad);
      Bad := Incoming; Bad.Items (I).Length := 65; Reject (Bad);
      Bad := Incoming; Bad.Items (I).Text (64) := 1; Reject (Bad);
      Bad := Incoming; Bad.Items (I).ID := 0;
      if I <= 3 then Reject (Bad); end if;
      Bad := Incoming; Bad.Items (I).ID := 9_998;
      if I /= 1 then Reject (Bad); end if;
   end loop;
   Bad := (others => <>); Bad.Total := 1; Reject (Bad);
   Bad := (others => <>); Bad.Active := 1; Reject (Bad);
   Bad := (others => <>);
   Publish (Current, Bad, Accepted);
   pragma Assert (Accepted and Active_Row (Current) = 0);
   pragma Assert (not Same_Mapping (Current, Incoming));
   Ada.Text_IO.Put_Line ("PASS Rust 10000-tab snapshot read by Ada; atomic rejection and stable row mapping");
end Projection_Tests;
''')
(out / "projection.gpr").write_text('''project Projection is
 for Source_Dirs use (".", "''' + str(native) + '''");
 for Source_Files use ("servo_tab_projection.ads", "servo_tab_projection.adb", "projection_tests.adb");
 for Object_Dir use "obj";
 for Exec_Dir use ".";
 for Main use ("projection_tests.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
 end Compiler;
end Projection;
''')
subprocess.run(["rustc", "--edition=2024", str(out / "writer.rs"), "-o", str(out / "writer")], check=True)
with (out / "snapshot.bin").open("wb") as f:
    subprocess.run([str(out / "writer")], stdout=f, check=True)
subprocess.run(["gprbuild", "-p", "-P", str(out / "projection.gpr")], check=True)
result = subprocess.run([str(out / "projection_tests")], cwd=out, capture_output=True, text=True)
(out / "tests.log").write_text(result.stdout + result.stderr)
print(result.stdout + result.stderr, end="")
result.check_returncode()
assert all(hashlib.sha256(p.read_bytes()).hexdigest() == hashes[str(p)] for p in inputs)
print("PASS hosted cross-language projection; live browser integration remains pending:", out)
