#!/usr/bin/env python3
"""Exercise the actual native cross-context alias scan with negative controls."""
from pathlib import Path
import hashlib
import json
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("      for I in Private_Contexts'Range loop", text.index("   procedure Poll_Table_Retirement is"))
end = text.index("      Buffer_Retirement_Pending := Saved.Ticket;", start)
scan = text[start:end]
prefix = '''with Ada.Text_IO;
procedure Scan_Test is
   type Image is record
      Retired, Sealed, Disjoint : Boolean;
   end record;
   type Context is record
      Attempted : Boolean;
      Source : Image;
   end record;
   Private_Contexts : array (1 .. 2) of Context;
   package Live_Snapshots is
      function Retired (Object : Image) return Boolean is (Object.Retired);
   end Live_Snapshots;
   package Application_VM is
      function Sealed (Object : Image) return Boolean is (Object.Sealed);
   end Application_VM;
   function Disjoint (Object : Image) return Boolean is (Object.Disjoint);
   Allowed, Expected : Boolean;
   Checks : Natural := 0;
   procedure Scan is
   begin
'''
suffix = '''
      Allowed := True;
   end Scan;
   function Flag (Bits, Position : Natural) return Boolean is
     ((Bits / 2 ** Position) mod 2 = 1);
begin
   for Bits in 0 .. 255 loop
      Expected := True;
      for I in Private_Contexts'Range loop
         Private_Contexts (I) :=
           (Flag (Bits, (I - 1) * 4),
            (Flag (Bits, (I - 1) * 4 + 1),
             Flag (Bits, (I - 1) * 4 + 2),
             Flag (Bits, (I - 1) * 4 + 3)));
         if Private_Contexts (I).Attempted and
           not Private_Contexts (I).Source.Retired
         then
            Expected := Expected and Private_Contexts (I).Source.Sealed and
              Private_Contexts (I).Source.Disjoint;
         end if;
      end loop;
      Allowed := False;
      Scan;
      pragma Assert (Allowed = Expected);
      Checks := Checks + 1;
   end loop;
   Ada.Text_IO.Put_Line ("Retired source scan PASS" & Checks'Image);
end Scan_Test;
'''
work = Path(tempfile.mkdtemp(prefix="retired-source-scan.", dir=root / "tests/intel-gpu/build"))
(work / "test.gpr").write_text('''project Test is
 for Source_Dirs use (".");
 for Main use ("scan_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-O0");
 end Compiler;
end Test;
''')
retired_clause = "not Live_Snapshots.Retired (Private_Contexts (I).Source) and then"
sealed_clause = "not Application_VM.Sealed (Private_Contexts (I).Source) or else"
assert scan.count(retired_clause) == scan.count(sealed_clause) == 1
results = {}
for name, body in [
    ("actual", scan),
    ("no_retired_exemption", scan.replace(retired_clause, "")),
    ("skip_unsealed", scan.replace(retired_clause,
        "Application_VM.Sealed (Private_Contexts (I).Source) and then")),
]:
    (work / "scan_test.adb").write_text(prefix + body + suffix)
    subprocess.run(["gprbuild", "-f", "-P", "test.gpr"], cwd=work, check=True)
    result = subprocess.run([str(work / "scan_test")], capture_output=True, text=True)
    results[name] = {"returncode": result.returncode, "stdout": result.stdout, "stderr": result.stderr}
    if name == "actual":
        assert result.returncode == 0 and "PASS 256" in result.stdout, results[name]
    else:
        assert result.returncode != 0 and "ASSERTION_ERROR" in result.stderr, results[name]
assert source.read_bytes() == original, "native source changed during fixture"
(work / "result.json").write_text(json.dumps({
    "source_sha256": hashlib.sha256(original).hexdigest(),
    "scan_sha256": hashlib.sha256(scan.encode()).hexdigest(),
    "results": results}, indent=2) + "\n")
print("Native retired-source scan PASS256; two negative controls rejected; evidence", work)
