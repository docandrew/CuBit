#!/usr/bin/env python3
"""Exercise the native dispatcher guard, not hardware retirement authority."""
from pathlib import Path
import hashlib
import json
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("         Found := False;", text.index("            Poll_Image_Retirement;"))
end = text.index("         if Found and then Request.tag", start)
guard = text[start:end]
assert guard.count("Poll_Service_Request (Sender, Request, Found);") == 1
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
procedure Dispatch_Test is
   Buffer_Retirement_Pending : Unsigned_64;
   Metadata, Maps, Poll_Result, Found, In_Place_Active : Boolean;
   function Metadata_Busy return Boolean is (Metadata);
   function Map_Metadata_Busy return Boolean is (Maps);
   Sender, Request, Calls, Checks : Natural := 0;
   procedure Poll_Service_Request (S, R : out Natural; F : out Boolean) is
   begin
      Calls := Calls + 1; S := 77; R := 88; F := Poll_Result;
   end Poll_Service_Request;
   Pending : constant array (1 .. 3) of Unsigned_64 := [0, 1, Unsigned_64'Last];
begin
   for Ticket of Pending loop
      for Meta in Boolean loop
         for Map_Busy in Boolean loop
            for Old_Found in Boolean loop
               for Available in Boolean loop
                 for Updating in Boolean loop
                  Buffer_Retirement_Pending := Ticket;
                  In_Place_Active := Updating;
                  Metadata := Meta; Maps := Map_Busy; Found := Old_Found;
                  Poll_Result := Available; Calls := 0; Sender := 0; Request := 0;
'''
suffix = '''
                  if Ticket = 0 and not Meta and not Map_Busy and not Updating then
                     pragma Assert (Calls = 1 and Sender = 77 and Request = 88);
                     pragma Assert (Found = Available);
                  else
                     pragma Assert (Calls = 0 and Sender = 0 and Request = 0);
                     pragma Assert (not Found);
                  end if;
                  Checks := Checks + 1;
                 end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Retirement dispatch PASS" & Checks'Image & " actual guard cases");
end Dispatch_Test;
'''
build = root / "tests/intel-gpu/build"
build.mkdir(exist_ok=True)
work = Path(tempfile.mkdtemp(prefix="retirement-dispatch.", dir=build))
(work / "test.gpr").write_text('''project Test is
 for Source_Dirs use (".");
 for Main use ("dispatch_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-O0");
 end Compiler;
end Test;
''')
mutant = guard.replace("Buffer_Retirement_Pending = 0 and then ", "", 1)
assert mutant != guard
update_mutant = guard.replace("and then not In_Place_Active", "", 1)
assert update_mutant != guard
results = {}
for name, body in [("actual", guard), ("missing_retirement_guard", mutant),
                   ("missing_update_guard", update_mutant)]:
    (work / "dispatch_test.adb").write_text(prefix + body + suffix)
    subprocess.run(["gprbuild", "-f", "-P", "test.gpr"], cwd=work, check=True)
    result = subprocess.run([str(work / "dispatch_test")], capture_output=True, text=True)
    results[name] = {"returncode": result.returncode,
                     "stdout": result.stdout, "stderr": result.stderr}
    if name == "actual":
        assert result.returncode == 0 and "PASS 96" in result.stdout, results[name]
    else:
        assert result.returncode != 0 and "ASSERTION_ERROR" in result.stderr, results[name]
assert source.read_bytes() == original, "native source changed during fixture"
(work / "result.json").write_text(json.dumps({
    "source_sha256": hashlib.sha256(original).hexdigest(),
    "guard_sha256": hashlib.sha256(guard.encode()).hexdigest(),
    "results": results}, indent=2) + "\n")
print("Retirement dispatch PASS96; omitted retirement/update guards rejected; evidence", work)
