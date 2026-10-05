#!/usr/bin/env python3
"""Fault-inject the actual native closed-table completion, current and old roots."""
import ast
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("   procedure Finish_Closed_Table_Retirement is")
end = text.index("   end Finish_Closed_Table_Retirement;", start) + len("   end Finish_Closed_Table_Retirement;")
body = text[start:end]
# Reuse the boundary stubs, not the other fixture's execution or expectations.
tree = ast.parse((Path(__file__).with_name("test-context-retirement-completion.py")).read_text())
prefix = next(ast.literal_eval(n.value) for n in tree.body if isinstance(n, ast.Assign)
              and any(isinstance(t, ast.Name) and t.id == "prefix" for t in n.targets))
prefix = prefix.replace("procedure Completion_Test is", "procedure Table_Test is")
prefix = prefix.replace("   Runtime_Fault : Boolean := False;",
                        "   Runtime_Fault : Boolean := False;\n   Recycled : Boolean := False;\n   Recycle_Calls : Natural := 0;")
prefix = prefix.replace("   package Context_Tickets is", "   Closed_Table_Source_Revision : Unsigned_64 := 4;\n   package Context_Tickets is")
prefix = prefix.replace("Steps = 2 and Evidence", "Recycled and Steps = (if Closed_Table_Source_Revision = 0 then 1 else 2) and Evidence")
old = '''         pragma Assert (Steps = 0 and Revision = 4 and Root = 12288 and Evidence);
         Steps := 1; Accepted := Fault /= 11;'''
new = '''         pragma Assert (Root = 12288 and Evidence);
         pragma Assert ((Steps = 0 and Revision = 2) or (Steps = 1 and Revision = 4));
         Accepted := not ((Steps = 0 and Fault = 11) or (Steps = 1 and Fault = 12));
         Steps := Steps + 1;'''
assert old in prefix
prefix = prefix.replace(old, new)
extra = '''
   package Intel_GPU_Buffer_Backing is subtype Slot is Positive; end Intel_GPU_Buffer_Backing;
   type Replacement_Record is record
      Ticket : Unsigned_64 := 55;
      Session : Unsigned_64 := 101;
      Revision : Unsigned_64 := 2;
      Root : Unsigned_64 := 12288;
   end record;
   Replacement_Tables : Boolean := True;
   Stored : Replacement_Record;
   Cleared : Boolean := False;
   package Replacement_Records is
      function Get (Object : Boolean; Slot : Positive) return Replacement_Record is (Stored);
      procedure Put (Object : in out Boolean; Slot : Positive; Value : Replacement_Record);
   end Replacement_Records;
   package body Replacement_Records is
      procedure Put (Object : in out Boolean; Slot : Positive; Value : Replacement_Record) is
      begin pragma Assert (Steps = 3 and Slot = 3); Cleared := True; end Put;
   end Replacement_Records;
   package Closed_Table_Tickets renames Context_Tickets;
   type Update_Record is record Candidate : Natural := 1; Tables : Backing; end record;
   type Update_Access is access all Update_Record;
   Updated : aliased Update_Record;
   package Application_State is
      function Updates (Slot : Positive) return Update_Access is (Updated'Access);
   end Application_State;
   Closed_Table_Index : Natural := 1;
   Buffer_Retirement_Is_Closed_Table : Boolean := True;
   Current_Table_Ticket : array (1 .. 2) of Unsigned_64 := [55, 88];
   procedure Recycle_Table_Ledger
     (Slot : Intel_GPU_Buffer_Backing.Slot; Session, Ticket : Unsigned_64;
      Accepted : out Boolean) is
   begin
      pragma Assert (Slot = 3 and Session = 101 and Ticket = 55);
      pragma Assert (Steps = (if Closed_Table_Source_Revision = 0 then 1 else 2));
      pragma Assert (Updated.Tables.Ready and not Cleared and not Recycled);
      Recycle_Calls := Recycle_Calls + 1;
      Accepted := Fault /= 18; Recycled := Accepted;
   end Recycle_Table_Ledger;
   Success : Boolean;
   Cases : Natural := 0;
'''
suffix = '''
begin
   for Is_Current in Boolean loop
      for Case_ID in 0 .. 18 loop
         Fault := Case_ID; Steps := 0; Cancels := 0; Quarantines := 0;
         Recycled := False; Recycle_Calls := 0;
         Runtime_Fault := Fault = 2;
         Closed_Table_Index := (if Fault = 1 then 0 else 1);
         Closed_Table_Source_Revision := (if Is_Current then 4 else 0);
         Buffer_Retirement_Pending := 55;
         Buffer_Retirement_Session := (if Fault = 16 then 0 elsif Fault = 6 then 102 else 101);
         Buffer_Retirement_Is_Closed_Table := True;
         Private_Contexts := [others => (others => <>)];
         if Fault = 4 then Private_Contexts (1).Life := Application_Lifetime.Published; end if;
         Stored := (others => <>);
         if Fault = 5 then Stored.Ticket := 56; end if;
         if Fault = 15 then Stored.Session := 102; end if;
         if Fault = 16 then Stored.Session := 0; end if;
         Current_Table_Ticket := [(if Is_Current then 55 else 66), 88];
         if Fault = 14 then Current_Table_Ticket (1) := 99; end if;
         Updated := (others => <>); Cleared := False;
         Finish_Closed_Table_Retirement;
         Success := Fault = 0 or (not Is_Current and (Fault = 12 or Fault = 14));
         pragma Assert (Runtime_Fault = not Success);
         pragma Assert (Cancels = (if Success then 0 else 1) and Quarantines = Cancels);
         pragma Assert (Updated.Tables.Ready = not Success and Cleared = Success);
         pragma Assert (Buffer_Retirement_Pending = 0 and not Buffer_Retirement_Is_Closed_Table);
         pragma Assert (Closed_Table_Index = 0 and Closed_Table_Source_Revision = 0);
         pragma Assert (Recycle_Calls = (if Success or Fault in 13 | 18 then 1 else 0));
         pragma Assert (Recycled = (Success or Fault = 13));
         if Success then
            pragma Assert (Steps = 3);
            pragma Assert (Current_Table_Ticket (1) =
              (if Is_Current then 0 elsif Fault = 14 then 99 else 66));
         elsif Fault in 11 .. 14 then
            pragma Assert (Steps = (if Fault = 11 or Fault = 14 then 1 elsif Fault = 12 then 2 else 3));
         elsif Fault = 18 then
            pragma Assert (Steps = (if Is_Current then 2 else 1));
         else
            pragma Assert (Steps = 0);
         end if;
         pragma Assert (Current_Table_Ticket (2) = 88 and Private_Contexts (1).Parent.Ready);
         Cases := Cases + 1;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Closed table completion PASS" & Cases'Image & " paths: distinct candidate/source epochs, exact ack, quarantine, parent retained");
end Table_Test;
'''
work = Path(tempfile.mkdtemp(prefix="closed-table-completion.", dir=root / "tests/intel-gpu/build"))
(work / "test.gpr").write_text('''project Test is
 for Source_Dirs use (".");
 for Main use ("table_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O0");
 end Compiler;
end Test;
''')
(work / "table_test.adb").write_text(prefix + extra + body + suffix)
subprocess.run(["gprbuild", "-P", "test.gpr"], cwd=work, check=True)
result = subprocess.run([str(work / "table_test")], capture_output=True, text=True)
assert result.returncode == 0 and "PASS 38" in result.stdout, (result.stdout, result.stderr)
# Negative control: omitting the ledger receipt must fail the acknowledgement
# ordering assertion, rather than letting a permissive boundary mock pass.
call = "Recycle_Table_Ledger (Slot, Saved.Session, Saved.Ticket, Accepted);"
assert body.count(call) == 1
mutant = body.replace(call, "Accepted := True;")
(work / "table_test.adb").write_text(prefix + extra + mutant + suffix)
subprocess.run(["gprbuild", "-P", "test.gpr"], cwd=work, check=True)
negative = subprocess.run([str(work / "table_test")], capture_output=True, text=True)
assert negative.returncode != 0 and "ASSERTION_ERROR" in negative.stderr, negative
assert source.read_bytes() == original
(work / "result.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(),
    "body_sha256": hashlib.sha256(body.encode()).hexdigest(), "stdout": result.stdout,
    "omitted_recycle_rejected": True, "negative_stderr": negative.stderr}, indent=2) + "\n")
print(result.stdout, "evidence", work)
