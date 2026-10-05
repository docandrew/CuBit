#!/usr/bin/env python3
"""Hosted extraction of the actual replacement publication lookup; no GPU claim."""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
start = source.index("         Ticket : constant Application_Buffers.Ticket := Update_Pending;", source.index("   procedure Publish_Update ("))
end = source.index("         procedure Publish_Tables", start)
body = source[start:end]
prefix = """
with Interfaces; use Interfaces;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Mappings;
procedure Replacement_Resolver_Test is
   package Application_Images is
      package Tables is
         subtype Page_Mapping is Intel_GPU_Table_Mappings.Page_Mapping;
      end Tables;
   end Application_Images;
   package Application_Buffers is
      subtype Ticket is Unsigned_64;
      function Ticket_Slot (T : Ticket) return Positive is (1);
   end Application_Buffers;
   package Intel_GPU_Buffer_Backing is subtype Slot is Positive; end;
begin
   for Mode in 0 .. 12 loop
      declare
         Present : Boolean := True;
         package Application_State is
            type Record_Type is record Table_Generation : Unsigned_64 := 7; end record;
            Updates : array (1 .. 1) of Record_Type;
            function Has_Update (Slot : Positive) return Boolean is (Present);
         end;
         Update_Pending : Unsigned_64 := 55;
         Update_Index : Positive := 1;
         Update_Session : Unsigned_64 := 99;
         Update_Identity : Unsigned_64 := 123;
         Owned : Boolean := True;
         Calls : Natural := 0;
         function Update_Exclusive return Boolean is (Owned);
         function Replacement_Table_Mapping (Slot : Positive; Session : Unsigned_64;
           Ordinal : Positive) return Intel_GPU_Table_Provenance.Mapping is
         begin
            pragma Assert (Slot = 1 and Session = 99 and Ordinal = 2);
            Calls := Calls + 1;
            case Mode is
               when 6 => Owned := False;
               when 7 => Update_Pending := 56;
               when 8 => Update_Session := 100;
               when 9 => Update_Identity := 124;
               when 10 => Application_State.Updates (1).Table_Generation := 8;
               when 11 => Present := False;
               when others => null;
            end case;
            return (Ticket => (if Mode = 12 then 56 else 55),
                    CPU => 16#100000#, DMA => 8192, Offset => 4096);
         end Replacement_Table_Mapping;
"""
suffix = """
         M : Application_Images.Tables.Page_Mapping;
      begin
         case Mode is
            when 1 => Update_Pending := 56;
            when 2 => Update_Session := 100;
            when 3 => Update_Identity := 124;
            when 4 => Application_State.Updates (1).Table_Generation := 8;
            when 5 => Present := False;
            when others => null;
         end case;
         M := Table_Page (2);
         pragma Assert (Calls = (if Mode in 1 .. 5 then 0 else 1));
         pragma Assert (M.CPU = (if Mode = 0 then 16#100000# else 0));
         pragma Assert (M.DMA = (if Mode = 0 then 8192 else 0));
      end;
   end loop;
end Replacement_Resolver_Test;
"""
evidence = Path(tempfile.mkdtemp(prefix="cubit-native-replacement-resolver."))
variants = {
    "actual": body,
    "no-postlookup-owner": body.replace("if not Held or else M.Ticket /= Ticket", "if M.Ticket /= Ticket"),
    "no-exact-ticket": body.replace("or else M.Ticket /= Ticket", ""),
    "no-generation": body.replace("and then Application_State.Updates (Slot).Table_Generation = Generation", "and then True"),
}
for name, code in variants.items():
    case = evidence / name
    case.mkdir()
    (case / "replacement_resolver_test.adb").write_text(prefix + code + suffix)
    with (case / "build.log").open("w") as log:
        subprocess.run(["gnatmake", "-q", "-gnat2022", "-gnata", "-gnato",
                        "-I" + str(root / "userspace/services/intel-gpu"),
                        "replacement_resolver_test.adb"], cwd=case, stdout=log, stderr=log, check=True)
    with (case / "run.log").open("w") as log:
        result = subprocess.run([str(case / "replacement_resolver_test")], stdout=log, stderr=log)
    assert (result.returncode == 0) == (name == "actual"), name
print("Native replacement resolver PASS13 and three negative controls:", evidence)
