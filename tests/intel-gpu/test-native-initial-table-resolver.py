#!/usr/bin/env python3
"""Extract native initial-publication resolver; hosted authority regression only."""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
start = source.index("                  Generation : constant", source.index("procedure Handle_Context_Preparation"))
end = source.index("                  procedure Publish_Tables", start)
body = source[start:end]
prefix = """
with Interfaces; use Interfaces;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Mappings;
procedure Initial_Resolver_Test is
   package Application_Images is
      package Tables is
         subtype Page_Mapping is Intel_GPU_Table_Mappings.Page_Mapping;
      end Tables;
   end Application_Images;
begin
   for Mode in 0 .. 9 loop
      declare
         type Context is record Table_Generation : Unsigned_64 := 7; end record;
         Private_Contexts : array (1 .. 1) of Context;
         Stored : constant Positive := 1;
         Preparing_Index : Natural := 1;
         Session : constant Unsigned_64 := 99;
         Identity : constant Unsigned_64 := 123;
         Preparing_Session : Unsigned_64 := 99;
         Preparing_Identity : Unsigned_64 := 123;
         Owned : Boolean := True;
         Calls : Natural := 0;
         function Application_Image_Owner return Boolean is (Owned);
         function Initial_Table_Mapping (Index : Positive; Session : Unsigned_64;
           Ordinal : Positive) return Intel_GPU_Table_Provenance.Mapping is
         begin
            pragma Assert (Index = 1 and Session = 99 and Ordinal = 2);
            Calls := Calls + 1;
            case Mode is
               when 5 => Owned := False;
               when 6 => Private_Contexts (1).Table_Generation := 8;
               when 7 => Preparing_Session := 100;
               when 8 => Preparing_Identity := 124;
               when others => null;
            end case;
            return (Ticket => (if Mode = 9 then 0 else 55),
                    CPU => 16#100000#, DMA => 8192, Offset => 4096);
         end Initial_Table_Mapping;
"""
suffix = """
         M : Application_Images.Tables.Page_Mapping;
      begin
         case Mode is
            when 1 => Preparing_Index := 0;
            when 2 => Preparing_Session := 100;
            when 3 => Preparing_Identity := 124;
            when 4 => Private_Contexts (1).Table_Generation := 8;
            when others => null;
         end case;
         M := Table_Page (2);
         pragma Assert (Calls = (if Mode in 1 .. 4 then 0 else 1));
         pragma Assert (M.CPU = (if Mode = 0 then 16#100000# else 0));
         pragma Assert (M.DMA = (if Mode = 0 then 8192 else 0));
      end;
   end loop;
end Initial_Resolver_Test;
"""
evidence = Path(tempfile.mkdtemp(prefix="cubit-native-initial-resolver."))
variants = {
    "actual": body,
    "no-postlookup-owner": body.replace("if not Held or else M.Ticket = 0", "if M.Ticket = 0"),
    "no-generation": body.replace("and then Private_Contexts (Stored).Table_Generation = Generation", "and then True"),
}
for name, code in variants.items():
    case = evidence / name
    case.mkdir()
    (case / "initial_resolver_test.adb").write_text(prefix + code + suffix)
    with (case / "build.log").open("w") as log:
        subprocess.run(["gnatmake", "-q", "-gnat2022", "-gnata", "-gnato",
                        "-I" + str(root / "userspace/services/intel-gpu"),
                        "initial_resolver_test.adb"], cwd=case, stdout=log, stderr=log, check=True)
    with (case / "run.log").open("w") as log:
        result = subprocess.run([str(case / "initial_resolver_test")], stdout=log, stderr=log)
    assert (result.returncode == 0) == (name == "actual"), name
print("Native initial resolver PASS10 and two negative controls:", evidence)
