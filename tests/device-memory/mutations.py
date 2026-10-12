#!/usr/bin/env python3
"""CAP-001 mutation checks: every mutant must be killed by the hosted tests
(authority_tests) and, where marked, by the SPARK proof.

Run inside the Nix shell from the repository root:
    python3 tests/device-memory/mutations.py [--no-prove]

Each mutant is a copy of the kernel sources with one textual edit, built in
tests/device-memory/build-mutations/<name>/. Prove runs use -j2; wrap the
whole script in the memory-capped systemd scope used for gnatprove.
"""
import pathlib
import shutil
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
HERE = ROOT / "tests" / "device-memory"
SRC = ROOT / "kernel" / "src"
PUB = ROOT / "shared" / "publications"
UNITS = ["capabilities.ads", "device_memory_admission.ads",
         "physical_ranges.ads", "physical_ranges.adb",
         "device_memory_authority.ads", "device_memory_authority.adb"]
SHARED_UNITS = ["device_memory_publication.ads"]

# (name, file, original, mutant, proof must also fail). A mutant marked
# proof-only is behaviourally equivalent today (Next_Device_Run's search is
# correct, so dropping its re-check changes no result); only the proof can
# and must kill it.
MUTANTS = [
    ("acpi-tables-mappable", "physical_ranges.ads",
     "(Class >= Firmware_Reserved)", "(Class >= ACPI_Tables)", False),
    ("byte-granular-blockers", "physical_ranges.ads",
     "or else Page_Of (M.Spans (I).Last) < Page_Of (First)",
     "or else M.Spans (I).Last < First", True),
    ("class-of-max", "physical_ranges.adb",
     "and then M.Spans (I).Class < Result then",
     "and then M.Spans (I).Class > Result then", True),
    ("overflow-stays-complete", "physical_ranges.adb",
     "         M.Complete := False;\n         return;",
     "         return;", True),
    ("unsealed-authorizes", "physical_ranges.ads",
     "(M.Sealed and then M.Complete and then First <= Last",
     "(M.Complete and then First <= Last", False),
    ("run-not-rechecked", "physical_ranges.adb",
     "Found := Device_Range (M, First, Last);", "Found := True;", "proof-only"),
    # Safety holds regardless (the run is re-checked), so only the tests'
    # maximality checks can kill this one.
    ("run-end-off-by-one", "physical_ranges.adb",
     "End_Page := Page_Of (M.Spans (I).First) - 1;",
     "End_Page := Page_Of (M.Spans (I).First);", False),
    ("limit-unchecked", "physical_ranges.ads",
     "(Bytes > 0 and then Base < Address_Limit\n      and then Bytes - 1 <= Address_Limit - 1 - Base",
     "(Bytes > 0 and then Base < Address_Limit", False),
    ("authorize-skips-classification", "device_memory_authority.adb",
     "if not Physical_Ranges.Device_Bytes (M, Base, Bytes) then\n         return False;",
     "if Bytes = 0 then\n         return False;", True),
    ("authorize-any-cap", "device_memory_authority.adb",
     "if Slot_Grants (Table, S, Base, Bytes, Mode) then",
     "if Table (S).capType = CAP_DEVICE_MEM then", True),
    ("mint-unbounded", "device_memory_authority.ads",
     "is (Physical_Ranges.Device_Bytes (M, Base, Bytes));",
     "is (Bytes > 0);", True),
    ("subrange-below-parent", "capabilities.ads",
     "         and then child.ref >= parent.ref\n",
     "", True),
    ("subrange-past-end", "capabilities.ads",
     "and then child.ref + (child.param - 1) <= parent.ref + (parent.param - 1));",
     "and then child.ref <= parent.ref + (parent.param - 1));", True),
    ("attenuation-rights-ignored", "capabilities.ads",
     "         and then child.authorityTag = parent.authorityTag\n         and then isSubsetOf (child.rights, parent.rights))\n        with Ghost;",
     "         and then child.authorityTag = parent.authorityTag)\n        with Ghost;", True),
    ("covers-start-unchecked", "device_memory_admission.ads",
     "       and then Base >= Resource_Base\n", "", False),
    ("allows-write-ignored", "device_memory_admission.ads",
     "(Can_Read and then (Requested = Read_Only or else Can_Write))",
     "(Can_Read)", False),
    # Denial records (CAP-001 NUC diagnostics).
    ("first-blocked-at-range-start", "physical_ranges.adb",
     "            Page := Page_Number'Max (Low, Page_Of (M.Spans (I).First));",
     "            Page := Low;", True),
    ("explain-always-no-capability", "device_memory_authority.adb",
     "      if not Physical_Ranges.Device_Bytes (M, Base, Bytes) then\n         return Explain_Mint (M, Base, Bytes);\n      end if;\n",
     "", True),
    ("reader-never-loses", "device_memory_publication.ads",
     "(if Total - Seen > Denial_Capacity then Total - (Denial_Capacity - 1)\n      else Seen + 1)",
     "(Seen + 1)", True),
]

GPR = """project Mutant is
   for Source_Dirs use ("{here}", "{here}/fixture", "src", "{time}");
   for Source_Files use ("authority_tests.adb", "config.ads",
     "capabilities.ads", "device_memory_admission.ads",
     "device_memory_publication.ads", "clock_publication.ads", "clock_publication.adb",
     "physical_ranges.ads", "physical_ranges.adb",
     "device_memory_authority.ads", "device_memory_authority.adb");
   for Object_Dir use "obj";
   for Exec_Dir use "obj";
   for Main use ("authority_tests.adb");
   package Compiler is
      for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
   end Compiler;
end Mutant;
"""


def run(cmd, cwd):
    return subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT, text=True)


def main():
    prove = "--no-prove" not in sys.argv
    only = [a.split("=", 1)[1] for a in sys.argv if a.startswith("--only=")]
    base = HERE / "build-mutations"
    survivors = []
    for name, unit, original, mutant, must_fail_proof in MUTANTS:
        if only and name not in only:
            continue
        work = base / name
        shutil.rmtree(work, ignore_errors=True)
        (work / "src").mkdir(parents=True)
        for u in UNITS:
            shutil.copy(SRC / u, work / "src" / u)
        for u in SHARED_UNITS:
            shutil.copy(PUB / u, work / "src" / u)
        text = (work / "src" / unit).read_text()
        if text.count(original) != 1:
            print(f"ERROR {name}: original text not found exactly once")
            survivors.append(name)
            continue
        (work / "src" / unit).write_text(text.replace(original, mutant))
        (work / "mutant.gpr").write_text(
            GPR.format(here=HERE, time=ROOT / "shared" / "time"))
        build = run(["gprbuild", "-p", "-q", "-P", "mutant.gpr"], work)
        if build.returncode != 0:
            test_killed, how = True, "compile"
        else:
            test = run([str(work / "obj" / "authority_tests")], work)
            test_killed, how = test.returncode != 0, "tests"
        proof_killed = None
        if prove and must_fail_proof:
            p = run(["gnatprove", "-P", "mutant.gpr", "--mode=all", "--level=2",
                     "--prover=cvc5,z3", "--checks-as-errors=on",
                     "--report=fail", "-j2", "-u", "physical_ranges.adb",
                     "device_memory_authority.adb", "capabilities.ads",
                     "device_memory_publication.ads"], work)
            proof_killed = p.returncode != 0
        status = ("killed" if test_killed else
                  "equivalent" if must_fail_proof == "proof-only" else "SURVIVED")
        proof = ("" if proof_killed is None else
                 ("; proof killed" if proof_killed else "; proof SURVIVED"))
        print(f"{name:34} tests {status} ({how}){proof}")
        if must_fail_proof == "proof-only" and not prove:
            print(f"{name:34} (proof-only mutant: needs the proof run)")
            continue
        if (not test_killed and must_fail_proof != "proof-only") or proof_killed is False:
            survivors.append(name)
    if survivors:
        print("SURVIVORS: " + ", ".join(survivors))
        return 1
    print(f"PASS: all {len(MUTANTS)} mutants killed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
