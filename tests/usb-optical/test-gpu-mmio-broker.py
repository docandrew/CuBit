"""Compile exact candidate PCI parsing and extent admission with synthetic PCI."""
import os
from pathlib import Path
import re
import subprocess
import tempfile
root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/devmgr/main.adb").read_text()
proc = "   procedure setupVirtioGpu is\n" + source.split("   procedure setupVirtioGpu is\n", 1)[1]
proc = proc.split("      irqLine := pciReadConfig8", 1)[0]
proc += "      Accepted := True; Result_Base := barPhys; Result_Bytes := barBytes;\n   end setupVirtioGpu;\n"
constants = "\n".join(re.findall(r"^   (?:PCI_|VIRTIO_PCI_CAP_)[A-Za-z_0-9]+\s*: constant[^;]+;", source, re.M))
fixture = '''with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
procedure Broker_Test is
@@CONSTANTS@@
   type Device is record
      found : Boolean := True;
      bus, slot, func : Unsigned_8 := 0;
   end record;
   gpuDev : Device;
   virtioGpuPID : constant := 1;
   No_Process : constant := 0;
   DMA_VIRT_BASE : constant Unsigned_64 := 16#6000_0000#;
   LF : constant Character := ASCII.LF;
   Data : array (Natural range 0 .. 255) of Unsigned_8;
   Size : Unsigned_64 := 16384;
   Accepted : Boolean;
   Result_Base, Result_Bytes : Unsigned_64;
   Reads : Natural := 0;
   procedure debugPrint (S : String) is begin null; end;
   function pciReadConfig8 (B,S,F,O : Unsigned_8) return Unsigned_8 is
   begin Reads := Reads + 1; if Reads > 1000 then raise Program_Error; end if;
      return Data (Natural (O)); end;
   function pciReadConfig32 (B,S,F,O : Unsigned_8) return Unsigned_32 is
      V : Unsigned_32 := 0;
   begin
      for I in 0 .. 3 loop
         V := V or Shift_Left (Unsigned_32 (Data (Natural (O) + I)), I * 8);
      end loop;
      Reads := Reads + 1; return V;
   end;
   function probeMemoryBARSize (D : Device; B : Unsigned_8) return Unsigned_64 is
   begin
      if B /= PCI_BASEADDR_0 + 8 then raise Program_Error; end if;
      return Size;
   end;
@@PROCEDURE@@
   procedure Word (At_Byte : Natural; V : Unsigned_32) is
   begin
      for I in 0 .. 3 loop Data (At_Byte + I) :=
         Unsigned_8 (Shift_Right (V, I * 8) and 255); end loop;
   end;
   procedure Cap (At_Byte, Next_Byte : Natural; Kind, Length : Unsigned_8;
                  Offset, Bytes : Unsigned_32) is
   begin
      Data (At_Byte) := 9; Data (At_Byte + 1) := Unsigned_8 (Next_Byte);
      Data (At_Byte + 2) := Length; Data (At_Byte + 3) := Kind;
      Data (At_Byte + 4) := 2; Word (At_Byte + 8, Offset); Word (At_Byte + 12, Bytes);
   end;
   procedure Reset is
   begin
      Data := (others => 0); Size := 16384;
      Word (24, 16#C#); Word (28, 16#3800#); Data (52) := 64;
      Cap (64, 80, 1, 16, 4096, 56);
      Cap (80, 100, 2, 20, 12288, 4096); Word (96, 4);
      Cap (100, 116, 3, 16, 8192, 1);
      Cap (116, 0, 4, 16, 0, 16);
   end;
   Count : Natural := 0;
   procedure Check (Expected : Boolean) is
   begin
      Accepted := False; Reads := 0; setupVirtioGpu;
      if Accepted /= Expected then raise Program_Error with "case" & Count'Image; end if;
      if Accepted and then (Result_Base /= 16#3800_0000_0000# or
        Result_Bytes /= Size) then raise Program_Error; end if;
      Count := Count + 1;
   end;
begin
   Reset; Check (True);
   Reset; Word (96, 0); Check (True);
   for L in Unsigned_8 loop
      Reset; Data (82) := L; Check (L >= 20 and then L <= 176);
   end loop;
   for L in Unsigned_32 range 0 .. 70 loop
      Reset; Word (76, L); Check (L >= 56);
   end loop;
   for O in Unsigned_32 range 0 .. 16400 loop
      Reset; Word (88, O); Check (O mod 2 = 0 and then O <= 12288);
   end loop;
   Reset; Word (88, Unsigned_32'Last); Check (False);
   Reset; Word (92, Unsigned_32'Last); Check (False);
   Reset; Word (112, 0); Check (False);
   Reset; Size := 0; Check (False);
   Reset; Data (52) := 4; Check (False);
   Reset; Data (52) := 244; Data (244) := 9; Check (False);
   Reset; Data (64) := 0; Data (65) := 64; Check (False);
   Reset; Data (84) := 3; Check (False);
   Put_Line ("PASS exact-source broker PCI checks=" & Count'Image);
end Broker_Test;
'''.replace("@@CONSTANTS@@", constants).replace("@@PROCEDURE@@", proc)
if not os.environ.get("IN_NIX_SHELL"):
    raise SystemExit("Run in Nix")
with tempfile.TemporaryDirectory(prefix="broker-policy-") as name:
    work = Path(name)
    (work / "broker_test.adb").write_text(fixture)
    subprocess.run(["gnatmake", "-q", "-gnata", "broker_test.adb"], cwd=work, check=True)
    subprocess.run([str(work / "broker_test")], check=True)
