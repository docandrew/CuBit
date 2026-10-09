"""Compile the production GPU BAR decode against a checked PCI-read fixture."""
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
source = (ROOT / "userspace/services/devmgr/main.adb").read_text()
block = source.split("-- BEGIN VIRTIO GPU BAR DECODE", 1)[1].split("\n", 1)[1]
block = block.split("-- END VIRTIO GPU BAR DECODE.", 1)[0]
fixture = '''with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
procedure GPU_BAR_Test is
   type Device is record bus, slot, func : Unsigned_8 := 0; end record;
   gpuDev : Device;
   commonBar : Unsigned_8;
   barRaw : Unsigned_32;
   barPhys : Unsigned_64;
   Low, High : Unsigned_32;
   Reads : Natural;
   Accepted : Boolean;
   PCI_BASEADDR_0 : constant Unsigned_8 := 16#10#;
   LF : constant Character := ASCII.LF;
   procedure debugPrint (S : String) is begin null; end;
   function pciReadConfig32 (B, S, F, Offset : Unsigned_8) return Unsigned_32 is
   begin
      Reads := Reads + 1;
      if Reads = 1 and then Offset = PCI_BASEADDR_0 + commonBar * 4 then
         return Low;
      elsif Reads = 2 and then commonBar < 5 and then
        Offset = PCI_BASEADDR_0 + commonBar * 4 + 4 then
         return High;
      else raise Program_Error with "unexpected PCI read"; end if;
   end;
   procedure Decode is
   begin
BLOCK
      Accepted := True;
   end;
   Count : Natural := 0;
   procedure Check (Index : Unsigned_8; L, H : Unsigned_32;
                    Valid : Boolean; Expected : Unsigned_64; Read_Count : Natural) is
   begin
      commonBar := Index; Low := L; High := H; Reads := 0;
      Accepted := False; barPhys := 0;
      Decode;
      if Accepted /= Valid or else Reads /= Read_Count or else
        (Valid and then barPhys /= Expected) then
         raise Program_Error with "BAR case" & Count'Image;
      end if;
      Count := Count + 1;
   end;
begin
   -- Captured OVMF virtio BAR: low32 zero, above 4 GiB.
   Check (2, 16#C#, 16#3800#, True, 16#3800_0000_0000#, 2);
   Check (2, 16#1234_500C#, 16#3800#, True, 16#3800_1234_5000#, 2);
   Check (5, 16#8000_0008#, 0, True, 16#8000_0000#, 1);
   Check (0, 16#8000_0004#, 0, True, 16#8000_0000#, 2);
   Check (5, 16#C#, 1, False, 0, 1);
   Check (2, 0, 0, False, 0, 1);
   Check (2, 4, 0, False, 0, 2);
   Check (2, 16#8000_0001#, 0, False, 0, 1);
   Check (2, 16#8000_0002#, 0, False, 0, 1);
   Check (2, 16#8000_0006#, 0, False, 0, 1);
   for Index in Unsigned_8 range 6 .. 255 loop
      Check (Index, 16#8000_0000#, 0, False, 0, 0);
   end loop;
   Put_Line ("PASS exact-source GPU BAR checks=" & Count'Image);
end GPU_BAR_Test;
'''.replace("BLOCK", block)

if not os.environ.get("IN_NIX_SHELL"):
    raise SystemExit("Run in nix develop")
with tempfile.TemporaryDirectory(prefix="gpu-bar-") as name:
    work = Path(name)
    (work / "gpu_bar_test.adb").write_text(fixture)
    subprocess.run(["gnatmake", "-q", "-gnata", "gpu_bar_test.adb"], cwd=work, check=True)
    subprocess.run([str(work / "gpu_bar_test")], check=True)
