"""Regression-test exact BAR sizing helper, including restoration order."""
import os
from pathlib import Path
import subprocess
import tempfile

if not os.environ.get('IN_NIX_SHELL'):
    raise SystemExit('Run in Nix')

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/devmgr/main.adb").read_text()
start = source.index('   function probeMemoryBARSize\n', source.index('   procedure setupVirtioGpu is'))
helper = source[start:source.index('   end probeMemoryBARSize;', start) + len('   end probeMemoryBARSize;')]
fixture = '''with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
procedure Probe_Test is
   type PCIDeviceInfo is record bus, slot, func : Unsigned_8 := 0; end record;
   PCI_COMMAND : constant Unsigned_8 := 4;
   Command : Unsigned_16 := 7;
   Original_Lo, Original_Hi, Lo, Hi, Mask_Lo, Mask_Hi : Unsigned_32;
   Wide : Boolean;
   Writes : Natural;
   function pciReadConfig16 (B,S,F,O : Unsigned_8) return Unsigned_16 is
   begin return Command; end;
   procedure pciWriteConfig16 (B,S,F,O : Unsigned_8; V : Unsigned_16) is
   begin
      if V = 7 and then (Lo /= Original_Lo or Hi /= Original_Hi) then
         raise Program_Error with "decode restored before BAR";
      end if;
      Command := V; Writes := Writes + 1;
   end;
   function pciReadConfig32 (B,S,F,O : Unsigned_8) return Unsigned_32 is
   begin
      if O = 24 then
         return (if Lo = Unsigned_32'Last then Mask_Lo else Lo);
      elsif O = 28 then
         return (if Hi = Unsigned_32'Last then Mask_Hi else Hi);
      else raise Program_Error; end if;
   end;
   procedure pciWriteConfig32 (B,S,F,O : Unsigned_8; V : Unsigned_32) is
   begin
      if (Command and 3) /= 0 then raise Program_Error with "live decode"; end if;
      if O = 24 then Lo := V;
      elsif O = 28 and Wide then Hi := V;
      else raise Program_Error with "wrong BAR written"; end if;
      Writes := Writes + 1;
   end;
@@HELPER@@
   Dev : PCIDeviceInfo;
   Count : Natural := 0;
   procedure Check (Bytes : Unsigned_64; Is_Wide : Boolean) is
      Mask : constant Unsigned_64 := not (Bytes - 1);
      Result, Expected : Unsigned_64;
   begin
      Wide := Is_Wide; Command := 7; Writes := 0;
      Original_Lo := (if Wide then 12 else 16#8000_0000#);
      Original_Hi := (if Wide then 16#3800# else 16#A5A5_A5A5#);
      Lo := Original_Lo; Hi := Original_Hi;
      Mask_Lo := Unsigned_32 (Mask and 16#FFFF_FFF0#) or (Original_Lo and 15);
      Mask_Hi := Unsigned_32 (Shift_Right (Mask, 32));
      Expected := (if Bytes >= 4096 and Bytes <= 1024 * 1024 then Bytes else 0);
      Result := probeMemoryBARSize (Dev, 24);
      if Result /= Expected or Command /= 7 or Lo /= Original_Lo or Hi /= Original_Hi
        or Writes /= (if Wide then 6 else 4)
      then raise Program_Error with "sizing/restoration case" & Count'Image; end if;
      Count := Count + 1;
   end;
begin
   for W in Boolean loop
      for Exponent in 4 .. 31 loop Check (2 ** Exponent, W); end loop;
   end loop;
   Wide := False; Command := 7; Writes := 0;
   Original_Lo := 16#8001#; Original_Hi := 0; Lo := Original_Lo; Hi := 0;
   if probeMemoryBARSize (Dev, 24) /= 0 or Writes /= 0 then
      raise Program_Error with "I/O BAR mutated";
   end if;
   Put_Line ("PASS exact BAR sizing/restoration checks=" & Natural'Image (Count + 1));
end Probe_Test;
'''.replace('@@HELPER@@', helper)
with tempfile.TemporaryDirectory(prefix='bar-probe-') as directory:
    work = Path(directory)
    (work / 'probe_test.adb').write_text(fixture)
    subprocess.run(['gnatmake', '-q', '-gnat2022', '-gnata', 'probe_test.adb'], cwd=work, check=True)
    subprocess.run([str(work / 'probe_test')], check=True)
