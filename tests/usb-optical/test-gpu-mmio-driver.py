"""Compile the candidate's exact admission and notification checks with fake caps."""
import os
from pathlib import Path
import subprocess
import tempfile
root = Path(__file__).resolve().parents[2]
text = (root / "userspace/services/virtio-gpu/main.adb").read_text()
part = text.split("   procedure initTransport is\n", 1)[1]
decl, body = part.split("   begin\n", 1)
admit = body.split('      trace ("map BAR");', 1)[0]
notify = body.split("      Notify_Displacement :=", 1)[1].split("      qsz :=", 1)[0]
fixture = '''with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
with Ada.Text_IO; use Ada.Text_IO;
procedure Policy_Test is
   package Access_Word is new System.Address_To_Access_Conversions (Unsigned_64);
   type Words is array (0 .. 5) of Unsigned_64;
   Bar, Notify : Words;
   barPhys : constant Unsigned_64 := 16#3800_0000_0000#;
   barBytes, notifyBytes, queueNotifyOffset : Unsigned_64 := 0;
   commonOff, notifyOff, isrOff, notifyMult : Unsigned_64;
   Q : Unsigned_16 := 0;
   OK, Failed : Boolean := False;
   SYSCall_GETPID : constant := 1;
   SYSCall_INSPECT_CAPABILITY : constant := 2;
   REG_QUEUE_NOTIFY_OFF : constant := 30;
   function Fits (Offset, Bytes, Limit : Unsigned_64) return Boolean is
     (Bytes /= 0 and then Offset <= Limit and then Bytes <= Limit - Offset);
   procedure fail (Why : String) is begin Failed := True; end;
   function syscall (N : Unsigned_64; A, B, C : Unsigned_64 := 0) return Unsigned_64 is
      Data : Words;
   begin
      if N = SYSCall_GETPID then return 1; end if;
      if N /= SYSCall_INSPECT_CAPABILITY or else A /= 1 or else
        (B /= 4 and B /= 7) then raise Program_Error; end if;
      Data := (if B = 4 then Bar else Notify);
      for I in Data'Range loop
         Access_Word.To_Pointer (To_Address (Integer_Address (C) +
           Integer_Address (I * 8))).all := Data (I);
      end loop;
      return 1;
   end;
   function read16 (Offset : Unsigned_64) return Unsigned_16 is (Q);
   procedure Admit is
DECL
   begin
ADMIT
      OK := True;
   end;
   procedure Queue_Check is
      Notify_Displacement : Unsigned_64;
   begin
      Notify_Displacement :=@@NOTIFY@@
      OK := True;
   end;
   procedure Reset is
   begin
      Bar := (7, 3, 0, barPhys, 16384, 0);
      Notify := (7, 2, 0, barPhys + 12288, 4096, 0);
      commonOff := 4096; notifyOff := 12288; isrOff := 8192; notifyMult := 4;
      OK := False; Failed := False;
   end;
   Count : Natural := 0;
   procedure Check (Expected : Boolean) is
   begin
      OK := False; Failed := False; Admit;
      if OK /= Expected or else Failed = Expected then raise Program_Error; end if;
      Count := Count + 1;
   end;
begin
   Reset; Check (True);
   for Bytes in Unsigned_64 range 0 .. 20000 loop
      Reset; Bar (4) := Bytes;
      Check (Bytes >= 16384 and then Bytes mod 4096 = 0);
   end loop;
   Reset; Bar (3) := barPhys + 4096; Check (False);
   Reset; Bar (0) := 1; Check (False);
   Reset; Bar (1) := 1; Check (False);
   Reset; Notify (3) := barPhys + 16384; Check (False);
   Reset; Notify (4) := 4097; Check (False);
   Reset; commonOff := Unsigned_64'Last; Check (False);
   Reset; isrOff := 16384; Check (False);
   Reset; notifyMult := 2 ** 32; Check (False);
   Reset; Admit;
   for Index in Unsigned_16 loop
      Q := Index; OK := False; Failed := False; Queue_Check;
      if OK /= (Index <= 1023) or else Failed = OK then raise Program_Error; end if;
      if OK and then queueNotifyOffset /= 12288 + Unsigned_64 (Index) * 4 then
         raise Program_Error;
      end if;
      Count := Count + 1;
   end loop;
   Reset; notifyMult := 0; Check (True);
   for Index in Unsigned_16 loop
      Q := Index; OK := False; Failed := False; Queue_Check;
      if not OK or Failed or queueNotifyOffset /= notifyOff then
         raise Program_Error with "shared notification address";
      end if;
      Count := Count + 1;
   end loop;
   Put_Line ("PASS candidate exact-source MMIO checks=" & Count'Image);
end Policy_Test;
'''.replace("DECL", decl).replace("ADMIT", admit).replace("@@NOTIFY@@", notify)
if not os.environ.get("IN_NIX_SHELL"):
    raise SystemExit("Run in Nix")
with tempfile.TemporaryDirectory(prefix="mmio-policy-") as name:
    work = Path(name)
    (work / "policy_test.adb").write_text(fixture)
    subprocess.run(["gnatmake", "-q", "-gnata", "policy_test.adb"], cwd=work, check=True)
    subprocess.run([str(work / "policy_test")], check=True)
