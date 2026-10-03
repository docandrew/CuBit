#!/usr/bin/env python3
"""Compile actual Ada window router against deterministic session failures.

The fake native sessions model per-surface state and a denied close. This is a
hosted routing/lease regression, not a substitute for native Desktop closure.
"""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
with tempfile.TemporaryDirectory(prefix="servo-window-router-") as tmp:
    d = Path(tmp)
    for name in ["servo_shell.ads", "servo_shell.adb", "servo_session.ads"]:
        (d / name).write_text((root / "userspace/servo/native" / name).read_text())
    (d / "cubit.ads").write_text("package CuBit is end CuBit;")
    (d / "cubit-messages.ads").write_text("with Interfaces; use Interfaces; package CuBit.Messages is SYSINFO_MEM_OWNED_SELF : constant Unsigned_64 := 1602; function getInfo (Query : Unsigned_64) return Unsigned_64 is (0); end CuBit.Messages;")
    (d / "faults.ads").write_text("package Faults is Deny_Close : Boolean := False; end Faults;")
    (d / "servo_session.adb").write_text('''with Faults;
package body Servo_Session is
   Opened, Painting : Boolean := False;
   Value : Unsigned_32 := 0;
   function Open return Unsigned_32 is
   begin Opened := True; Value := 0; return 1; end Open;
   function Is_Open return Boolean is (Opened);
   procedure Close is
   begin if not Faults.Deny_Close then Opened := False; end if; end Close;
   procedure Metrics (Result : access Viewport) is
   begin Result.Width := Value; end Metrics;
   procedure Begin_Input is begin null; end Begin_Input;
   function Poll (Result : access Event) return Unsigned_32 is (0);
   function Location (Text : System.Address; Capacity : Unsigned_32) return Unsigned_32 is (0);
   procedure State (URL : System.Address; URL_Length : Unsigned_32;
      Title : System.Address; Title_Length : Unsigned_32; Flags : Unsigned_32) is
   begin Value := Flags; end State;
   procedure Security (Text : System.Address; Length : Unsigned_32) is begin null; end Security;
   procedure Tab_Title (Index : Unsigned_32; Text : System.Address; Length : Unsigned_32) is begin null; end Tab_Title;
   procedure Tab_Parked (Index : Unsigned_32) is begin null; end Tab_Parked;
   procedure Navigation_Error is begin null; end Navigation_Error;
   procedure Window_Error is begin null; end Window_Error;
   function Prepare return Unsigned_32 is
   begin Painting := True; return 1; end Prepare;
   procedure Cancel is begin Painting := False; end Cancel;
   function Present (RGBA : System.Address; Length : Unsigned_64;
      Width, Height : Unsigned_32) return Unsigned_32 is
   begin pragma Assert (Painting); Painting := False; return 1; end Present;
   function Pending return Unsigned_32 is (0);
end Servo_Session;
''')
    (d / "main.adb").write_text('''with Servo_Shell; use Servo_Shell;
with Interfaces; use Interfaces;
with System; with Faults; with Ada.Text_IO;
procedure Main is
   V : aliased Viewport;
   R : Unsigned_32;
begin
   pragma Assert (Select_Window (0) = 0 and Select_Window (5) = 0);
   for I in Unsigned_32 range 1 .. 4 loop
      pragma Assert (Open = I);
      State (System.Null_Address, 0, System.Null_Address, 0, I * 100);
   end loop;
   pragma Assert (Open = 0);
   for I in Unsigned_32 range 1 .. 4 loop
      pragma Assert (Select_Window (I) = 1);
      Metrics (V'Access); pragma Assert (V.Width = I * 100);
   end loop;
   pragma Assert (Select_Window (2) = 1 and then Prepare = 1);
   pragma Assert (Select_Window (1) = 0 and Prepare = 0 and Open = 0);
   pragma Assert (Select_Window (2) = 1);
   Cancel;
   pragma Assert (Select_Window (1) = 1 and then Prepare = 1);
   pragma Assert (Present (System.Null_Address, 0, 0, 0) = 1);
   pragma Assert (Select_Window (3) = 1);
   Faults.Deny_Close := True; Close;
   pragma Assert (Select_Window (3) = 0 and Open = 0);
   pragma Assert (Select_Window (1) = 1);
   Metrics (V'Access); pragma Assert (V.Width = 100);
   Faults.Deny_Close := False;
   pragma Assert (Open = 3);
   Metrics (V'Access); pragma Assert (V.Width = 0);
   Close;
   pragma Assert (Select_Window (3) = 0);
   for I in Unsigned_32 range 1 .. 4 loop
      if I /= 3 then
         R := Select_Window (I); pragma Assert (R = 1);
         Metrics (V'Access); pragma Assert (V.Width = I * 100);
         Close;
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("SERVO-WINDOW-ROUTER: PASS independent state, capacity, pinned frame, denied close and reuse");
end Main;
''')
    subprocess.run(["gnatmake", "-q", "-gnat2022", "-gnata", "-gnato", "main.adb"], cwd=d, check=True)
    subprocess.run([str(d / "main")], check=True)
