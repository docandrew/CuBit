#!/usr/bin/env python3
"""Execute the actual IRQ fault dispatcher against a modeled process adapter."""
import pathlib, subprocess, tempfile, sys
root=pathlib.Path(sys.argv[1]).resolve()
source=(root/'kernel/src/interrupts.adb').read_text()
start=source.index('    procedure handlePageFault (err : in Unsigned_64) with')
end=source.index('    end handlePageFault;',start)+len('    end handlePageFault;')
# The extracted subprogram is nested in this harness; GNAT rejects the
# original library-level SPARK aspect there. Its executable body is unchanged.
body=source[start:end].replace(" with\n        SPARK_Mode => On", "", 1)
prefix='''with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO;
procedure Main is
   Kills, User_Faults, Kernel_Faults : Natural := 0;
   Expected_Write, Kernel_Handles : Boolean := False;
   package Util is
      function isBitSet (Word : Unsigned_64; Bit : Natural) return Boolean is
        ((Word and Shift_Left (Unsigned_64'(1), Bit)) /= 0);
      function numToAddr (Word : Unsigned_64) return System.Address is
        (To_Address (Integer_Address (Word)));
   end Util;
   package x86 is
      function getCR2 return Unsigned_64 is (4096);
   end x86;
   package PerCPUData is
      function getCurrentPID return Natural is (1);
   end PerCPUData;
   package Process is
      subtype ProcessID is Natural;
      procedure kill (PID : ProcessID);
      procedure pageFault (PID : ProcessID; Addr : System.Address; Write : Boolean);
      procedure kernelUserFault (PID : ProcessID; Addr : System.Address;
                                 Write : Boolean; Handled : out Boolean);
   end Process;
   package body Process is
      procedure kill (PID : ProcessID) is
      begin pragma Assert (PID = 1); Kills := Kills + 1; end;
      procedure pageFault (PID : ProcessID; Addr : System.Address; Write : Boolean) is
      begin
         pragma Assert (PID = 1 and Addr = To_Address (4096));
         pragma Assert (Write = Expected_Write);
         User_Faults := User_Faults + 1;
      end;
      procedure kernelUserFault (PID : ProcessID; Addr : System.Address;
                                 Write : Boolean; Handled : out Boolean) is
      begin
         pragma Assert (PID = 1 and Addr = To_Address (4096));
         pragma Assert (Write = Expected_Write);
         Kernel_Faults := Kernel_Faults + 1;
         Handled := Kernel_Handles;
      end;
   end Process;
   procedure println (Text : String) is begin null; end;
   procedure println (Addr : System.Address) is begin null; end;
   procedure print (Text : String) is begin null; end;
'''
suffix='''
   Raised : Boolean;
   P, U, R, X : Boolean;
begin
   for Handles in Boolean loop
      Kernel_Handles := Handles;
      for Error_Code in Unsigned_64 range 0 .. 31 loop
         Kills := 0; User_Faults := 0; Kernel_Faults := 0;
         P := (Error_Code and 1) /= 0;
         Expected_Write := (Error_Code and 2) /= 0;
         U := (Error_Code and 4) /= 0;
         R := (Error_Code and 8) /= 0;
         X := (Error_Code and 16) /= 0;
         Raised := False;
         begin handlePageFault (Error_Code);
         exception when others => Raised := True; end;
         pragma Assert (Raised = (R or (not U and (X or P or not Handles))));
         pragma Assert (Kills = (if not R and U and (X or P) then 1 else 0));
         pragma Assert (User_Faults = (if not R and not X and not P and U then 1 else 0));
         pragma Assert (Kernel_Faults = (if not R and not X and not P and not U then 1 else 0));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS actual page-fault dispatcher: 64 access/error paths");
end Main;
'''
with tempfile.TemporaryDirectory(prefix='penny-fault-routing-') as tmp:
 out=pathlib.Path(tmp)
 (out/'test.gpr').write_text('project Test is\nfor Main use ("main.adb");\nfor Object_Dir use "obj";\npackage Compiler is\nfor Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-O0");\nend Compiler;\nend Test;\n')
 def run(code):
  (out/'main.adb').write_text(prefix+code+suffix)
  subprocess.run(['gprbuild','-q','-f','-p','-P',str(out/'test.gpr')],check=True)
  return subprocess.run([str(out/'obj/main')],capture_output=True,text=True)
 result=run(body);print(result.stdout,end='');assert result.returncode==0,result.stderr
 for name,old,new in [('instruction fallthrough','                Process.kill (pid);\n                return;',''),('lost write intent','faultAddr, Write => write','faultAddr, Write => False'),('reserved-bit fallthrough','if reservedWrite then','if False then')]:
  assert old in body
  result=run(body.replace(old,new,1))
  assert result.returncode!=0, f'missed negative control: {name}'
  print('PASS negative control:',name)
