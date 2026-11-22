"""Actual Desktop completion admission with routing stubbed; no IPC/fence claim."""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[2]
s=(root/'userspace/services/desktop/main.adb').read_text()
a=s.index('   procedure collectPresentations is');b=s.index('   end collectPresentations;',a)+len('   end collectPresentations;');body=s[a:b]
a=body.index('         if DM.Enabled');b=body.rindex('      end loop;')
# Completion routing is tested separately. Preserve the actual poll/admission/
# failure control flow while replacing the route with a costed test handler.
body=body[:a]+'         Handle_Completion;\n'+body[b:]
prefix='''with Ada.Text_IO;with Interfaces;use Interfaces;with System;
with Compositor_Dispatch_Budget;
procedure Check is
 package DB renames Compositor_Dispatch_Budget;
 package DSP is type Frame_Outcome is (Published);type Buffer_Disposition is (Released);end;
 subtype CompletionEntry is Unsigned_64;NULL_COMPLETION:constant CompletionEntry:=0;
 Now,Cost:Unsigned_64:=100;
 Left,Handled,Polls,Faults:Natural:=0;
 Invalid:Boolean:=False;
 function dispatchNow return Unsigned_64 is (Now);
 function Poll_Completion(A:System.Address) return Unsigned_64 is
 begin
  Polls:=Polls+1;
  if Left=0 then return 0;end if;
  Left:=Left-1;
  return(if Invalid then 2 else 1);
 end;
 procedure Handle_Completion is
 begin Handled:=Handled+1;if Now/=Unsigned_64'Last then Now:=Now+Cost;end if;end;
 package CR is procedure Quarantine(S:in out Natural);end;
 package body CR is procedure Quarantine(S:in out Natural) is begin S:=1;end;end;
 package Desktop_Launch_Refresh is procedure Quarantine;end;
 package body Desktop_Launch_Refresh is procedure Quarantine is null;end;
 launchRequest:Natural:=0;
 LF:constant String:=(1=>ASCII.LF);
 procedure debugPrint(S:String) is null;
 procedure quarantinePresentations is begin Faults:=Faults+1;end;
'''
suffix='''
 procedure Scenario(N:Natural;Step:Unsigned_64;Expected:Natural;Broken_Clock,Malformed:Boolean:=False) is
 begin
  Left:=N;Cost:=Step;Now:=(if Broken_Clock then Unsigned_64'Last else 100);
  Handled:=0;Polls:=0;Faults:=0;Invalid:=Malformed;
  collectPresentations;
  pragma Assert(Handled=Expected);
  pragma Assert(Polls<=65 and Left=N-Handled-(if Malformed and N>0 then 1 else 0));
  pragma Assert(Faults=(if Malformed and N>0 then 1 else 0));
 end;
begin
 for I in 1..1000 loop
  Scenario(1000,0,64); -- Refills beyond one queue capacity cannot starve input.
  Scenario(1000,100,5); -- 500us boundary is checked between handlers.
  Scenario(1000,600,1); -- One handler may overrun; no further work admitted.
  Scenario(1000,0,1,True);Scenario(0,0,0);Scenario(3,0,3);
  Scenario(1000,0,0,False,True);
 end loop;
 Ada.Text_IO.Put_Line("COMPLETION-DRAIN: PASS 7000 actual admission/flood/clock-fault/malformed-poll scenarios");
end Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-completion-drain-') as tmp:
 d=Path(tmp)
 for ext in ('ads','adb'):
  p=root/'userspace/lib/compositor'/('compositor_dispatch_budget.'+ext);(d/p.name).write_bytes(p.read_bytes())
 (d/'check.gpr').write_text('project Check is for Source_Dirs use (".");for Object_Dir use "obj";for Exec_Dir use ".";for Main use ("check.adb");package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato");end Compiler;end Check;')
 for mode in ('normal','unbounded','ignore-clock','continue-malformed'):
  variant=body
  if mode=='unbounded':variant=variant.replace('while DB.Can_Complete (Batch, dispatchNow) loop','while True loop').replace('         DB.Charge_Completion (Batch);','')
  if mode=='ignore-clock':variant=variant.replace('DB.Can_Complete (Batch, dispatchNow)','DB.Can_Complete (Batch, DB.Completion_Start (Batch))')
  if mode=='continue-malformed':variant=variant.replace('            return;','            null;')
  (d/'check.adb').write_text(prefix+variant+suffix)
  subprocess.run(['gprbuild','-f','-q','-p','-P',str(d/'check.gpr')],check=True)
  v=subprocess.run([str(d/'check')],capture_output=True,text=True)
  if mode=='normal':assert v.returncode==0,v.stderr;print(v.stdout,end='')
  else:assert v.returncode!=0 and 'ASSERTION_ERROR' in v.stderr,v;print('COMPLETION-DRAIN: rejected '+mode)
