"""Run the actual UI.App event loop against deterministic input/frame boundaries."""
from pathlib import Path
import hashlib,json,os,subprocess,tempfile
assert os.environ.get('IN_NIX_SHELL')
root=Path(__file__).resolve().parents[2]
source=(root/'userspace/lib/ui/cubit-ui-app.adb').read_text()
a=source.index('   procedure Run (win : in out Window)');b=source.index('   end Run;',a)+len('   end Run;')
body=source[a:b].replace('CuBit.UI.State.Followup_Render_Requested','Followup').replace('CuBit.UI.Is_Empty','Is_Empty').replace('CuBit.UI.Rect','Rect')
out=Path(tempfile.mkdtemp(prefix='input-terminal-loop-'))
prefix=r"""with Interfaces; use Interfaces; with Ada.Text_IO;
procedure Check is
 type Rect is record x,y,w,h:Natural:=0;end record;
 function Is_Empty(R:Rect) return Boolean is (R.w=0 or R.h=0);
 type Window is record
  inputStopped:Boolean:=False; protectedFrames:Boolean:=True;
  frames:Boolean:=True; deferredDamage:Rect:=(others=>0);
  surfaceId,bufferPages:Unsigned_64:=11;
 end record;
 type Pointer_Interaction is null record;
 type Input_Event is record kind:Natural:=0;end record;
 INPUT_POINTER_DOWN:constant:=1;INPUT_POINTER_UP:constant:=2;
 INPUT_POINTER_WHEEL:constant:=3;INPUT_POINTER_MOVE:constant:=4;
 ui,controls,pointerRepaint:Integer:=0;
 SYSCALL_GETTIME:constant:=1;
 type Mode is (Stop_On_Poll, Stop_On_Timed_Wait, Stop_On_Wait, Ordinary_Timer);
 Current:Mode:=Stop_On_Poll;
 Polls,Waits,Renders,Publishes,Timers:Natural:=0;
 Clock:Unsigned_64:=0;
 package FP is function Pending(F:Boolean) return Boolean is (F);end;
 package Client_Frame_Wakeup is
  function Deadline(Now,App:Unsigned_64;Pending:Boolean) return Unsigned_64 is
   (if App/=0 then App elsif Pending then Now+1 else 0);
 end;
 function syscall(N:Integer) return Unsigned_64 is
 begin Clock:=Clock+10;return Clock;end;
 function Is_Open(W:Window) return Boolean is (W.surfaceId/=0);
 function Full_Rect(W:Window) return Rect is ((0,0,100,100));
 function Followup(N:Integer) return Boolean is (False);
 procedure Begin_Paint(W:in out Window;D:Rect;R:out Rect;Ready:out Boolean) is
 begin R:=D;Ready:=not W.inputStopped;end;
 procedure Render(W:in out Window;R:Rect) is
 begin pragma Assert(not W.inputStopped);Renders:=Renders+1;end;
 procedure Present(W:in out Window;R:Rect) is
 begin pragma Assert(not W.inputStopped);Publishes:=Publishes+1;end;
 procedure Cancel_Paint(W:in out Window) is begin null;end;
 procedure Poll_Input(W:in out Window;E:out Input_Event;Found:out Boolean) is
 begin
  Polls:=Polls+1;pragma Assert(Polls<=2 and not W.inputStopped);
  E:=(others=><>);Found:=False;
  if Current=Stop_On_Poll then W.inputStopped:=True;end if;
 end;
 procedure Wait_Input_Until(W:in out Window;D:Unsigned_64;E:out Input_Event;Found:out Boolean) is
 begin
  pragma Assert(not W.inputStopped);Waits:=Waits+1;E:=(others=><>);Found:=False;
  if Current=Stop_On_Timed_Wait then W.inputStopped:=True;end if;
 end;
 procedure Wait_Input(W:in out Window;E:out Input_Event;Found:out Boolean) is
 begin
  pragma Assert(not W.inputStopped);Waits:=Waits+1;E:=(others=><>);Found:=False;W.inputStopped:=True;
 end;
 function Next_Deadline return Unsigned_64 is
  (if Current=Stop_On_Timed_Wait or Current=Ordinary_Timer then 15 else 0);
 procedure On_Deadline(W:in out Window;D:in out Rect;Running:in out Boolean) is
 begin pragma Assert(not W.inputStopped);Timers:=Timers+1;Running:=False;end;
 procedure Begin_Input_Event(W:in out Window;E:Input_Event) is begin null;end;
 procedure Finish_Input_Event(W:in out Window;E:Input_Event) is begin null;end;
 procedure Apply_Pointer_Event(P:in out Pointer_Interaction;U,C:Integer;W:in out Window;E:Input_Event;D:in out Rect;Policy:Integer) is begin null;end;
 procedure Handle_Event(W:in out Window;E:Input_Event;D:in out Rect;Running:in out Boolean) is begin null;end;
 function Input_May_Remain(W:Window) return Boolean is (False);
"""
suffix=r"""
 W:Window;
begin
 for M in Mode loop
  Current:=M;W:=(others=><>);Polls:=0;Waits:=0;Renders:=0;Publishes:=0;Timers:=0;Clock:=0;
  if M=Stop_On_Wait then W.frames:=False;end if;
  Run(W);
  pragma Assert(W.surfaceId=11 and W.bufferPages=11);
  pragma Assert(Polls=1);
  if M=Stop_On_Poll then pragma Assert(Waits=0 and Renders=1 and Publishes=1);
  else pragma Assert(Waits=1);end if;
  pragma Assert(Timers=(if M=Ordinary_Timer then 1 else 0));
 end loop;
 Ada.Text_IO.Put_Line("PASS terminal poll, timed/untimed waits, retained state, live timer");
end Check;
"""
variants={'baseline':body,
 'missing-loop-stop':body.replace('while running and then not win.inputStopped loop','while running loop'),
 'missing-wait-stop':body.replace('if running and then not win.inputStopped and then not hasPendingEvent then','if running and then not hasPendingEvent then'),
 'missing-timer-stop':body.replace('if win.inputStopped then\n                        running := False;\n                     elsif', 'if')}
result={'source_sha256':hashlib.sha256(source.encode()).hexdigest(),'variants':{}}
try:
 for name,text in variants.items():
  (out/'check.adb').write_text(prefix+text+suffix)
  with (out/(name+'.log')).open('w') as log:
   subprocess.run(['gnatmake','-f','-q','-gnat2022','-gnata','-gnato','check.adb'],cwd=out,stdout=log,stderr=log,check=True)
   run=subprocess.run([str(out/'check')],stdout=log,stderr=log,timeout=5)
  result['variants'][name]=run.returncode
  assert (run.returncode==0)==(name=='baseline'),name
 result['status']='PASS'
 print('PASS actual Run loop and three negative controls',out,flush=True)
finally:
 (out/'result.json').write_text(json.dumps(result,indent=2)+'\n')
 for pattern in ('*.o','*.ali','check'):
  for p in out.glob(pattern):p.unlink()
