from pathlib import Path
import subprocess, tempfile
root=Path(__file__).resolve().parents[2]
s=(root/'userspace/services/display/main.adb').read_text()
a=s.index('   procedure collectFrames is'); b=s.index('   end collectFrames;',a)+len('   end collectFrames;')
body=s[a:b]
prefix='''with Ada.Text_IO; with Interfaces; use Interfaces;
with System; with CuBit.Backend_Targets;
procedure Check is
 package BT renames CuBit.Backend_Targets;use type BT.Phase;
 use ASCII;
 subtype Output_Index is Natural range 0..1;
 type Rect is record x,y,w,h:Natural:=0;end record;
 type Tag is record label:Unsigned_32;length,flags:Unsigned_8;reserved:Unsigned_16;end record;
 type Words is array(0..3) of Unsigned_64;
 type Message is record tag:Check.Tag;words:Check.Words;end record;
 type CompletionEntry is record token:Unsigned_64;valid:Boolean;status:Natural;msg:Message;end record;
 OP_GPU_PRESENT_BUFFER:constant:=77;COMPLETION_OK:constant:=0;
 COMPLETION_QUEUE_SIZE:constant:=8;
 package DSP is type Frame_Result is record Session:Natural:=1;end record;end DSP;
 package PS is
  subtype Submission_ID is Natural;procedure Close(S:in out Natural);
 end PS;
 package body PS is procedure Close(S:in out Natural) is begin S:=99;end;end PS;
 type Output_State is record
  Targets:BT.State;
  backendToken:Unsigned_64:=0;
  backendTarget:Natural:=1;
  backendFrame:DSP.Frame_Result;
  activeSession,sessionOutput,currentOutput,sourceOutput:Natural:=1;
  gpuPreviousDamage,backendDamage:Rect;
  backendID,frameState:Natural:=0;
  presentationFault:Boolean:=False;
 end record;
 type States is array(Output_Index) of Output_State;
 outputStates:States;
 Queue:array(1..16) of CompletionEntry;
 Count,Next,Replies,Finishes:Natural:=0;
 Last_Published:Boolean:=False;
 Usable:Boolean:=True;
 function outputUsable(O:Natural) return Boolean is (Usable);
 function Poll_Completion(A:System.Address) return Natural is
  Result:CompletionEntry with Import,Address=>A;
 begin
  if Next=Count then return 0;end if;
  Next:=Next+1;Result:=Queue(Next);return 1;
 end;
 function frameReplySlot(O:Output_Index) return Natural is (O);
 function replyCap(S:Natural;R:Message) return Unsigned_64 is
 begin Replies:=Replies+1;return 0;end;
 procedure debugPrint(S:String) is null;
 procedure finishFrame(Output:Output_Index;ID:PS.Submission_ID;
  Result:in out DSP.Frame_Result;Published,Certain:Boolean;Response:out Message) is
 begin
  Finishes:=Finishes+1;Last_Published:=Published;
  pragma Assert(Published=Certain);
  if not Published then
   outputStates(Output).presentationFault:=True;
   BT.Quarantine(outputStates(Output).Targets);
  end if;
  Response:=((0,0,0,0),(others=>0));
 end;
'''
suffix='''
 Accepted:Boolean;
 Saved:States;
 procedure Initialize is
 begin
  outputStates:=(others=><>);Usable:=True;
  Count:=1;Next:=0;Replies:=0;Finishes:=0;Last_Published:=False;
  for O in Output_Index loop
   BT.Cleared(outputStates(O).Targets);
   BT.Prepare(outputStates(O).Targets,Accepted);pragma Assert(Accepted);
   BT.Seal(outputStates(O).Targets,Unsigned_64(100+O));
   outputStates(O).backendToken:=Unsigned_64(100+O);
   outputStates(O).backendDamage:=(1,2,3,4);
  end loop;
  Queue(1):=(100,True,COMPLETION_OK,((OP_GPU_PRESENT_BUFFER,1,0,0),(others=>0)));
 end;
begin
 for O in Output_Index loop
  Initialize;Saved:=outputStates;Queue(1).token:=Unsigned_64(100+O);
  collectFrames;
  pragma Assert(Finishes=1 and Replies=1 and Last_Published);
  pragma Assert(BT.Current(outputStates(O).Targets)=BT.Idle and BT.Active(outputStates(O).Targets)=1);
  pragma Assert(outputStates(O).gpuPreviousDamage=outputStates(O).backendDamage);
  pragma Assert(outputStates(1-O)=Saved(1-O));
  -- Duplicate completion after retirement must never release another target.
  Next:=0;collectFrames;
  for State of outputStates loop pragma Assert(BT.Current(State.Targets)=BT.Failed);end loop;
  pragma Assert(Finishes=1 and Replies=1);
 end loop;
 for Fault in 0..9 loop
  Initialize;Saved:=outputStates;
  case Fault is
   when 0=>Queue(1).valid:=False;
   when 1=>Queue(1).status:=1;
   when 2=>Queue(1).msg.tag.length:=2;
   when 3=>Queue(1).msg.words(0):=1;
   when 4=>outputStates(0).activeSession:=2;
   when 5=>outputStates(0).sessionOutput:=2;
   when 6=>outputStates(0).sourceOutput:=2;
   when 7=>Usable:=False;
   when 8=>outputStates(0).backendTarget:=0;
   when others=>BT.Quarantine(outputStates(0).Targets);
  end case;
  collectFrames;
  pragma Assert(Finishes=1 and Replies=1 and not Last_Published);
  pragma Assert(BT.Current(outputStates(0).Targets)=BT.Failed);
  pragma Assert(BT.Active(outputStates(0).Targets)=0 and BT.Token(outputStates(0).Targets)=100);
  pragma Assert(outputStates(0).gpuPreviousDamage=Saved(0).gpuPreviousDamage);
  pragma Assert(outputStates(1)=Saved(1));
 end loop;
 Initialize;Queue(1).token:=999;Count:=16;
 for I in 2..16 loop Queue(I):=Queue(1);end loop;
 collectFrames;
 pragma Assert(Next=COMPLETION_QUEUE_SIZE and Finishes=0 and Replies=0);
 for State of outputStates loop
  pragma Assert(BT.Current(State.Targets)=BT.Failed and BT.Active(State.Targets)=0);
 end loop;
 Ada.Text_IO.Put_Line("BACKEND-COMPLETIONS: PASS actual two-output routing, 10 fault modes, duplicate quarantine and bounded unknown-completion flood");
end Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-backend-completions-') as tmp:
 d=Path(tmp)
 for p in (root/'userspace/runtime/gnat/cubit.ads',root/'userspace/lib/display/cubit-backend_targets.ads',root/'userspace/lib/display/cubit-backend_targets.adb'):
  (d/p.name).write_bytes(p.read_bytes())
 (d/'check.adb').write_text(prefix+body+suffix)
 (d/'check.gpr').write_text('project Check is for Source_Dirs use (".");for Object_Dir use "obj";for Exec_Dir use ".";for Main use ("check.adb");package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato");end Compiler;end Check;')
 subprocess.run(['gprbuild','-f','-q','-p','-P',str(d/'check.gpr')],check=True)
 subprocess.run([str(d/'check')],check=True)
