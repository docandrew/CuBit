"""Actual Display prepare/flip functions with independent two-output pixel memory.

Only memory IO and GPU IPC are mocked. Tests exact inactive-buffer repair,
copy exclusion, immutable active/other-output buffers, and failed-flip state.
"""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[2]
s=(root/'userspace/services/display/main.adb').read_text()
def helper(name):
 a=s.index('   function '+name+' ');b=s.index('   end '+name+';',a)+len('   end '+name+';');return s[a:b]
prefix='''with Ada.Text_IO; with Interfaces; use Interfaces;
with System; with System.Storage_Elements; use System.Storage_Elements;
with Compositor_Repaint; with Compositor_Damage;
with CuBit.Backend_Targets; with Compositor_Requests;
procedure Check is
 use type System.Address;
 package Repair renames Compositor_Repaint;
 package BT renames CuBit.Backend_Targets;
 use type BT.Phase;
 backendSequence:Unsigned_64:=0;
 use ASCII;
 type Rect is record x,y,w,h:Natural:=0; end record;
 function isEmpty(R:Rect) return Boolean is (R.w=0 or R.h=0);
 function clampGpuRect(R:Rect) return Rect is
  (Natural'Min(R.x,16),Natural'Min(R.y,16),
   Natural'Min(R.w,16-Natural'Min(R.x,16)),Natural'Min(R.h,16-Natural'Min(R.y,16)));
 function unionRect(A,B:Rect) return Rect is
  (Natural'Min(A.x,B.x),Natural'Min(A.y,B.y),
   Natural'Max(A.x+A.w,B.x+B.w)-Natural'Min(A.x,B.x),
   Natural'Max(A.y+A.h,B.y+B.h)-Natural'Min(A.y,B.y));
 type Tag is record label:Unsigned_32;length,flags:Unsigned_8;reserved:Unsigned_16;end record;
 type Words is array(0..3) of Unsigned_64;
 type Message is record tag:Check.Tag;authorityTag:Unsigned_64;words:Check.Words;end record;
 NULL_MESSAGE:constant Message:=((0,0,0,0),0,(others=>0));
 OP_GPU_PRESENT_BUFFER:constant Unsigned_32:=77;
 CAP_SLOT_GPU:constant:=9;
 subtype MessageTag is Tag;
 subtype Gpu_Buffer_Index is Natural range 0..1;
 subtype Output_Index is Natural range 0..1;
 type Backend_Kind is (Native_GPU,Firmware);
 type Backend_Type is record Kind:Backend_Kind:=Native_GPU;end record;
 type Addresses is array(Gpu_Buffer_Index) of System.Address;
 type Output_State is record
  backend:Backend_Type;
  srcAddr:System.Address;
  srcPitch,fbPitch:Natural:=64;
  gpuScanoutAddr:Addresses;
  Targets:BT.State;
  gpuPreviousDamage:Rect;
  presentationFault:Boolean:=False;
 end record;
 outputStates:array(Output_Index) of Output_State;
 selectedOutput:Output_Index:=0;
 type Image is array(0..15,0..15) of Natural;
 type Pair is array(Gpu_Buffer_Index) of Image;
 type All_Buffers is array(Output_Index) of Pair;
 Buffers:All_Buffers:=(others=>(others=>(others=>(others=>0))));
 Sources:array(Output_Index) of Image:=(others=>(others=>(others=>0)));
 Current:Rect;
 repairCopies,backendCopies:Natural:=0;
 Calls:Natural:=0;
 Reply_OK:Boolean:=True;
 function capCall(Cap:Natural;Request:in out Message) return MessageTag is
 begin
  pragma Assert(BT.Current(outputStates(selectedOutput).Targets)=BT.In_Flight);
  pragma Assert(not BT.Writable(outputStates(selectedOutput).Targets,0) and not BT.Writable(outputStates(selectedOutput).Targets,1));
  Calls:=Calls+1;Request.words(0):=(if Reply_OK then 0 else 1);
  return(OP_GPU_PRESENT_BUFFER,1,0,0);
 end;
 procedure debugPrint(S:String) is null;
 procedure copyGpuRect(Target:System.Address;TargetPitch:Natural;
                       Source:System.Address;SourcePitch:Natural;R:Rect;Counter:in out Natural) is
  State:Output_State renames outputStates(selectedOutput);
  B:Gpu_Buffer_Index:=BT.Target(State.Targets);
 begin
  pragma Assert(BT.Writable(State.Targets,B));
  pragma Assert(Target=State.gpuScanoutAddr(B));
  pragma Assert(TargetPitch=64 and SourcePitch=64);
  for Y in R.y..R.y+R.h-1 loop
   for X in R.x..R.x+R.w-1 loop
    if Source=State.srcAddr then Buffers(selectedOutput)(B)(Y,X):=Sources(selectedOutput)(Y,X);
    else
     pragma Assert(Source=State.gpuScanoutAddr(BT.Active(State.Targets)));
     pragma Assert(not(X>=Current.x and X<Current.x+Current.w and Y>=Current.y and Y<Current.y+Current.h),"redundant overlap copy");
     Buffers(selectedOutput)(B)(Y,X):=Buffers(selectedOutput)(BT.Active(State.Targets))(Y,X);
    end if;
   end loop;
  end loop;
  Counter:=Counter+R.w*R.h*4;
 end;
'''
suffix='''
 Previous:Rect;
 Old:All_Buffers;
 State_Before:Output_State;
 Request:Message;
 Expected_Repair:Natural;
 Before_Repair,Before_Source:Natural;
 procedure Initialize is
 begin
  for O in Output_Index loop
   outputStates(O):=(srcAddr=>To_Address(Integer_Address(100+O)),
    gpuScanoutAddr=>[To_Address(Integer_Address(200+O*10)),To_Address(Integer_Address(201+O*10))],others=><>);
   BT.Cleared(outputStates(O).Targets);
  end loop;
 end;
begin
 Initialize;
 for Step in 1..2000 loop
  selectedOutput:=Step mod 2;
  Current:=(case (Step/20) mod 5 is
    when 0=>(4,4,8,8),when 1=>(2,2,12,12),when 2=>(7,0,6,15),
    when 3=>(0,7,6,9),when others=>(14,14,20,20));
  Current:=clampGpuRect(Current);
  Previous:=outputStates(selectedOutput).gpuPreviousDamage;
  Old:=Buffers;
  State_Before:=outputStates(selectedOutput);
  for Y in Current.y..Current.y+Current.h-1 loop
   for X in Current.x..Current.x+Current.w-1 loop Sources(selectedOutput)(Y,X):=Step;end loop;
  end loop;
  Expected_Repair:=0;
  for Y in 0..15 loop
   for X in 0..15 loop
    if X>=Previous.x and X<Previous.x+Previous.w and Y>=Previous.y and Y<Previous.y+Previous.h and
     not(X>=Current.x and X<Current.x+Current.w and Y>=Current.y and Y<Current.y+Current.h)
    then Expected_Repair:=Expected_Repair+4;end if;
   end loop;
  end loop;
  Before_Repair:=repairCopies;Before_Source:=backendCopies;
  Request:=prepareGpuRect(Current);
  pragma Assert(BT.Current(outputStates(selectedOutput).Targets)=BT.Preparing);
  pragma Assert(BT.Active(outputStates(selectedOutput).Targets)=BT.Active(State_Before.Targets));
  pragma Assert(prepareGpuRect(Current)=NULL_MESSAGE);
  pragma Assert(repairCopies-Before_Repair=Expected_Repair);
  pragma Assert(backendCopies-Before_Source=Current.w*Current.h*4);
  pragma Assert(Buffers(1-selectedOutput)=Old(1-selectedOutput));
  pragma Assert(Buffers(selectedOutput)(BT.Active(State_Before.Targets))=Old(selectedOutput)(BT.Active(State_Before.Targets)));
  pragma Assert(Buffers(selectedOutput)(BT.Target(State_Before.Targets))=Sources(selectedOutput));
  declare T:constant Rect:=(if isEmpty(Previous) then Current else unionRect(Previous,Current));begin
   pragma Assert(Request.tag=(OP_GPU_PRESENT_BUFFER,4,0,Unsigned_16(selectedOutput)));
   pragma Assert(Request.words=[Unsigned_64(BT.Target(State_Before.Targets)),
    Unsigned_64(T.x) or Shift_Left(Unsigned_64(T.y),32),
    Unsigned_64(T.w) or Shift_Left(Unsigned_64(T.h),32),0]);
  end;
  -- Fork the saved pre-prepare fixture to test the synchronous path independently.
  outputStates(selectedOutput):=State_Before;Buffers:=Old;
  -- Preparation does not authorize reuse; only the successful GPU reply flips.
  pragma Assert(copyAndFlipGpuRect(Current));
  pragma Assert(BT.Active(outputStates(selectedOutput).Targets)=BT.Target(State_Before.Targets));
  pragma Assert(outputStates(selectedOutput).gpuPreviousDamage=Current);
 end loop;
 for O in Output_Index loop
  selectedOutput:=O;State_Before:=outputStates(O);Old:=Buffers;
  Reply_OK:=False;Current:=(3,3,6,6);
  pragma Assert(not copyAndFlipGpuRect(Current));
  pragma Assert(outputStates(O).presentationFault);
  pragma Assert(BT.Current(outputStates(O).Targets)=BT.Failed);
  pragma Assert(prepareGpuRect(Current)=NULL_MESSAGE);
  pragma Assert(BT.Active(outputStates(O).Targets)=BT.Active(State_Before.Targets));
  pragma Assert(outputStates(O).gpuPreviousDamage=State_Before.gpuPreviousDamage);
  pragma Assert(Buffers(O)(BT.Active(State_Before.Targets))=Old(O)(BT.Active(State_Before.Targets)));
 end loop;
 Initialize;selectedOutput:=0;Old:=Buffers;Before_Repair:=repairCopies;Before_Source:=backendCopies;
 outputStates(0).srcAddr:=System.Null_Address;
 pragma Assert(prepareGpuRect((0,0,4,4))=NULL_MESSAGE);
 outputStates(0).srcAddr:=To_Address(100);outputStates(0).backend.Kind:=Firmware;
 pragma Assert(prepareGpuRect((0,0,4,4))=NULL_MESSAGE);
 outputStates(0).backend.Kind:=Native_GPU;
 pragma Assert(prepareGpuRect((16,16,4,4))=NULL_MESSAGE);
 pragma Assert(Buffers=Old and repairCopies=Before_Repair and backendCopies=Before_Source);
 Ada.Text_IO.Put_Line("DISPLAY-REPAIR: PASS 2000 exact two-output frames, minimal repair bytes, active isolation and failed-flip retention");
end Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-display-repair-') as tmp:
 d=Path(tmp)
 for name in ('compositor_damage','compositor_repaint','compositor_pool','compositor_requests'):
  for ext in ('ads','adb'):
   p=root/'userspace/lib/compositor'/f'{name}.{ext}';(d/p.name).write_bytes(p.read_bytes())
 for p in (root/'userspace/runtime/gnat/cubit.ads',root/'userspace/lib/display/cubit-backend_targets.ads',root/'userspace/lib/display/cubit-backend_targets.adb'):
  (d/p.name).write_bytes(p.read_bytes())
 (d/'check.adb').write_text(prefix+helper('prepareGpuRect')+helper('copyAndFlipGpuRect')+suffix)
 (d/'check.gpr').write_text('''project Check is
 for Source_Dirs use (".");for Object_Dir use "obj";for Exec_Dir use ".";
 for Main use ("check.adb");
 package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato");end Compiler;
end Check;''')
 subprocess.run(['gprbuild','-f','-q','-p','-P',str(d/'check.gpr')],check=True)
 subprocess.run([str(d/'check')],check=True)
 for mode in ('missing-repair','redundant-copy'):
  body=helper('prepareGpuRect')
  if mode=='missing-repair':
   body=body.replace('if Compositor_Damage.Valid (Region) then','if False then')
  else:
   a=body.index('         for Region of Repair.Before_Draw')
   b=body.index('         -- The backend still uploads',a)
   body=body[:a]+"""         copyGpuRect
           (outputStates (selectedOutput).gpuScanoutAddr (target), outputStates (selectedOutput).fbPitch,
            outputStates (selectedOutput).gpuScanoutAddr (BT.Active (outputStates (selectedOutput).Targets)), outputStates (selectedOutput).fbPitch,
            previous, repairCopies);
"""+body[b:]
  assert body!=helper('prepareGpuRect')
  (d/'check.adb').write_text(prefix+body+helper('copyAndFlipGpuRect')+suffix)
  subprocess.run(['gprbuild','-f','-q','-p','-P',str(d/'check.gpr')],check=True)
  result=subprocess.run([str(d/'check')],capture_output=True,text=True)
  assert result.returncode!=0 and 'ASSERTION_ERROR' in result.stderr,result
  print('DISPLAY-REPAIR: rejected '+mode+' mutant')
