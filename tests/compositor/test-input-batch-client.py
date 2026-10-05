"""Actual UI App Receive_Input routing with mocked IPC and transfer boundary."""
from pathlib import Path
import hashlib,json,subprocess,tempfile
ROOT=Path(__file__).resolve().parents[2]
source=(ROOT/'userspace/lib/ui/cubit-ui-app.adb').read_text()
a=source.index('   procedure Take_Cached_Input\n');b=source.index('   end Receive_Input;',a)+len('   end Receive_Input;')
original=source[a:b];routine=original.replace('CuBit.Desktop_Messages.','Desktop_Messages.')
helper=source[source.index('   procedure Add_Input_Count '):source.index('   function Input_Statistics ')]
spec=(ROOT/'userspace/lib/ui/cubit-ui-app.ads').read_text()
diag=spec[spec.index('   type Input_Diagnostics is record'):spec.index('   end record;',spec.index('   type Input_Diagnostics is record'))+len('   end record;')]
(ROOT/'tests/compositor/build').mkdir(parents=True,exist_ok=True)
out=Path(tempfile.mkdtemp(prefix='input-batch-client-',dir=ROOT/'tests/compositor/build'));print(out,flush=True)
inputs={}
for directory,names in [('userspace/lib/ui',['client_input_batch_cache']),('userspace/lib/compositor',['compositor_input_protocol','compositor_input_batch_wire','compositor_input_batches','compositor_input_queue'])]:
 for name in names:
  for ext in ('ads','adb'):
   p=ROOT/directory/f'{name}.{ext}';data=p.read_bytes();(out/p.name).write_bytes(data);inputs[str(p.relative_to(ROOT))]=hashlib.sha256(data).hexdigest()
for name in ('cubit.ads','cubit-grant_references.ads','cubit-desktop_protocol.ads','cubit-desktop_protocol.adb'):
 p=ROOT/'userspace/runtime/gnat'/name;data=p.read_bytes();(out/name).write_bytes(data);inputs[str(p.relative_to(ROOT))]=hashlib.sha256(data).hexdigest()
(out/'client.gpr').write_text('''project Client is
for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("client_test.adb");
package Compiler is for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato","-O1"); end Compiler;
end Client;''')
prefix='''with Ada.Text_IO; with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol; with Client_Input_Batch_Cache;
procedure Client_Test is
package DP renames CuBit.Desktop_Protocol;
package C renames Client_Input_Batch_Cache;
use type DP.Status_Code; use type DP.Operation; use type DP.Input_Event_Kind;
type Tag is record Label:Unsigned_32; Length,Flags:Unsigned_8; Reserved:Unsigned_16; end record;
type Message is record Tag:Client_Test.Tag; Words:DP.Payload; end record;
package Desktop_Messages is
function To_Wire(M:Message) return DP.Wire_Message is ((M.Tag.Label,M.Tag.Length,M.Tag.Flags,M.Tag.Reserved,M.Words));
function From_Wire(M:DP.Wire_Message) return Message is (((M.Label,M.Length,M.Flags,M.Reserved),M.Words));
end;
type Input_Event is record Kind,Serial,Payload0,Payload1:Unsigned_64:=0; end record;
'''+diag+'''
type Window is record
surfaceId:Unsigned_64:=11; lastEvent:Unsigned_64:=0;
batchedInput:Boolean:=True; inputMayRemain:Boolean:=False; waiting:Boolean:=False;
inputStopped,inputErrorReported:Boolean:=False; bufferPages:Unsigned_64:=11;
inputCache:C.State; inputStats:Input_Diagnostics; width,height:Natural:=100;
end record;
Fetch_Count,Call_Count,Theme_Calls,Buffer_Calls:Natural:=0; Event_Kind:Unsigned_64:=6; Fetch_OK:Boolean:=True; Batch_Length:C.W.B.Count:=3;
Error_Logs:Natural:=0; Reply_Status:DP.Status_Code:=DP.Success;
CAP_SLOT_DESKTOP:constant:=15;
function Input_Wait_Pending(win:Window) return Boolean is (win.waiting);
procedure Apply_Input_Result(win:in out Window; decoded:DP.Input_Result; event:out Input_Event; found:out Boolean) is
begin
found:=False;event:=(others=><>);
if decoded.Status/=DP.Success then return;end if;
win.lastEvent:=decoded.Value.Serial;win.inputMayRemain:=decoded.Value.More_Pending;
found:=decoded.Value.Kind/=DP.No_Input;
event:=(DP.Input_Event_Kind'Enum_Rep(decoded.Value.Kind),decoded.Value.Serial,decoded.Value.Payload0,decoded.Value.Payload1);
end;
package Client_Input_Channel is
procedure Fetch(S:in out C.State; Surface:C.W.Identity; After:C.W.Word; Loaded:out Boolean);
end;
package body Client_Input_Channel is
procedure Fetch(S:in out C.State; Surface:C.W.Identity; After:C.W.Word; Loaded:out Boolean) is
Batch:C.W.B.Batch:=(Length=>Batch_Length,Through=>After+Unsigned_64(Batch_Length),others=><>);
begin
Fetch_Count:=Fetch_Count+1;Loaded:=False;if not Fetch_OK then return;end if;
for I in 1..Batch_Length loop Batch.Items(I):=(True,After+Unsigned_64(I),Event_Kind,Surface,(if Event_Kind=8 then 640 else 65+Unsigned_64(I)),(if Event_Kind=8 then 480 else 0));end loop;
C.Load(S,C.W.Encode(Batch,Surface,123,After),C.P.Encode(C.P.Receipt'(DP.Success,123,Batch_Length,Batch.Through,False)),Surface,123,After,Loaded);
end;end;
function capCall(Slot:Unsigned_64; M:in out Message) return Tag is
D:constant DP.Input_Request_Decoding:=DP.Decode_Input_Request(Desktop_Messages.To_Wire(M));
begin
pragma Assert(Slot=CAP_SLOT_DESKTOP and D.Valid);Call_Count:=Call_Count+1;
if Reply_Status/=DP.Success then M:=Desktop_Messages.From_Wire(DP.Encode_Status(D.Value.Kind,Reply_Status));return M.Tag;end if;
M:=Desktop_Messages.From_Wire(DP.Encode_Input_Reply(D.Value.Kind,(DP.Text_Entered,D.Value.After_Serial+1,90,0,False)));
return M.Tag;
end;
'''
suffix='''
Win:Window; E:Input_Event; Found:Boolean; Count:Unsigned_64;
procedure Reset is
begin Win:=(others=><>);Error_Logs:=0;Reply_Status:=DP.Success;Fetch_Count:=0;Call_Count:=0;Fetch_OK:=True;Batch_Length:=3;Event_Kind:=6;Theme_Calls:=0;Buffer_Calls:=0;end;
begin
Count:=Unsigned_64'Last-1;Add_Input_Count(Count,8);pragma Assert(Count=Unsigned_64'Last);
Add_Input_Count(Count);pragma Assert(Count=Unsigned_64'Last);
Reset;Receive_Input(Win,DP.Poll_Input,E,Found);
pragma Assert(Win.inputStats.Successful_Fetches=1 and Win.inputStats.Fetched_Events=3 and Win.inputStats.Delivered_Events=1 and Win.inputStats.Fallback_Polls=0);
pragma Assert(Found and E.Serial=1 and Win.lastEvent=1 and Win.inputMayRemain and Fetch_Count=1 and Call_Count=0);
Receive_Input(Win,DP.Wait_Input,E,Found,999);
pragma Assert(Found and E.Serial=2 and Win.lastEvent=2 and Call_Count=0 and Fetch_Count=1);
Receive_Input(Win,DP.Wait_Input,E,Found,999);
pragma Assert(Found and E.Serial=3 and not Win.inputMayRemain and Call_Count=0);
Receive_Input(Win,DP.Wait_Input,E,Found,999);
pragma Assert(Found and E.Serial=4 and Call_Count=1 and Fetch_Count=1);
pragma Assert(Win.inputStats.Delivered_Events=3 and Win.inputStats.Successful_Fetches=1);
Reset;Batch_Length:=0;Receive_Input(Win,DP.Poll_Input,E,Found);
pragma Assert(not Found and Win.lastEvent=0 and not Win.inputMayRemain and Call_Count=0 and Fetch_Count=1);
pragma Assert(Win.inputStats.Successful_Fetches=1 and Win.inputStats.Fetched_Events=0 and Win.inputStats.Delivered_Events=0);
Reset;Fetch_OK:=False;Receive_Input(Win,DP.Poll_Input,E,Found);
pragma Assert(Found and E.Serial=1 and Call_Count=1 and Fetch_Count=1);
pragma Assert(Win.inputStats.Fallback_Polls=1 and Win.inputStats.Successful_Fetches=0);
Reset;Win.batchedInput:=False;Receive_Input(Win,DP.Poll_Input,E,Found);
pragma Assert(Found and Call_Count=1 and Fetch_Count=0);
Reset;Win.waiting:=True;Receive_Input(Win,DP.Poll_Input,E,Found);
pragma Assert(not Found and Call_Count=0 and Fetch_Count=0);
Reset;Win.surfaceId:=0;Receive_Input(Win,DP.Wait_Input,E,Found);
pragma Assert(not Found and Call_Count=0 and Fetch_Count=0);
Reset;Client_Input_Channel.Fetch(Win.inputCache,12,0,Found);Fetch_Count:=0;
Receive_Input(Win,DP.Poll_Input,E,Found);
pragma Assert(Found and E.Serial=1 and Call_Count=1 and Fetch_Count=0 and C.Remaining(Win.inputCache)=0);
pragma Assert(Win.inputStats.Cache_Rejections=1 and Win.inputStats.Delivered_Events=0);
Reset;Poll_Cached_Input(Win,E,Found);
pragma Assert(not Found and Fetch_Count=0 and Call_Count=0 and Win.lastEvent=0);
Reset;Client_Input_Channel.Fetch(Win.inputCache,11,0,Found);Fetch_Count:=0;
Win.waiting:=True;Poll_Cached_Input(Win,E,Found);
pragma Assert(not Found and C.Remaining(Win.inputCache)=3 and Fetch_Count=0 and Call_Count=0);
Win.waiting:=False;
for I in 1..3 loop
 Poll_Cached_Input(Win,E,Found);
 pragma Assert(Found and E.Serial=Unsigned_64(I) and Win.lastEvent=Unsigned_64(I));
 pragma Assert(Fetch_Count=0 and Call_Count=0);
end loop;
Poll_Cached_Input(Win,E,Found);
pragma Assert(not Found and Fetch_Count=0 and Call_Count=0 and Win.lastEvent=3);
-- Wrong surface and wrong acknowledgment invalidate cached data. No hidden
-- input request is permitted; normal polling may recover in a later batch.
for Wrong_Ack in Boolean loop
 Reset;Client_Input_Channel.Fetch(Win.inputCache,(if Wrong_Ack then 11 else 12),0,Found);Fetch_Count:=0;
 if Wrong_Ack then Win.lastEvent:=1;end if;
 Count:=Win.lastEvent;
 Poll_Cached_Input(Win,E,Found);
 pragma Assert(not Found and C.Remaining(Win.inputCache)=0 and Win.lastEvent=Count);
 pragma Assert(Fetch_Count=0 and Call_Count=0 and Win.inputStats.Cache_Rejections=1);
 Receive_Input(Win,DP.Poll_Input,E,Found);
 pragma Assert(Found and E.Serial=Count+1 and Fetch_Count=1 and Call_Count=0);
end loop;
Reset;Client_Input_Channel.Fetch(Win.inputCache,11,0,Found);Fetch_Count:=0;
Win.batchedInput:=False;Poll_Cached_Input(Win,E,Found);
pragma Assert(not Found and C.Remaining(Win.inputCache)=3 and Fetch_Count=0 and Call_Count=0);
Win.batchedInput:=True;Win.surfaceId:=0;Poll_Cached_Input(Win,E,Found);
pragma Assert(not Found and C.Remaining(Win.inputCache)=3 and Fetch_Count=0 and Call_Count=0);
for Kind in Unsigned_64 range 8..9 loop
 Reset;Event_Kind:=Kind;Batch_Length:=1;
 Client_Input_Channel.Fetch(Win.inputCache,11,0,Found);Fetch_Count:=0;
 Poll_Cached_Input(Win,E,Found);
 pragma Assert(Found and E.Kind=Kind and E.Serial=1 and Win.lastEvent=1);
 pragma Assert(Fetch_Count=0 and Call_Count=0 and Theme_Calls=1);
 pragma Assert(Buffer_Calls=(if Kind=8 then 1 else 0));
 if Kind=8 then pragma Assert(Win.width=640 and Win.height=480 and E.Payload0=640 and E.Payload1=480);end if;
end loop;
-- A validated terminal reply stops subsequent polling/cached input, but does
-- not erase surface identity or the retained frame allocation.
Reset;Win.batchedInput:=False;Reply_Status:=DP.Bad_Object;
Receive_Input(Win,DP.Wait_Input,E,Found,999);
pragma Assert(not Found and Win.inputStopped and Error_Logs=1 and Call_Count=1);
pragma Assert(Win.surfaceId=11 and Win.bufferPages=11 and Win.lastEvent=0);
for I in 1..10 loop
 Receive_Input(Win,DP.Poll_Input,E,Found);
 Receive_Input(Win,DP.Wait_Input,E,Found,999);
 Poll_Cached_Input(Win,E,Found);
end loop;
pragma Assert(Call_Count=1 and Fetch_Count=0 and Error_Logs=1);
declare Sibling:Window; begin
 Reply_Status:=DP.Success;Receive_Input(Sibling,DP.Poll_Input,E,Found);
 pragma Assert(Found and not Sibling.inputStopped);
end;
-- Other failures remain retryable, log once per failure episode, and a
-- successful response clears that episode. A malformed Bad_Object is not final.
Reset;Win.batchedInput:=False;Reply_Status:=DP.Invalid_Request;
for I in 1..3 loop Receive_Input(Win,DP.Poll_Input,E,Found);end loop;
pragma Assert(not Win.inputStopped and Error_Logs=1 and Call_Count=3);
Reply_Status:=DP.Success;Receive_Input(Win,DP.Poll_Input,E,Found);
pragma Assert(Found and not Win.inputErrorReported);
declare W:DP.Wire_Message:=DP.Encode_Status(DP.Poll_Input,DP.Bad_Object);begin
 W.Reserved:=1;
 Apply_Input_Result(Win,DP.Decode_Input_Result(W,DP.Poll_Input),E,Found);
 pragma Assert(not Win.inputStopped and Error_Logs=2);
end;
Ada.Text_IO.Put_Line("INPUT BATCH CLIENT: PASS actual Receive_Input cache-first poll/wait, empty reply, fallback, opt-out and context recovery");
end Client_Test;
'''
apply_start=source.index('   procedure Apply_Input_Result\n')
apply_end=source.index('   end Apply_Input_Result;',apply_start)+len('   end Apply_Input_Result;')
apply=source[apply_start:apply_end]
start=prefix.index('procedure Apply_Input_Result(')
end=prefix.index('package Client_Input_Channel is',start)
stubs="""INPUT_CONFIGURE:constant Unsigned_64:=8; INPUT_RESYNC:constant Unsigned_64:=9;
LF:constant Character:=ASCII.LF;
procedure debugPrint(S:String) is begin Error_Logs:=Error_Logs+1;end;
procedure Refresh_Theme is begin Theme_Calls:=Theme_Calls+1;end;
function Content_Size_From_Surface(win:Window; N:Unsigned_64; Horizontal:Boolean) return Natural is (Natural(N));
procedure Ensure_Buffer(win:in out Window; W,H:Natural; Resized:out Boolean) is
begin Buffer_Calls:=Buffer_Calls+1;win.width:=W;win.height:=H;Resized:=True;end;
"""
prefix=prefix[:start]+stubs+apply+'\n'+prefix[end:]
prefix+=helper
variants={'baseline':routine,'terminal-bypass':routine.replace('or else win.inputStopped ', ''),'wait-bypass':routine.replace('if Cache.Remaining (win.inputCache) > 0 then','if Cache.Remaining (win.inputCache) > 0 and then operation = DP.Poll_Input then'),'ack-ahead':routine.replace('Apply_Input_Result (win, Decoded, event, found);','Apply_Input_Result (win, Decoded, event, found); win.lastEvent := win.lastEvent + 1;')}
result={'status':'INCOMPLETE','source_sha256':hashlib.sha256(source.encode()).hexdigest(),'routine_sha256':hashlib.sha256(original.encode()).hexdigest(),'inputs':inputs,'variants':{}}
try:
 for name,body in variants.items():
  assert name=='baseline' or body!=routine
  (out/'client_test.adb').write_text(prefix+body+suffix)
  with (out/(name+'.log')).open('w') as log:
   subprocess.run(['gprbuild','-q','-p','-P',str(out/'client.gpr')],check=True,stdout=log,stderr=subprocess.STDOUT)
   run=subprocess.run([str(out/'client_test')],stdout=log,stderr=subprocess.STDOUT)
  result['variants'][name]=run.returncode;assert (run.returncode==0)==(name=='baseline'),name
 for rel,digest in inputs.items():assert hashlib.sha256((ROOT/rel).read_bytes()).hexdigest()==digest,rel
 (out/'client_test.adb').write_text(prefix+routine+suffix)
 with (out/'restored-baseline.log').open('w') as log:
  subprocess.run(['gprbuild','-q','-p','-P',str(out/'client.gpr')],check=True,stdout=log,stderr=subprocess.STDOUT)
  subprocess.run([str(out/'client_test')],check=True,stdout=log,stderr=subprocess.STDOUT)
 result['status']='PASS';print('PASS actual client routing and three rejected negative controls',flush=True)
finally:(out/'result.json').write_text(json.dumps(result,indent=2)+'\n')
