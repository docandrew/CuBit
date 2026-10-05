"""Exercise the actual batch handler with a mocked kernel/grant boundary.

No native IPC claim. Source may be a pending native-compiled Desktop snapshot.
"""
from pathlib import Path
import argparse, hashlib, json, os, subprocess, tempfile
ROOT=Path(__file__).resolve().parents[2]
ap=argparse.ArgumentParser();ap.add_argument('--source',type=Path,default=ROOT/'userspace/services/desktop/main.adb');args=ap.parse_args()
source=args.source.read_text();start=source.index('      if request.tag.label = Compositor_Input_Protocol.Label then')
end=source.index('      if request.tag.label = Publication.Publish_Label then',start)
original=source[start:end]
hook=original.replace('CuBit.Desktop_Messages.', 'Desktop_Messages.')
(ROOT/'tests/compositor/build').mkdir(parents=True,exist_ok=True)
out=Path(tempfile.mkdtemp(prefix='input-batch-service-',dir=ROOT/'tests/compositor/build'));print(out,flush=True)
files={}
def copy(path):
 data=path.read_bytes();(out/path.name).write_bytes(data);files[str(path.relative_to(ROOT))]=hashlib.sha256(data).hexdigest()
for stem in ('compositor_close_request','compositor_input_queue','compositor_input_batches','compositor_input_batch_wire','compositor_input_protocol','compositor_input_acknowledgment','compositor_input_delivery','compositor_input_delivery_pool'):
 for ext in ('ads','adb'):copy(ROOT/'userspace/lib/compositor'/f'{stem}.{ext}')
for name in ('cubit.ads','cubit-grant_references.ads','cubit-desktop_protocol.ads','cubit-desktop_protocol.adb'):copy(ROOT/'userspace/runtime/gnat'/name)
(out/'service.gpr').write_text('''project Service is
for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use ".";
for Main use ("service_test.adb");
package Compiler is for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O1"); end Compiler;
end Service;
''')
prefix='''with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol; with CuBit.Grant_References;
with Compositor_Input_Queue; with Compositor_Input_Batches;
with Compositor_Input_Batch_Wire; with Compositor_Input_Protocol;
with Compositor_Input_Acknowledgment; with Compositor_Input_Delivery;
with Compositor_Input_Delivery_Pool; with Compositor_Close_Request;
procedure Service_Test is
package DP renames CuBit.Desktop_Protocol;
package IQ renames Compositor_Input_Queue;
subtype PendingInputQueue is IQ.Queue;
package Close_Policy renames Compositor_Close_Request;
package W renames Compositor_Input_Batch_Wire;
package BP renames Compositor_Input_Protocol;
package GR renames CuBit.Grant_References;
use type DP.Status_Code; use type IQ.Queue; use type IQ.Event; use type GR.Reference;
subtype ProcessID is Unsigned_64;
NO_PROCESS : constant ProcessID := 0;
type Tag is record Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16; end record;
type Message is record Tag : Service_Test.Tag; Words : DP.Payload; end record;
NULL_MESSAGE : constant Message := ((0,0,0,0), [others=>0]);
package Desktop_Messages is
function To_Wire (M : Message) return DP.Wire_Message is ((M.Tag.Label,M.Tag.Length,M.Tag.Flags,M.Tag.Reserved,M.Words));
function From_Wire (M : DP.Wire_Message) return Message is (((M.Label,M.Length,M.Flags,M.Reserved),M.Words));
end Desktop_Messages;
subtype SurfaceIndex is Natural range 0..1;
type Surface is record Owner : ProcessID := 42; end record;
surfaces : array (SurfaceIndex) of Surface;
type Waiter_State is record Active : Boolean := False; end record;
type InputSnapshot is record pointerPosition, buttons, modifiers, generation : Unsigned_64 := 0; end record;
type SurfaceInputChannel is record
NextSerial : Unsigned_64 := 1; exposedThrough : Unsigned_64 := 0; snapshot : InputSnapshot;
Events : IQ.Queue; PendingClose : Unsigned_64 := 0; Waiter : Waiter_State;
end record;
inputChannels : array (SurfaceIndex) of SurfaceInputChannel;
Exists, Channel_Available : Boolean := True;
Ensure_Count, Acquire_Count, Write_Count, Return_Count : Natural := 0;
Acquired_OK, Written_OK, Return_OK, Mapping_OK : Boolean := True;
Captured : W.Snapshot_Words := [others=>0];
Last_Reply : Message := NULL_MESSAGE;
function findSurface (Target : Unsigned_64) return Integer is (if Exists and Target=11 then 0 else -1);
procedure ensureInputChannel (Target : Unsigned_64; Channel_Slot : out Integer) is
begin Ensure_Count:=Ensure_Count+1; pragma Assert(Target=11); Channel_Slot:=(if Channel_Available then 0 else -1); end;
procedure Acquire (Owner : W.Identity; Grant : GR.Reference; Mapping : out W.Word; Acquired : out Boolean) is
begin
pragma Assert(Owner=42 and Grant=(17,23)); Acquire_Count:=Acquire_Count+1;
Acquired:=Acquired_OK; Mapping:=(if Mapping_OK then 4096 else 0);
end;
procedure Write (Mapping : W.Word; Payload : W.Snapshot_Words; Written : out Boolean) is
begin pragma Assert(Mapping=4096); Write_Count:=Write_Count+1; Captured:=Payload; Written:=Written_OK; end;
procedure Return_Loan (Grant : GR.Reference; Confirmed : out Boolean) is
begin pragma Assert(Grant=(17,23)); Return_Count:=Return_Count+1; Confirmed:=Return_OK; end;
package Desktop_Input_Transfer is
package Engine is new Compositor_Input_Delivery (Acquire,Write,Return_Loan);
end Desktop_Input_Transfer;
package Input_Transfers is new Compositor_Input_Delivery_Pool (8,Desktop_Input_Transfer.Engine);
Empty : constant Input_Transfers.State := (others=><>);
inputTransfers : Input_Transfers.State;
statsRequests, statsInputReq : Unsigned_64 := 0;
function reply (From : ProcessID; M : Message) return Unsigned_64 is
begin Last_Reply:=M; return 1; end;
procedure handleRequest (From : ProcessID; Request : Message) is
ReplyMsg : Message := NULL_MESSAGE; Ignore : Unsigned_64;
use type DP.Status_Code;
begin
'''
# Extract the actual enqueue path too: publication and later coalescing must
# be checked together, not just against independent policy contracts.
start_enqueue=source.index('   procedure enqueueInput\n',source.index('   end queueConfigure;'))
end_enqueue=source.index('   end enqueueInput;',start_enqueue)+len('   end enqueueInput;')
enqueue=source[start_enqueue:end_enqueue]
close_start=source.index('   procedure requestClose (target : Unsigned_64) is')
close_end=source.index('   end requestClose;',close_start)+len('   end requestClose;')
close=source[close_start:close_end]
constants=source[source.index('   INPUT_NONE '):source.index('   KEYMOD_SHIFT ')]
stubs="""inputQueueOverflows : Unsigned_64 := 0;
LF : constant String := (1 => ASCII.LF);
procedure debugPrint(S:String) is null;
procedure exitCompositor(Status:Integer) is begin raise Program_Error;end;
procedure completeInputWaiter(Target:Unsigned_64) is null;
"""
prefix=prefix.replace('procedure handleRequest (',constants+stubs+close+'\n'+enqueue+'\nprocedure handleRequest (')

# A private type's default initialization must use an object, not an aggregate.
prefix=prefix.replace('Empty : constant Input_Transfers.State := (others=><>);','Empty : Input_Transfers.State;')
suffix='''
end handleRequest;
Request : Message;
Before : IQ.Queue;
procedure Reset is
begin
inputTransfers:=Empty; inputChannels:=[others=>(others=><>)];
for I in IQ.Index range 0..2 loop inputChannels(0).Events(I):=(True,Unsigned_64(I+1),6,11,65+Unsigned_64(I),0); end loop;
inputChannels(0).PendingClose:=4; Before:=inputChannels(0).Events;
Exists:=True; Channel_Available:=True; Acquired_OK:=True; Written_OK:=True; Return_OK:=True; Mapping_OK:=True;
Ensure_Count:=0; Acquire_Count:=0; Write_Count:=0; Return_Count:=0;
Request:=Desktop_Messages.From_Wire(BP.Encode(BP.Request'(11,1,(17,23),123)));
end;
procedure Check_Failure (Status : DP.Status_Code; From : ProcessID := 42) is
begin
handleRequest(From,Request);
pragma Assert(BP.Decode(Desktop_Messages.To_Wire(Last_Reply),123).Status=Status);
pragma Assert(inputChannels(0).Events=Before and inputChannels(0).PendingClose=4);
end;
begin
Reset; handleRequest(42,Request);
pragma Assert(BP.Decode(Desktop_Messages.To_Wire(Last_Reply),123).Status=DP.Success);
pragma Assert(Acquire_Count=1 and Write_Count=1 and Return_Count=1);
pragma Assert(not inputChannels(0).Events(0).Valid and inputChannels(0).Events(1).Valid and inputChannels(0).Events(2).Valid and inputChannels(0).PendingClose=4);
declare B : constant W.Decoding := W.Decode(Captured,11,123,1); begin
pragma Assert(B.Accepted and then B.Value.Length=3 and then B.Value.Through=4 and then B.Value.Items(1).Serial=2 and then B.Value.Items(3).Kind=10); end;
Reset; Request.Tag.Length:=3; Check_Failure(DP.Invalid_Request); pragma Assert(Acquire_Count=0 and Ensure_Count=0);
Reset; Request.Words(2):=0; Check_Failure(DP.Invalid_Request); pragma Assert(Acquire_Count=0);
Reset; Request.Words(3):=0; Check_Failure(DP.Invalid_Request); pragma Assert(Acquire_Count=0);
Reset; Exists:=False; Check_Failure(DP.Bad_Object); pragma Assert(Acquire_Count=0 and Ensure_Count=0);
Reset; Check_Failure(DP.Denied,43); pragma Assert(Acquire_Count=0 and Ensure_Count=0 and inputChannels(0).exposedThrough=0);
Reset; Check_Failure(DP.Denied,0); pragma Assert(Acquire_Count=0 and Ensure_Count=0);
Reset; Channel_Available:=False; Check_Failure(DP.Resources_Exhausted); pragma Assert(Acquire_Count=0);
Reset; inputChannels(0).Waiter.Active:=True; Check_Failure(DP.Bad_State); pragma Assert(Acquire_Count=0);
Reset; inputChannels(0).Events(1).Kind:=11; Before:=inputChannels(0).Events; Check_Failure(DP.Bad_State); pragma Assert(Acquire_Count=0);
Reset; Acquired_OK:=False; Check_Failure(DP.Denied); pragma Assert(Acquire_Count=1 and Write_Count=0 and Return_Count=0);
Reset; Written_OK:=False; Check_Failure(DP.Resources_Exhausted); pragma Assert(Return_Count=1);
Reset; Mapping_OK:=False; Check_Failure(DP.Resources_Exhausted); pragma Assert(Write_Count=0 and Return_Count=1);
Reset; Return_OK:=False; Check_Failure(DP.Resources_Exhausted);
pragma Assert(Input_Transfers.Pending(inputTransfers,1));
Check_Failure(DP.Resources_Exhausted); pragma Assert(Acquire_Count=1 and Return_Count=1);
Return_OK:=True; Input_Transfers.Poll(inputTransfers); handleRequest(42,Request);
pragma Assert(BP.Decode(Desktop_Messages.To_Wire(Last_Reply),123).Status=DP.Success);
-- Fetch the complete queue in eight-event batches, retaining each current batch.
Reset; inputChannels(0).PendingClose:=0;
for I in IQ.Index loop inputChannels(0).Events(I):=(True,Unsigned_64(I+1),6,11,65,0); end loop;
pragma Assert(IQ.Capacity mod 8 = 0);
for Batch in Unsigned_64 range 0..Unsigned_64(IQ.Capacity/8-1) loop
Request:=Desktop_Messages.From_Wire(BP.Encode(BP.Request'(11,Batch*8,(17,23),123+Batch)));
handleRequest(42,Request);
declare B : constant W.Decoding := W.Decode(Captured,11,123+Batch,Batch*8); begin
pragma Assert(B.Accepted and then B.Value.Length=8 and then B.Value.Through=(Batch+1)*8 and then B.Value.More=((Batch+1)*8<Unsigned_64(IQ.Capacity))); end;
for I in IQ.Index loop pragma Assert(inputChannels(0).Events(I).Valid=(Unsigned_64(I+1)>Batch*8)); end loop;
end loop;
-- A previously exposed motion is immutable even when publication fails.
-- The writer mock captures bytes before reporting a write/return failure.
for Failure in 0..3 loop
 Reset; inputChannels := [others=>(others=><>)];
 Request:=Desktop_Messages.From_Wire(BP.Encode(BP.Request'(11,0,(17,23),123)));
 enqueueInput(INPUT_POINTER_MOVE,11,10,0);
 if Failure=1 then Written_OK:=False;
 elsif Failure=2 then Return_OK:=False;
 elsif Failure=3 then Acquired_OK:=False;end if;
 handleRequest(42,Request);
 pragma Assert(inputChannels(0).exposedThrough=1);
 enqueueInput(INPUT_POINTER_MOVE,11,98,0);
 enqueueInput(INPUT_POINTER_MOVE,11,99,0);
 pragma Assert(inputChannels(0).NextSerial=3);
 -- Same-After retry preserves the old position and adds the newer serial.
 Written_OK:=True;Return_OK:=True;Acquired_OK:=True;
 Input_Transfers.Poll(inputTransfers);
 handleRequest(42,Request);
 declare D : constant W.Decoding := W.Decode(Captured,11,123,0);begin
 pragma Assert(D.Accepted and then D.Value.Length=2 and then
   D.Value.Items(1).Serial=1 and then D.Value.Items(1).Payload0=10 and then
   D.Value.Items(2).Serial=2 and then D.Value.Items(2).Payload0=99);end;
 Request:=Desktop_Messages.From_Wire(BP.Encode(BP.Request'(11,1,(17,23),124)));
 handleRequest(42,Request);
 declare D : constant W.Decoding := W.Decode(Captured,11,124,1);begin
 pragma Assert(D.Accepted and then D.Value.Length=1 and then
   D.Value.Items(1).Serial=2 and then D.Value.Items(1).Payload0=99);end;
end loop;
-- Empty replies must not adopt an arbitrary caller-supplied Through value.
Reset; inputChannels := [others=>(others=><>)];
Request:=Desktop_Messages.From_Wire(BP.Encode(BP.Request'(11,Unsigned_64'Last,(17,23),123)));
handleRequest(42,Request);pragma Assert(inputChannels(0).exposedThrough=0);
enqueueInput(INPUT_POINTER_MOVE,11,10,0);enqueueInput(INPUT_POINTER_MOVE,11,99,0);
pragma Assert(inputChannels(0).NextSerial=2);
-- Close remains an ordering barrier between exposed and later motion.
Reset; inputChannels := [others=>(others=><>)];
Request:=Desktop_Messages.From_Wire(BP.Encode(BP.Request'(11,0,(17,23),123)));
enqueueInput(INPUT_POINTER_MOVE,11,10,0);handleRequest(42,Request);
requestClose(11);enqueueInput(INPUT_POINTER_MOVE,11,99,0);handleRequest(42,Request);
declare D : constant W.Decoding := W.Decode(Captured,11,123,0);begin
pragma Assert(D.Accepted and then D.Value.Length=3 and then
 D.Value.Items(1).Serial=1 and then D.Value.Items(2).Kind=10 and then
 D.Value.Items(2).Serial=2 and then D.Value.Items(3).Serial=3 and then
 D.Value.Items(3).Payload0=99);end;
-- Overflow must still resynchronize explicitly rather than mutate old serials.
Reset;inputChannels := [others=>(others=><>)];
Request:=Desktop_Messages.From_Wire(BP.Encode(BP.Request'(11,0,(17,23),123)));
enqueueInput(INPUT_POINTER_MOVE,11,10,0);handleRequest(42,Request);
for I in 1..IQ.Capacity loop enqueueInput(INPUT_TEXT,11,65,0);end loop;
pragma Assert(inputChannels(0).snapshot.generation=1 and inputChannels(0).exposedThrough=1);
Request:=Desktop_Messages.From_Wire(BP.Encode(BP.Request'(11,1,(17,23),124)));
handleRequest(42,Request);
declare D : constant W.Decoding := W.Decode(Captured,11,124,1);begin
pragma Assert(D.Accepted and then D.Value.Length=1 and then D.Value.Items(1).Kind=9 and then D.Value.Through=Unsigned_64(IQ.Capacity+1));end;
-- A fresh channel starts with no exposure and a new serial namespace.
inputChannels(0):=(others=><>);pragma Assert(inputChannels(0).exposedThrough=0 and inputChannels(0).NextSerial=1);
-- Unexposed adjacent motion still coalesces without consuming extra serials.
Reset;inputChannels := [others=>(others=><>)];
enqueueInput(INPUT_POINTER_MOVE,11,10,0);enqueueInput(INPUT_POINTER_MOVE,11,99,0);
pragma Assert(inputChannels(0).NextSerial=2 and inputChannels(0).Events(0).Payload0=99);
Ada.Text_IO.Put_Line("INPUT BATCH SERVICE: PASS actual handler authorization, malformed data, grant faults, retry and full-queue batch drain");
end Service_Test;
'''
variants={'baseline':hook,
 'no-exposure-freeze':hook.replace("Channel.exposedThrough := Unsigned_64'Max\n                                (Channel.exposedThrough, Value.Through);",'null;'),
 'owner-gate':hook.replace('from = NO_PROCESS or else surfaces (SurfaceIndex (Index)).owner /= from','False'),
 'ack-current':hook.replace('Channel.events, Channel.pendingClose, Decoded.Value.After);','Channel.events, Channel.pendingClose, Value.Through);'),
 'publish-failure':hook.replace('if Delivered = Transfer.Published then','if Delivered /= Transfer.Acquisition_Failed then')}
result={'source':str(args.source),'source_sha256':hashlib.sha256(source.encode()).hexdigest(),'handler_sha256':hashlib.sha256(original.encode()).hexdigest(),'inputs':files,'variants':{},'status':'INCOMPLETE'}
try:
 for name,body in variants.items():
  assert name=='baseline' or body!=hook,name
  (out/'service_test.adb').write_text(prefix+body+suffix)
  with (out/(name+'.log')).open('w') as log:
   subprocess.run(['gprbuild','-q','-p','-P',str(out/'service.gpr')],check=True,stdout=log,stderr=subprocess.STDOUT)
   ran=subprocess.run([str(out/'service_test')],stdout=log,stderr=subprocess.STDOUT)
  result['variants'][name]=ran.returncode
  assert (ran.returncode==0)==(name=='baseline'),name
 for rel,digest in files.items():assert hashlib.sha256((ROOT/rel).read_bytes()).hexdigest()==digest,rel
 (out/'service_test.adb').write_text(prefix+hook+suffix)
 with (out/'restored-baseline.log').open('w') as log:
  subprocess.run(['gprbuild','-q','-p','-P',str(out/'service.gpr')],check=True,stdout=log,stderr=subprocess.STDOUT)
  subprocess.run([str(out/'service_test')],check=True,stdout=log,stderr=subprocess.STDOUT)
 result['status']='PASS';print('PASS actual batch handler and four rejected negative controls',flush=True)
finally:(out/'result.json').write_text(json.dumps(result,indent=2)+'\n')
