"""Actual Desktop adapter + current unpatched SDK, mocked IPC/grant calls."""
from pathlib import Path
import runpy
import shutil
import subprocess
import tempfile

here = Path(__file__).parent
fixture = runpy.run_path(str(here/'test-metrics-publisher-boundary.py'))
root, runtime = fixture['root'], fixture['runtime']
messages = fixture['messages'].replace(' Submissions:Natural:=0;', ''' Submissions:Natural:=0;
 type Captured is array(1..100) of Unsigned_64;
 Sent_Tokens,Sent_Counts:Captured:=(others=>0);''')
message_body = fixture['message_body'].replace('Submissions:=Submissions+1;', '''Submissions:=Submissions+1;
 Sent_Tokens(Submissions):=Token; Sent_Counts(Submissions):=Msg.words(2)/64-1;''')
grants = fixture['grants'].replace(' type Grant_Reference', ' Create_OK:Boolean:=True; Creates:Natural:=0;\n type Grant_Reference')
grant_body = fixture['grant_body'].replace('Ref:=(1,1); Created:=True;', 'Creates:=Creates+1; Ref:=(1,1); Created:=Create_OK;')
main = '''with Ada.Command_Line; with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants; with CuBit.Metric_Protocol;
with Desktop_Metric_Publisher;
with Compositor_Stage_Metrics;
with Compositor_Work_Metrics;
procedure Check is
 package D is new Desktop_Metric_Publisher(10);
 package P renames CuBit.Metric_Protocol;
 Sequence:Unsigned_64:=100;
 C:CompletionEntry;
 Mode:constant String:=Ada.Command_Line.Argument(1);
 procedure Record_One(Frame:Unsigned_64:=1) is
 begin D.Record_Completion((0,1,Frame,10,20)); end;
 procedure Good_Reply(Token,Count:Unsigned_64) is
 begin
  C.token:=Token; C.valid:=True; C.status:=0;
  C.msg.tag:=(P.Status'Enum_Rep(P.OK),P.Message_Words,0,0);
  C.msg.words:=(Count,0,0,0);
 end;
begin
 D.Pump(Sequence,100_020);
 pragma Assert(Submissions=0 and Sequence=100 and not D.Matches(0));
 if Mode="normal-overload" then
  for I in 1..55 loop Record_One(Unsigned_64(I)); end loop;
  pragma Assert(Submissions=0 and CuBit.Memory_Grants.Creates=0);
  D.Pump(Sequence,20);
  pragma Assert(Submissions=1 and Sent_Counts(1)=63 and Sent_Tokens(1)=101);
  for I in 56..110 loop Record_One(Unsigned_64(I)); end loop;
  D.Pump(Sequence,20);
  pragma Assert(Submissions=2 and Sent_Counts(2)=63 and Sent_Tokens(2)=102);
  for I in 111..210 loop Record_One(Unsigned_64(I)); end loop;
  pragma Assert(D.Dropped=100 and not D.Disabled);
  D.Pump(Sequence,200_000); pragma Assert(Submissions=2 and Sequence=102);
  Good_Reply(999,63); D.Collect(C); pragma Assert(not D.Matches(999) and not D.Disabled);
  Good_Reply(102,63); D.Collect(C); D.Collect(C);
  pragma Assert(not D.Disabled and D.Rejected=0);
  Record_One(211); D.Pump(Sequence,100_020);
  pragma Assert(Submissions=3 and Sent_Counts(3)=9 and Sent_Tokens(3)=103);
  Good_Reply(101,63); D.Collect(C);
  Good_Reply(103,9); D.Collect(C);
  pragma Assert(not D.Disabled and D.Invalid=0 and D.Dropped=100);
 elsif Mode="stage-mixed" then
  for Stage in Compositor_Stage_Metrics.Stage loop
   D.Record_Stage(Stage,10,20);
  end loop;
  for Work in Compositor_Work_Metrics.Work_Kind loop D.Record_Work(Work,100,20); end loop;
  Record_One; D.Pump(Sequence,100_020);
  pragma Assert(Submissions=1 and Sent_Counts(1)=15 and D.Invalid=0);
  Good_Reply(101,15); D.Collect(C);
  pragma Assert(not D.Disabled and D.Rejected=0);
 elsif Mode="invalid-clock" then
  D.Record_Completion((0,1,1,10,9));
  D.Record_Completion((0,1,1,Unsigned_64'Last,20));
  D.Pump(Sequence,100_020);
  for Stage in Compositor_Stage_Metrics.Stage loop
   D.Record_Stage(Stage,10,9);
   D.Record_Stage(Stage,0,Unsigned_64'Last);
  end loop;
  D.Record_Work(Compositor_Work_Metrics.Scene_Pixels,100,Unsigned_64'Last);
  pragma Assert(D.Invalid=11 and Submissions=0 and not D.Disabled);
 else
  if Mode="submit-failed" then Submit_OK:=False;
  elsif Mode="grant-failed" then CuBit.Memory_Grants.Create_OK:=False;
  elsif Mode="token-exhaustion" then Sequence:=Unsigned_64'Last-1; end if;
  Record_One; D.Pump(Sequence,100_020);
  if Mode="submit-failed" or Mode="grant-failed" or Mode="token-exhaustion" then
   pragma Assert(D.Disabled);
   if Mode="token-exhaustion" then pragma Assert(Submissions=0 and CuBit.Memory_Grants.Creates=0);
   elsif Mode="grant-failed" then pragma Assert(Submissions=0 and D.Rejected=9);
   else pragma Assert(Submissions=1 and D.Rejected=9); end if;
  else
   pragma Assert(Submissions=1 and Sent_Counts(1)=9 and D.Matches(101));
   Good_Reply(101,9);
   if Mode="invalid-cqe" then C.valid:=False;
   elsif Mode="transport-failed" then C.status:=1;
   elsif Mode="bad-length" then C.msg.tag.length:=3;
   elsif Mode="bad-flags" then C.msg.tag.flags:=1;
   elsif Mode="bad-reserved" then C.msg.tag.reserved:=1;
   elsif Mode="bad-tail" then C.msg.words(3):=1;
   elsif Mode="bad-count" then C.msg.words(0):=2;
   elsif Mode="overflow-count" then C.msg.words(0):=Unsigned_64'Last;
   elsif Mode="unavailable" then C.msg.tag.label:=P.Status'Enum_Rep(P.Unavailable);
   elsif Mode="unknown-label" then C.msg.tag.label:=12345;
   elsif Mode="denied" then C.msg.tag.label:=P.Status'Enum_Rep(P.Denied); C.msg.words:=(others=>0);
   elsif Mode="definite-refusal" then C.msg.tag.label:=P.Status'Enum_Rep(P.Exhausted); C.msg.words:=(others=>0);
   else raise Program_Error; end if;
   D.Collect(C);
   if Mode="definite-refusal" then
    pragma Assert(not D.Disabled and D.Rejected=9);
    Record_One(2); D.Pump(Sequence,100_020);
    pragma Assert(Submissions=2 and Sent_Counts(2)=9 and Sent_Tokens(2)=102);
    Good_Reply(102,9); D.Collect(C); pragma Assert(not D.Disabled);
   else
    pragma Assert(D.Disabled);
    if Mode="denied" then pragma Assert(D.Rejected=9 and D.Invalid=0);
    else pragma Assert(D.Invalid=1); end if;
   end if;
  end if;
  if D.Disabled then
   declare Before:constant Natural:=Submissions; Prev:constant Unsigned_64:=Sequence;
   begin
    for I in 1..1000 loop Record_One; D.Pump(Sequence,200_000); end loop;
    pragma Assert(D.Dropped=1000 and Submissions=Before and Sequence=Prev);
   end;
  end if;
 end if;
 Ada.Text_IO.Put_Line("PASS desktop adapter " & Mode);
end Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-desktop-metrics-') as tmp:
    d=Path(tmp)
    for name in ('cubit.ads','cubit-metric_records.ads','cubit-metric_records.adb',
                 'cubit-metric_protocol.ads','cubit-metric_batches.ads','cubit-metric_batches.adb',
                 'cubit-metrics.ads','cubit-metrics.adb'):
        shutil.copyfile(runtime/name,d/name)
    for unit in ('compositor_elapsed','compositor_frame_trace','compositor_requests',
                 'compositor_work_metrics','compositor_stage_metrics','compositor_release_metrics','compositor_metric_batch_policy','compositor_metric_completion'):
        for source in (root/'userspace/lib/compositor').glob(unit+'.ad?'):
            shutil.copyfile(source,d/source.name)
    for source in (root/'userspace/services/desktop').glob('desktop_metric_publisher.ad?'):
        shutil.copyfile(source,d/source.name)
    for name,value in {'cubit-messages.ads':messages,'cubit-messages.adb':message_body,
                       'cubit-memory_grants.ads':grants,'cubit-memory_grants.adb':grant_body,
                       'check.adb':main}.items():
        (d/name).write_text(value)
    (d/'check.gpr').write_text('''project Check is
 for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use ".";
 for Main use ("check.adb");
 package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato"); end Compiler;
end Check;''')
    subprocess.run(['gprbuild','-q','-p','-P',str(d/'check.gpr')],check=True)
    modes=('normal-overload','stage-mixed','invalid-clock','submit-failed','grant-failed','token-exhaustion',
           'invalid-cqe','transport-failed','bad-length','bad-flags','bad-reserved','bad-tail',
           'bad-count','overflow-count','unavailable','unknown-label','denied','definite-refusal')
    for mode in modes:
        subprocess.run([str(d/'check'),mode],check=True)
