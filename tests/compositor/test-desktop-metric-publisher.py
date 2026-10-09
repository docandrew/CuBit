"""Actual Desktop adapter + current unpatched SDK, mocked IPC/grant calls."""
from pathlib import Path
import runpy
import shutil
import subprocess
import tempfile

here = Path(__file__).parent
fixture = runpy.run_path(str(here/'test-metrics-publisher-boundary.py'))
root, runtime = fixture['root'], fixture['runtime']
messages = fixture['messages'].replace('with Interfaces; use Interfaces;', 'with Interfaces; use Interfaces; with CuBit.Metric_Records;').replace(' Submissions:Natural:=0;', ''' Submissions:Natural:=0;
 type Captured is array(1..100) of Unsigned_64;
 Sent_Tokens,Sent_Counts:Captured:=(others=>0);
 type Page_Copies is array(1..100) of CuBit.Metric_Records.Page_Words;
 Snapshots:Page_Copies;''')
message_body = fixture['message_body'].replace('package body CuBit.Messages is', 'with CuBit.Memory_Grants; with CuBit.Metric_Records; package body CuBit.Messages is').replace('Submissions:=Submissions+1;', '''Submissions:=Submissions+1;
 Sent_Tokens(Submissions):=Token; Sent_Counts(Submissions):=Msg.words(2)/64-1;
 declare Page:CuBit.Metric_Records.Page_Words with Import,
  Address=>CuBit.Memory_Grants.Addresses(Natural(Msg.words(0)));
 begin Snapshots(Submissions):=Page; end;''')
grants = fixture['grants'].replace(' type Grant_Reference', ' Create_OK:Boolean:=True; Creates:Natural:=0;\n type Address_Table is array(1..2) of System.Address; Addresses:Address_Table;\n type Grant_Reference')
grant_body = fixture['grant_body'].replace('Ref:=(1,1); Created:=True;', 'Creates:=Creates+1; Ref:=(Unsigned_64(Creates),1); Addresses(Creates):=Address; Created:=Create_OK;')
main = '''with Ada.Command_Line; with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants; with CuBit.Metric_Protocol;
with Desktop_Metric_Publisher;
with Compositor_Stage_Metrics;
with Compositor_Work_Metrics;
with Compositor_Trace_Wire; with CuBit.Metric_Records;
procedure Check is
 package D is new Desktop_Metric_Publisher(10);
 package P renames CuBit.Metric_Protocol;
 package R renames CuBit.Metric_Records;
 package W renames Compositor_Trace_Wire;
 use type R.Record_Kind, R.Page_Words;
 Trace_Input:constant W.Event:=(W.Input_Event,999,(77,88,1,20));
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
 if Mode="trace-overload" then
  for I in 1..12 loop D.Record_Trace(Trace_Input); end loop;
  pragma Assert(Submissions=0 and CuBit.Memory_Grants.Creates=0 and D.Delay_Us(20)=0);
  D.Pump(Sequence,20);
  pragma Assert(Submissions=1 and Sent_Counts(1)=60);
  for I in 1..12 loop D.Record_Trace(Trace_Input); end loop;
  D.Pump(Sequence,20);
  pragma Assert(Submissions=2 and Sent_Counts(2)=60);
  for I in 1..100 loop D.Record_Trace(Trace_Input); end loop;
  pragma Assert(D.Dropped=400 and not D.Disabled);
  declare Held:R.Page_Words with Import, Address=>CuBit.Memory_Grants.Addresses(1);
  begin pragma Assert(Held=Snapshots(1)); end;
  Good_Reply(102,60); D.Collect(C);
  D.Record_Trace((W.Input_Event,0,(0,0,0,0)));
  D.Record_Unsupported_Trace;
  D.Record_Trace(Trace_Input);
  D.Record_Trace_Status(20);
  D.Pump(Sequence,100_020);
  pragma Assert(Submissions=3 and Sent_Counts(3)=22 and D.Invalid=1);
  for Part in 0..3 loop
   declare V:constant R.Decoded_Record:=R.Decode(R.Slot(Snapshots(3),13+Part));
   begin pragma Assert(V.Success and then V.Value.Kind=R.Trace and then
     V.Value.Trace_ID=125 and then V.Value.Part=Part); end;
  end loop;
  for Offset in 0..2 loop
   declare V:constant R.Decoded_Record:=R.Decode(R.Slot(Snapshots(3),18+Offset*2));
   begin pragma Assert(V.Success and then V.Value.Kind=R.Gauge and then
     V.Value.Key=13+Offset and then V.Value.Value=(if Offset=1 then 100 else 1)); end;
  end loop;
  Good_Reply(101,60); D.Collect(C); Good_Reply(103,22); D.Collect(C);
  pragma Assert(not D.Disabled and D.Rejected=0);
 elsif Mode="trace-tail" then
  for I in 1..12 loop D.Record_Trace(Trace_Input); end loop;
  D.Record_Trace(Trace_Input);
  pragma Assert(Submissions=0 and D.Dropped=4 and D.Delay_Us(20)=0);
  D.Pump(Sequence,20); pragma Assert(Submissions=1 and Sent_Counts(1)=60);
  Good_Reply(101,60); D.Collect(C);
  D.Record_Trace(Trace_Input); D.Pump(Sequence,100_020);
  pragma Assert(Submissions=2 and Sent_Counts(2)=16);
  declare V:constant R.Decoded_Record:=R.Decode(R.Slot(Snapshots(2),13));
  begin pragma Assert(V.Success and then V.Value.Trace_ID=14); end;
 elsif Mode="normal-overload" then
  for I in 1..51 loop Record_One(Unsigned_64(I)); end loop;
  pragma Assert(Submissions=0 and CuBit.Memory_Grants.Creates=0);
  D.Pump(Sequence,20);
  pragma Assert(Submissions=1 and Sent_Counts(1)=63 and Sent_Tokens(1)=101);
  for I in 52..102 loop Record_One(Unsigned_64(I)); end loop;
  D.Pump(Sequence,20);
  pragma Assert(Submissions=2 and Sent_Counts(2)=63 and Sent_Tokens(2)=102);
  for I in 103..202 loop Record_One(Unsigned_64(I)); end loop;
  pragma Assert(D.Dropped=100 and not D.Disabled);
  D.Pump(Sequence,200_000); pragma Assert(Submissions=2 and Sequence=102);
  Good_Reply(999,63); D.Collect(C); pragma Assert(not D.Matches(999) and not D.Disabled);
  Good_Reply(102,63); D.Collect(C); D.Collect(C);
  pragma Assert(not D.Disabled and D.Rejected=0);
  Record_One(203); D.Pump(Sequence,100_020);
  pragma Assert(Submissions=3 and Sent_Counts(3)=13 and Sent_Tokens(3)=103);
  Good_Reply(101,63); D.Collect(C);
  Good_Reply(103,13); D.Collect(C);
  pragma Assert(not D.Disabled and D.Invalid=0 and D.Dropped=100);
 elsif Mode="stage-mixed" then
  for Stage in Compositor_Stage_Metrics.Stage loop
   D.Record_Stage(Stage,10,20);
  end loop;
  for Work in Compositor_Work_Metrics.Work_Kind loop D.Record_Work(Work,100,20); end loop;
  Record_One; D.Pump(Sequence,100_020);
  pragma Assert(Submissions=1 and Sent_Counts(1)=23 and D.Invalid=0);
  Good_Reply(101,23); D.Collect(C);
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
  pragma Assert(D.Invalid=15 and Submissions=0 and not D.Disabled);
 else
  if Mode="submit-failed" then Submit_OK:=False;
  elsif Mode="grant-failed" then CuBit.Memory_Grants.Create_OK:=False;
  elsif Mode="token-exhaustion" then Sequence:=Unsigned_64'Last-1; end if;
  Record_One; D.Pump(Sequence,100_020);
  if Mode="submit-failed" or Mode="grant-failed" or Mode="token-exhaustion" then
   pragma Assert(D.Disabled);
   if Mode="token-exhaustion" then pragma Assert(Submissions=0 and CuBit.Memory_Grants.Creates=0);
   elsif Mode="grant-failed" then pragma Assert(Submissions=0 and D.Rejected=13);
   else pragma Assert(Submissions=1 and D.Rejected=13); end if;
  else
   pragma Assert(Submissions=1 and Sent_Counts(1)=13 and D.Matches(101));
   Good_Reply(101,13);
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
    pragma Assert(not D.Disabled and D.Rejected=13);
    Record_One(2); D.Pump(Sequence,100_020);
    pragma Assert(Submissions=2 and Sent_Counts(2)=13 and Sent_Tokens(2)=102);
    Good_Reply(102,13); D.Collect(C); pragma Assert(not D.Disabled);
   else
    pragma Assert(D.Disabled);
    if Mode="denied" then pragma Assert(D.Rejected=13 and D.Invalid=0);
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
    for name in ('cubit.ads','cubit-protocols.ads','cubit-log_protocol.ads','cubit-log_records.ads','cubit-log_records.adb','cubit-metric_records.ads','cubit-metric_records.adb',
                 'cubit-metric_protocol.ads','cubit-metric_batches.ads','cubit-metric_batches.adb',
                 'cubit-metrics.ads','cubit-metrics.adb'):
        shutil.copyfile(runtime/name,d/name)
    for unit in ('compositor_trace_publication','compositor_trace_metrics','compositor_trace_wire','compositor_input_trace','compositor_source_trace','compositor_render_trace','compositor_elapsed','compositor_frame_trace','compositor_requests',
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
    modes=('trace-overload','trace-tail','normal-overload','stage-mixed','invalid-clock','submit-failed','grant-failed','token-exhaustion',
           'invalid-cqe','transport-failed','bad-length','bad-flags','bad-reserved','bad-tail',
           'bad-count','overflow-count','unavailable','unknown-label','denied','definite-refusal')
    for mode in modes:
        subprocess.run([str(d/'check'),mode],check=True)
