"""Compile the complete metrics publisher against mocked IPC/grant boundaries.

Portable batching/record/protocol sources are real. This is hosted adapter
fault coverage, not a kernel/grant-lifetime proof or native service test.
--adapter-dir permits testing an owner's proposed source without applying it.
"""
import argparse
from pathlib import Path
import shutil
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
runtime = root / 'userspace/runtime/gnat'

messages = '''with Interfaces; use Interfaces;
package CuBit.Messages is
 subtype CapabilitySlot is Unsigned_64 range 0..63;
 type MessageTag is record
  label:Unsigned_32:=0; length,flags:Unsigned_8:=0; reserved:Unsigned_16:=0;
 end record;
 type MessageWords is array(0..3) of Unsigned_64;
 type Message is record
  tag:MessageTag; authorityTag:Unsigned_64:=0; words:MessageWords:=(others=>0);
 end record;
 NULL_MESSAGE:constant Message:=(others=><>);
 COMPLETION_OK:constant Unsigned_64:=0;
 type CompletionEntry is record
  requestId,token:Unsigned_64:=0; msg:Message;
  from,status:Unsigned_64:=0; valid:Boolean:=False;
 end record;
 Submit_OK:Boolean:=True;
 Submissions:Natural:=0;
 function capSubmit(Slot:CapabilitySlot; Msg:Message; Token:Unsigned_64) return Boolean;
 function capCall(Slot:CapabilitySlot; Msg:in out Message) return MessageTag;
end CuBit.Messages;
'''
message_body = '''package body CuBit.Messages is
 function capSubmit(Slot:CapabilitySlot; Msg:Message; Token:Unsigned_64) return Boolean is
 begin Submissions:=Submissions+1; return Submit_OK; end;
 function capCall(Slot:CapabilitySlot; Msg:in out Message) return MessageTag is
 begin return (0,0,0,0); end;
end CuBit.Messages;
'''
grants = '''with Interfaces; use Interfaces;
with System; with CuBit.Messages;
package CuBit.Memory_Grants is
 type Grant_Reference is record slot,generation:Unsigned_64:=1; end record;
 procedure Create_Via_Capability
  (Cap:CuBit.Messages.CapabilitySlot; Address:System.Address; Pages:Natural;
   Writable:Boolean; Ref:out Grant_Reference; Created:out Boolean);
 procedure Revoke(Ref:in out Grant_Reference; Accepted:out Boolean);
 function Retirement_Confirmed(Ref:Grant_Reference) return Boolean;
end CuBit.Memory_Grants;
'''
grant_body = '''package body CuBit.Memory_Grants is
 procedure Create_Via_Capability
  (Cap:CuBit.Messages.CapabilitySlot; Address:System.Address; Pages:Natural;
   Writable:Boolean; Ref:out Grant_Reference; Created:out Boolean) is
 begin Ref:=(1,1); Created:=True; end;
 procedure Revoke(Ref:in out Grant_Reference; Accepted:out Boolean) is
 begin Ref:=(0,0); Accepted:=True; end;
 function Retirement_Confirmed(Ref:Grant_Reference) return Boolean is (Ref.slot=0);
end CuBit.Memory_Grants;
'''
main = '''with Ada.Command_Line; with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metrics; with CuBit.Metric_Records; with CuBit.Metric_Protocol;
procedure Check is
 package M renames CuBit.Metrics;
 package R renames CuBit.Metric_Records;
 package P renames CuBit.Metric_Protocol;
 Item:M.Publisher(10);
 Value:constant R.Metric_Record:=(R.Counter,1,0,1,0);
 Accepted,Submitted,Handled:Boolean;
 C:CompletionEntry;
 Mode:constant String:=Ada.Command_Line.Argument(1);
 procedure Fill_And_Send(Token:Unsigned_64) is
 begin
  for I in 1..R.Maximum_Records loop
   M.Put(Item,Value,Accepted); pragma Assert(Accepted);
  end loop;
  M.Flush(Item,Token,Submitted); pragma Assert(Submitted);
 end;
begin
 C.token:=11; C.valid:=True;
 C.msg.tag:=(P.Status'Enum_Rep(P.OK),P.Message_Words,0,0);
 C.msg.words:=(63,0,0,0);
 if Mode="invalid-completion" or Mode="normal" or Mode="wrong-token" then
  Fill_And_Send(11); Fill_And_Send(12);
  pragma Assert(not M.Has_Room(Item));
  if Mode="invalid-completion" then C.valid:=False;
  elsif Mode="wrong-token" then C.token:=99; end if;
  M.Complete(Item,C,Handled);
  if Mode="normal" then
   pragma Assert(Handled and M.Has_Room(Item) and not M.Disabled(Item));
   M.Put(Item,Value,Accepted); pragma Assert(Accepted);
  elsif Mode="wrong-token" then
   pragma Assert(not Handled and not M.Has_Room(Item) and not M.Disabled(Item));
  else
   pragma Assert(Handled and M.Disabled(Item) and not M.Has_Room(Item));
   M.Put(Item,Value,Accepted); pragma Assert(not Accepted);
   pragma Assert(M.Dropped(Item)=1);
  end if;
 elsif Mode="disabled-room" or Mode="disabled-put" then
  M.Put(Item,Value,Accepted); pragma Assert(Accepted);
  Submit_OK:=False; M.Flush(Item,11,Submitted);
  pragma Assert(not Submitted and M.Disabled(Item) and M.Rejected(Item)=1);
  if Mode="disabled-room" then pragma Assert(not M.Has_Room(Item));
  else
   for I in 1..1000 loop
    M.Put(Item,Value,Accepted); pragma Assert(not Accepted);
   end loop;
   pragma Assert(M.Dropped(Item)=1000);
   M.Flush(Item,12,Submitted); pragma Assert(not Submitted and Submissions=1);
  end if;
 else raise Program_Error; end if;
 Ada.Text_IO.Put_Line("PASS " & Mode);
end Check;
'''
def run(adapter_dir, expect_known_bugs=False):
    with tempfile.TemporaryDirectory(prefix='cubit-metrics-boundary-') as tmp:
        d = Path(tmp)
        for name in ('cubit.ads', 'cubit-metric_records.ads', 'cubit-metric_records.adb',
                     'cubit-metric_batches.ads', 'cubit-metric_batches.adb',
                     'cubit-metric_protocol.ads'):
            shutil.copyfile(runtime / name, d / name)
        for name in ('cubit-metrics.ads', 'cubit-metrics.adb'):
            shutil.copyfile(adapter_dir / name, d / name)
        for name, source in {'cubit-messages.ads': messages, 'cubit-messages.adb': message_body,
                             'cubit-memory_grants.ads': grants, 'cubit-memory_grants.adb': grant_body,
                             'check.adb': main}.items():
            (d / name).write_text(source)
        (d / 'check.gpr').write_text('''project Check is
     for Source_Dirs use ("."); for Object_Dir use "obj";
     for Exec_Dir use "."; for Main use ("check.adb");
     package Compiler is
      for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato");
     end Compiler;
    end Check;
    ''')
        subprocess.run(['gprbuild', '-q', '-p', '-P', str(d/'check.gpr')], check=True)
        for mode in ('normal', 'wrong-token', 'invalid-completion', 'disabled-room', 'disabled-put'):
            r = subprocess.run([str(d/'check'), mode], capture_output=True, text=True)
            known = expect_known_bugs and mode not in ('normal', 'wrong-token')
            if known:
                assert r.returncode != 0 and 'ASSERTION_ERROR' in r.stderr, (mode, r)
                print('REPRODUCED adapter bug:', mode)
            else:
                assert r.returncode == 0, (mode, r.stdout, r.stderr)
                print(r.stdout.strip())

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--adapter-dir', type=Path, default=runtime)
    parser.add_argument('--expect-known-bugs', action='store_true')
    args = parser.parse_args()
    run(args.adapter_dir, args.expect_known_bugs)
