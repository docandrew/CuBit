from pathlib import Path
import tempfile,subprocess,shutil,os
assert os.environ.get('IN_NIX_SHELL')
r=Path(__file__).resolve().parents[2];w=Path(tempfile.mkdtemp(prefix='desktop-logs-host-',dir=r/'tests/compositor/build'));print(w,flush=True)
for folder,names in [('userspace/services/desktop',['desktop_logs']),('userspace/runtime/gnat',['cubit','cubit-log_records','cubit-text_to_log','cubit-protocols']),('userspace/lib/compositor',['compositor_requests'])]:
 for n in names:
  for ext in ['ads','adb']:
   p=r/folder/(n+'.'+ext)
   if p.exists():shutil.copy2(p,w/p.name)
(w/'cubit-messages.ads').write_text('''with Interfaces; package CuBit.Messages is
 type CompletionEntry is record Token : Interfaces.Unsigned_64 := 0; end record;
 procedure debugPrint (Text : String) is null;
end CuBit.Messages;''')
(w/'cubit-logging.ads').write_text('''with Interfaces; with CuBit.Messages; with CuBit.Log_Records;
package CuBit.Logging is
 type Publisher is limited record Busy : Boolean := False; Token : Interfaces.Unsigned_64 := 0; end record;
 Count : Natural := 0; Reject : Boolean := False;
 Last : CuBit.Log_Records.Log_Record;
 procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record; Token : Interfaces.Unsigned_64; Submitted : out Boolean);
 procedure Complete (Item : in out Publisher; Completion : CuBit.Messages.CompletionEntry; Handled : out Boolean);
 function Pending (Item : Publisher) return Boolean is (Item.Busy);
 function Dropped (Item : Publisher) return Interfaces.Unsigned_64 is (0);
end CuBit.Logging;''')
(w/'cubit-logging.adb').write_text('''package body CuBit.Logging is
 use type Interfaces.Unsigned_64;
 procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record; Token : Interfaces.Unsigned_64; Submitted : out Boolean) is
 begin pragma Assert (not Item.Busy); Count := Count+1; Last := Value; Submitted := not Reject; Item.Busy := Submitted; Item.Token := Token; end;
 procedure Complete (Item : in out Publisher; Completion : CuBit.Messages.CompletionEntry; Handled : out Boolean) is
 begin Handled := Item.Busy and Completion.Token=Item.Token; if Handled then Item.Busy:=False; end if; end;
end CuBit.Logging;''')
(w/'check.adb').write_text('''with Desktop_Logs; with CuBit.Logging; with CuBit.Log_Records; with Interfaces; with Ada.Text_IO;
procedure Check is
 package D renames Desktop_Logs; package L renames CuBit.Logging;
 use type Interfaces.Unsigned_64;
 Seq : Interfaces.Unsigned_64 := 100;
 procedure Done is begin D.Collect ((Token=>Seq)); end;
begin
 D.Write ("split"); D.Pump (Seq); pragma Assert (L.Count=0);
 D.Write (" line" & ASCII.CR); D.Write (ASCII.LF & "second" & ASCII.LF);
 D.Pump (Seq); pragma Assert (Seq=101 and L.Count=1 and D.Matches(101));
 pragma Assert (CuBit.Log_Records.Text(L.Last)="split line");
 D.Pump (Seq); pragma Assert (L.Count=1);
 D.Collect ((Token=>999)); D.Pump (Seq); pragma Assert (L.Count=1);
 Done; D.Pump(Seq); pragma Assert (CuBit.Log_Records.Text(L.Last)="second"); Done;
 for I in 1..40 loop D.Write("burst" & ASCII.LF); end loop;
 for I in 1..32 loop D.Pump(Seq); pragma Assert(CuBit.Log_Records.Text(L.Last)="burst"); Done; end loop;
 D.Pump(Seq); pragma Assert(CuBit.Log_Records.Text(L.Last)="desktop: log records dropped= 8 publisher_dropped= 0"); Done;
 D.Write ((1..513=>'x') & ASCII.LF); D.Pump(Seq);
 pragma Assert(CuBit.Log_Records.Text(L.Last)="desktop: log records dropped= 9 publisher_dropped= 0"); Done;
 L.Reject:=True; D.Write("unavailable" & ASCII.LF); D.Pump(Seq);
 declare N : constant Natural:=L.Count; begin
 for I in 1..1000 loop D.Write("ignored" & ASCII.LF); D.Pump(Seq); end loop;
 pragma Assert(L.Count=N and not D.Matches(Seq)); end;
 Ada.Text_IO.Put_Line("PASS framing, busy retention, foreign completion, overflow, invalid line, unavailable collector");
end Check;''')
(w/'test.gpr').write_text('project Test is for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("check.adb"); package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato"); end Compiler; end Test;')
subprocess.run(['alr','exec','--','gprbuild','-q','-p','-P',str(w/'test.gpr')],cwd=r/'kernel',check=True)
subprocess.run([str(w/'check')],check=True)
