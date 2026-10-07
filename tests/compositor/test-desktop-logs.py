from pathlib import Path
import tempfile,subprocess,shutil,os
assert os.environ.get('IN_NIX_SHELL')
r=Path(__file__).resolve().parents[2];w=Path(tempfile.mkdtemp(prefix='desktop-logs-host-',dir=r/'tests/compositor/build'));print(w,flush=True)
for folder,names in [('userspace/services/desktop',['desktop_logs']),('userspace/runtime/gnat',['cubit','cubit-log_records','cubit-text_to_log','cubit-protocols']),('userspace/lib/compositor',['compositor_requests'])]:
 for n in names:
  for ext in ['ads','adb']:
   p=r/folder/(n+'.'+ext)
   if p.exists():shutil.copy2(p,w/p.name)
(w/'cubit-messages.ads').write_text('''package CuBit.Messages is
 Calls, Bytes : Natural := 0;
 procedure debugPrint (Text : String);
end CuBit.Messages;''')
(w/'cubit-messages.adb').write_text('''package body CuBit.Messages is
 procedure debugPrint (Text : String) is
 begin Calls := Calls + 1; Bytes := Bytes + Text'Length; end;
end CuBit.Messages;''')
# Transport fixture models immediate acceptance/shedding, not the ring itself.
# The real Desktop adapter and text framer are compiled unchanged.
(w/'cubit-logging.ads').write_text('''with CuBit.Log_Records;
package CuBit.Logging is
 type Publisher is limited null record;
 Count, Accepted_Count : Natural := 0; Reject : Boolean := False;
 History : array (1..4096) of CuBit.Log_Records.Log_Record;
 procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
                 Submitted : out Boolean);
end CuBit.Logging;''')
(w/'cubit-logging.adb').write_text('''package body CuBit.Logging is
 procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
                 Submitted : out Boolean) is
 begin
   Count := Count + 1; History (Count) := Value;
   Submitted := not Reject;
   if Submitted then Accepted_Count := Accepted_Count + 1; end if;
 end;
end CuBit.Logging;''')
(w/'check.adb').write_text('''with Desktop_Logs; with CuBit.Logging;
with CuBit.Log_Records; with CuBit.Messages; with Ada.Text_IO;
procedure Check is
 package D renames Desktop_Logs; package L renames CuBit.Logging;
 package R renames CuBit.Log_Records;
 procedure Expect (Index : Positive; Text : String) is
 begin pragma Assert (R.Text (L.History (Index)) = Text); end;
begin
 D.Write ("split"); pragma Assert (L.Count=0);
 D.Write (" line" & ASCII.CR); pragma Assert (L.Count=0);
 D.Write (ASCII.LF & "second" & ASCII.LF);
 pragma Assert (L.Count=2 and L.Accepted_Count=2);
 Expect (1,"split line"); Expect (2,"second");
 pragma Assert (CuBit.Messages.Calls=3 and CuBit.Messages.Bytes=19);
 -- No adapter-owned backlog or completion handshake: every line is offered.
 for I in 1..40 loop D.Write("burst" & ASCII.LF); Expect(I+2,"burst"); end loop;
 pragma Assert (L.Count=42 and L.Accepted_Count=42);
 D.Write ((1..513=>'x') & ASCII.LF);
 pragma Assert (L.Count=43); Expect(43,"desktop: log records dropped= 1");
 D.Write ("bad" & Character'Val(0) & ASCII.LF);
 pragma Assert (L.Count=44); Expect(44,"desktop: log records dropped= 2");
 D.Write ("recovered" & ASCII.LF); Expect(45,"recovered");
 -- One data attempt and at most one loss-report attempt per Write, even
 -- when the publisher sheds both. No retry loop or retained CQ ownership.
 L.Reject := True;
 for I in 1..1000 loop
   declare Before : constant Natural := L.Count; begin
     D.Write ("unavailable" & ASCII.LF);
     pragma Assert (L.Count=Before+2 and L.Accepted_Count=45);
   end;
 end loop;
 L.Reject := False;
 D.Write ("available" & ASCII.LF);
 Expect(2046,"available"); Expect(2047,"desktop: log records dropped= 2002");
 pragma Assert (L.Count=2047 and L.Accepted_Count=47);
 D.Write ("final" & ASCII.LF);
 pragma Assert (L.Count=2048); Expect(2048,"final");
 Ada.Text_IO.Put_Line("PASS framing, serial echo, direct publication, invalid lines, bounded shedding, recovery");
end Check;''')
(w/'test.gpr').write_text('project Test is for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("check.adb"); package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato"); end Compiler; end Test;')
subprocess.run(['alr','exec','--','gprbuild','-q','-p','-P',str(w/'test.gpr')],cwd=r/'kernel',check=True)
subprocess.run([str(w/'check')],check=True)
