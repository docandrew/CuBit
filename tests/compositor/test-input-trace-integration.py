"""Actual Desktop dequeue trace append/drain with real bounded trace storage."""
from pathlib import Path
import subprocess,tempfile,runpy
root=Path(__file__).resolve().parents[2]
s=(root/'userspace/services/desktop/main.adb').read_text()
a=s.index('      if found and then Desktop_Timing_Policy.Enabled then',s.index('   end enqueueInput;'))
b=s.index('      end if;',a)+len('      end if;')
append=s[a:b]
a=s.index('      for I in 1 .. IT.Count (inputDequeueTrace) loop')
b=s.index('      IT.Reset (inputDequeueTrace);',a)+len('      IT.Reset (inputDequeueTrace);')
drain=s[a:b]
fields=runpy.run_path(str(Path(__file__).with_name('check-source-trace.py')))['fields']
code='''with Ada.Text_IO; with Interfaces; use Interfaces;
with Compositor_Input_Trace;
procedure Check is
   package IT renames Compositor_Input_Trace;
   inputDequeueTrace : IT.State;
   package Desktop_Timing_Policy is Enabled : Boolean := False; end;
   found : Boolean := True;
   type Event_Record is record target,serial,kind : Unsigned_64 := 1; end record;
   event : Event_Record;
   Clock : Unsigned_64 := 1;
   Reads : Natural := 0;
   function timingNow return Unsigned_64 is
   begin Reads:=Reads+1; return Clock; end;
   function Decimal(V:Unsigned_64) return String is
      T:constant String:=V'Image;
   begin return T(T'First+1..T'Last); end;
   LF : constant Character := ASCII.LF;
   procedure debugPrint(T:String) is
   begin Ada.Text_IO.Put(T); end;
   procedure Append is
   begin
'''+append+'''
   end Append;
   procedure Drain is
   begin
'''+drain+'''
   end Drain;
begin
   Append; Drain;
   pragma Assert(Reads=0 and IT.Count(inputDequeueTrace)=0);
   Desktop_Timing_Policy.Enabled:=True;
   for I in 1 .. 200 loop
      event.serial:=Unsigned_64(I); Clock:=Unsigned_64(I);
      Append;
      if I mod 40=0 then Drain; end if;
   end loop;
   pragma Assert(Reads=200 and IT.Count(inputDequeueTrace)=0);
   found:=False; Append; pragma Assert(Reads=200);
end Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-input-trace-glue-') as tmp:
    d=Path(tmp);(d/'check.adb').write_text(code)
    (d/'check.gpr').write_text(f'''project Check is
 for Source_Dirs use (".","{root}/userspace/lib/compositor");
 for Source_Files use ("check.adb","compositor_input_trace.ads","compositor_input_trace.adb","compositor_elapsed.ads");
 for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("check.adb");
 package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato"); end Compiler;
end Check;''')
    subprocess.run(['gprbuild','-q','-p','-P',str(d/'check.gpr')],check=True)
    result=subprocess.run([str(d/'check')],check=True,capture_output=True,text=True)
    rows=[];batches=0
    for line in result.stdout.splitlines():
        if not line.strip(): continue
        if 'COMPOSITOR-INPUT:' in line:
            rows.append(fields(line,'COMPOSITOR-INPUT:',{'surface','serial','kind','dequeued_us'}))
        else:
            assert fields(line,'COMPOSITOR-INPUT-STATS:',{'count','invalid','dropped'})=={'count':40,'invalid':0,'dropped':0}
            batches+=1
    assert batches==5 and rows==[dict(surface=1,serial=i,kind=1,dequeued_us=i) for i in range(1,201)]
    print('INPUT-TRACE-GLUE: PASS 200 exact records across 5 batches, disabled/empty no clock reads')
