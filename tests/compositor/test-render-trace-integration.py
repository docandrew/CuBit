"""Actual Desktop render identity, successful-submit append, and drain glue.
Real pool/trace policies; mocked rasterizer addresses, clock, and IPC outcome.
"""
from pathlib import Path
import runpy,subprocess,tempfile
root=Path(__file__).resolve().parents[2]
s=(root/'userspace/services/desktop/main.adb').read_text()
a=s.index('   procedure noteClientDraw ');b=s.index('   end noteClientDraw;',a)+len('   end noteClientDraw;')
helper=s[a:b]
a=s.index('      if capSubmit (CAP_SLOT_DISPLAY, request, CP.Token (P.Transfer)) then')
a=s.index('         if Desktop_Timing_Policy.Enabled then',a);b=s.index('         end if;',a)+len('         end if;')
submit=s[a:b]
a=s.index('      for I in 1 .. RT.Count (renderTrace) loop');b=s.index('      RT.Reset (renderTrace);',a)+len('      RT.Reset (renderTrace);')
drain=s[a:b]
code='''with Ada.Text_IO; with Interfaces; use Interfaces;
with Compositor_Render_Trace; with Compositor_Pool; with Compositor_Surface_State;
procedure Check is
 package RT renames Compositor_Render_Trace;
 package BP renames Compositor_Pool;
 package Surface_Policy is new Compositor_Surface_State;
 package Desktop_Timing_Policy is Enabled : Boolean := False; end;
 package CP is
   type State is record Sess,Fr:Unsigned_64:=0; end record;
   function Session(S:State) return Unsigned_64 is (S.Sess);
   function Token(S:State) return Unsigned_64 is (S.Fr);
 end CP;
 subtype Output_Index is Natural range 0..1;
 type Output_Presentation is record
   Buffer:Natural:=100; Pool:BP.State; Transfer:CP.State:=(7,90);
   Started_Us:Unsigned_64:=10;
 end record;
 presentations:array(Output_Index) of Output_Presentation;
 primaryOutput:Output_Index:=0; activeOutput:Output_Index:=1;
 nativeOutputPass:Boolean:=False; directOutput:Boolean:=True;
 backBufferAddr:Natural:=100;
 type Attached is record Address:Natural:=1000; Acquired:Boolean:=True; end record;
 type Attachments is array(Surface_Policy.Slot) of Attached;
 type Surface is record
   id:Unsigned_64:=42; publicationMode:Boolean:=True; bufferAddr:Natural:=1000;
   publicationPolicy:Surface_Policy.State; publicationBuffers:Attachments;
 end record;
 S:Surface;
 renderTrace:RT.State;
 Reads:Natural:=0;
 function timingNow return Unsigned_64 is
 begin Reads:=Reads+1; return 9; end;
 function Decimal(V:Unsigned_64) return String is
   T:constant String:=V'Image;
 begin return T(T'First+1..T'Last); end;
 LF:constant Character:=ASCII.LF;
 procedure debugPrint(T:String) is
 begin Ada.Text_IO.Put(T); end;
'''+helper+'''
 procedure Submitted(Accepted:Boolean) is
   Output:constant Output_Index:=0;
   P:Output_Presentation renames presentations(Output);
   Held:constant BP.Ticket:=BP.Writer(P.Pool);
 begin
   if Accepted then
'''+submit+'''
   end if;
 end Submitted;
 procedure Drain is
 begin
'''+drain+'''
 end Drain;
 T:BP.Ticket;
begin
 for O in Output_Index loop
   presentations(O).Pool:=BP.Open(Unsigned_64(7+O));
   BP.Acquire(presentations(O).Pool,T);
 end loop;
 S.publicationPolicy.Requested:=1; S.publicationPolicy.Issued:=4;
 S.publicationPolicy.Buffers(1):=(Surface_Policy.Visible,1,4);
 noteClientDraw(S); Submitted(True);
 pragma Assert(Reads=0 and RT.Count(renderTrace)=0);
 Desktop_Timing_Policy.Enabled:=True;
 noteClientDraw(S);
 pragma Assert(RT.Item(renderTrace,1).Output_ID=0 and RT.Item(renderTrace,1).Writer_Epoch=7);
 nativeOutputPass:=True; noteClientDraw(S);
 pragma Assert(RT.Item(renderTrace,2).Output_ID=1 and RT.Item(renderTrace,2).Writer_Epoch=8);
 nativeOutputPass:=False; directOutput:=False; noteClientDraw(S);
 pragma Assert(RT.Unsupported(renderTrace)=1 and Reads=2);
 directOutput:=True; S.bufferAddr:=999; noteClientDraw(S);
 pragma Assert(RT.Invalid(renderTrace)=1 and Reads=2);
 S.bufferAddr:=1000; S.publicationBuffers(1).Acquired:=False;
 noteClientDraw(S);
 pragma Assert(RT.Invalid(renderTrace)=2 and Reads=2);
 S.publicationBuffers(1).Acquired:=True;
 S.publicationMode:=False; noteClientDraw(S);
 pragma Assert(RT.Unsupported(renderTrace)=2 and Reads=2);
 S.publicationMode:=True;
 presentations(0).Pool:=BP.Open(7); noteClientDraw(S);
 pragma Assert(RT.Invalid(renderTrace)=3 and Reads=2);
 BP.Acquire(presentations(0).Pool,T);
 RT.Reset(renderTrace); noteClientDraw(S); Submitted(False);
 pragma Assert(RT.Count(renderTrace)=1);
 Submitted(True); pragma Assert(RT.Count(renderTrace)=2);
 Drain;
 pragma Assert(RT.Count(renderTrace)=0 and RT.Unsupported(renderTrace)=0);
end Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-render-trace-glue-') as tmp:
 d=Path(tmp);(d/'check.adb').write_text(code)
 (d/'check.gpr').write_text(f'''project Check is
 for Source_Dirs use (".","{root}/userspace/lib/compositor");
 for Source_Files use ("check.adb","compositor_render_trace.ads","compositor_render_trace.adb",
 "compositor_pool.ads","compositor_pool.adb","compositor_surface_state.ads","compositor_surface_state.adb","compositor_elapsed.ads");
 for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("check.adb");
 package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato"); end Compiler;
end Check;''')
 subprocess.run(['gprbuild','-q','-p','-P',str(d/'check.gpr')],check=True)
 r=subprocess.run([str(d/'check')],check=True,capture_output=True,text=True)
 fields=runpy.run_path(str(Path(__file__).with_name('check-source-trace.py')))['fields']
 rows=[line for line in r.stdout.splitlines() if line.startswith('COMPOSITOR-RENDER:')]
 assert len(rows)==2 and 'kind=1 output=0 buffer=1 writer_epoch=7 writer_serial=1 surface=42 source_epoch=1 source_ticket=4 session=0 frame=0 observed_us=9' in rows[0]
 assert 'kind=2 output=0 buffer=1 writer_epoch=7 writer_serial=1 surface=0 source_epoch=0 source_ticket=0 session=7 frame=90 observed_us=10' in rows[1]
 assert 'COMPOSITOR-RENDER-STATS: count=2 invalid=0 dropped=0 unsupported=0' in r.stdout
 print('RENDER-TRACE-GLUE: PASS actual source/writer selection, disabled clock, identity failures, unsupported paths, successful submission and drain')
