"""Compile actual Desktop trace append/drain glue with portable trace policy.

Mocks only serial output, the clock, and already-accepted publication fields;
this does not validate IPC acceptance or native scheduling.
"""
from pathlib import Path
import runpy
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root/'userspace/services/desktop/main.adb').read_text()
anchor = source.index('S.publicationInputAfter := Decoded.Value.Input_After;')
start = source.index('                                    if Desktop_Timing_Policy.Enabled then',anchor)
end = source.index('                                    end if;',start)+len('                                    end if;')
append = source[start:end]
start = source.index('      for I in 1 .. ST.Count (sourceTrace) loop')
end = source.index('      ST.Reset (sourceTrace);',start)+len('      ST.Reset (sourceTrace);')
drain = source[start:end]
check = runpy.run_path(str(Path(__file__).with_name('check-source-trace.py')))['check']
prefix = '''with Ada.Text_IO; with Ada.Command_Line; with Interfaces; use Interfaces;
with Compositor_Source_Trace;
procedure Source_Integration is
   package ST renames Compositor_Source_Trace;
   sourceTrace : ST.State;
   package Desktop_Timing_Policy is Enabled : Boolean := False; end;
   Clock : Unsigned_64 := 0;
   Reads : Natural := 0;
   function timingNow return Unsigned_64 is
   begin Reads := Reads+1; return Clock; end;
   function Decimal (V : Unsigned_64) return String is
      T : constant String := V'Image;
   begin return T(T'First+1..T'Last); end;
   LF : constant String := (1 => ASCII.LF);
   procedure debugPrint (T : String) is
   begin Ada.Text_IO.Put(T); end;
   type Surface is record id, publicationInputAfter : Unsigned_64 := 1; end record;
   S : Surface;
   type Payload is record Epoch, Ticket : Unsigned_64 := 1; end record;
   type Decoding is record Value : Payload; end record;
   Decoded : Decoding;
   procedure Accept_Trace is
   begin
'''
suffix = '''
   end Drain_Trace;
begin
   -- Disabled timing neither samples the clock nor appends diagnostics.
   Accept_Trace;
   pragma Assert (Reads=0 and ST.Count(sourceTrace)=0);
   Desktop_Timing_Policy.Enabled := True;
   if Ada.Command_Line.Argument(1) = "valid" then
      for I in 1 .. 200 loop
         Clock := Unsigned_64(I); Decoded.Value.Ticket := Unsigned_64(I);
         S.publicationInputAfter := Unsigned_64(I/2);
         Accept_Trace;
         if I mod 40 = 0 then
            Drain_Trace;
            pragma Assert(ST.Count(sourceTrace)=0 and ST.Lost(sourceTrace)=0);
         end if;
      end loop;
      pragma Assert (Reads=200);
   elsif Ada.Command_Line.Argument(1) = "overflow" then
      for I in 1 .. 65 loop
         Clock := Unsigned_64(I); Decoded.Value.Ticket := Unsigned_64(I);
         Accept_Trace;
      end loop;
      Drain_Trace;
   else
      Clock := Unsigned_64'Last;
      Accept_Trace;
      Drain_Trace;
   end if;
   pragma Assert (ST.Count(sourceTrace)=0 and ST.Lost(sourceTrace)=0 and ST.Invalid(sourceTrace)=0);
end Source_Integration;
'''
with tempfile.TemporaryDirectory(prefix='cubit-source-integration-') as temp:
    out=Path(temp)
    for name in ('compositor_source_trace.ads','compositor_source_trace.adb','compositor_elapsed.ads'):
        (out/name).write_bytes((root/'userspace/lib/compositor'/name).read_bytes())
    (out/'source_integration.adb').write_text(prefix+append+'\n   end Accept_Trace;\n   procedure Drain_Trace is\n   begin\n'+drain+suffix)
    (out/'test.gpr').write_text('''project Test is
      for Source_Dirs use ("."); for Object_Dir use "obj";
      for Exec_Dir use "."; for Main use ("source_integration.adb");
      package Compiler is
        for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato","-O2");
      end Compiler;
    end Test;''')
    subprocess.run(['gprbuild','-q','-p','-P',str(out/'test.gpr')],check=True)
    for mode in ('valid','overflow','invalid'):
        r=subprocess.run([str(out/'source_integration'),mode],capture_output=True,text=True,check=True)
        if mode=='valid':
            result=check(r.stdout)
            assert result['batches']==5 and len(result['records'])==200
            assert result['unknown_input_records']==1
        else:
            assert ('dropped=1' if mode=='overflow' else 'invalid=1') in r.stdout
            try: check(r.stdout)
            except ValueError: pass
            else: raise AssertionError('accepted incomplete actual drain')
    print('SOURCE-INTEGRATION: PASS actual append/drain, disabled clock, 200records/5batches, overflow and unavailable-clock rejection')
