"""Run procmgr's actual REQ_SERVICE block with fake lookup/mint/audit syscalls.
Real authority and metric/log tag policies; no kernel authentication claim.
Run inside nix develop. Temporary build outputs do not touch native staging.
"""
from pathlib import Path
import shutil
import subprocess
import tempfile
import sys

root = Path(__file__).resolve().parents[2]
runtime = root / 'userspace/runtime/gnat'
source = (root / 'userspace/services/procmgr/main.adb').read_text()
block = source.split('                           when REQ_SERVICE =>', 1)[1].split('                           when REQ_NOTIFICATION =>', 1)[0]
assert 'Next_Metric_Issuance' in block, 'issuance patch missing'
registration = source.split('      --  Metrics registration authority belongs', 1)[1].split('      --  A recycled PID', 1)[0]
registration = registration[registration.index('      if systemStartup'): ]
prefix = '''with Ada.Text_IO; with Interfaces; use Interfaces;
with CuBit.Authority_Policy; with CuBit.Metric_Protocol;
with CuBit.Log_Protocol; with CuBit.Audio_Control; with CuBit.Clock_Control;
procedure Check is
   package P renames CuBit.Metric_Protocol;
   Next_Log_Issuance, Next_Metric_Issuance : Unsigned_32 := 1;
   param0 : Unsigned_32 := 24;
   systemStartup, approveLogViewer : Boolean := False;
   childPID, rightsMask, slotNum : Unsigned_64 := 42;
   ignore, Last_Tag, Last_Role, Last_Reason : Unsigned_64 := 0;
   Mints, Audits, Lookups : Natural := 0;
   Present : Boolean := True;
   DRIVER_LOGSTORE : constant Unsigned_64 := 13;
   DRIVER_MIXER : constant Unsigned_64 := 9;
   DRIVER_CLOCK : constant Unsigned_64 := 19;
   SYSINFO_REGISTERED_DRIVER, SYSCALL_SLEEP, CAP_TYPE_ENDPOINT,
   AUTH_SOURCE_MANIFEST, AUTH_REASON_MANIFEST_REQUEST : constant Unsigned_64 := 1;
   AUTH_REASON_STARTUP_REQUIRED : constant Unsigned_64 := 2;
   AUTH_REASON_MINT_FAILED : constant Unsigned_64 := 3;
   AUTH_REASON_SERVICE_MISSING : constant Unsigned_64 := 4;
   LF : constant Character := ASCII.LF;
   procedure debugPrint (S : String) is null;
   function syscall (Op, Arg : Unsigned_64) return Unsigned_64 is (0);
   function getInfo (Op, Role : Unsigned_64) return Unsigned_64 is
   begin
      Lookups := Lookups + 1; Last_Role := Role;
      return (if Present then 77 else 0);
   end;
   procedure recordAuthority
     (PID, Slot, Source, Reason, Kind : Unsigned_64; Requested, Approved : Boolean;
      Rights, Ref, Tag : Unsigned_64) is
   begin
      Audits := Audits + 1; Last_Reason := Reason;
      pragma Assert (Requested and not Approved);
   end;
   procedure mintRecorded
     (PID, Kind, Ref, Tag, Rights, Slot, Source, Reason : Unsigned_64;
      Requested : Boolean; Result : out Unsigned_64) is
   begin
      pragma Assert (Ref=77 and PID=childPID and Rights=rightsMask and Slot=slotNum);
      pragma Assert (Requested);
      Mints:=Mints+1; Last_Tag:=Tag; Result:=0;
   end;
   procedure Register_Service (Trusted : Boolean; Identity : String) is
      systemStartup : constant Boolean := Trusted;
      pkgId : constant String := Identity;
      pkgIdLen : constant Natural := Identity'Length;
      newPID : constant Unsigned_64 := 99;
      CAP_TYPE_NOTIFICATION, CAP_SLOT_SERVICE_REG,
      AUTH_SOURCE_IDENTITY_POLICY, AUTH_REASON_PACKAGE_ID : constant Unsigned_64 := 2;
      procedure mintRecorded
        (PID, Kind, Ref, Tag, Rights, Slot, Source, Reason : Unsigned_64;
         Requested : Boolean; Result : out Unsigned_64) is
      begin
         pragma Assert(PID=newPID and Kind=CAP_TYPE_NOTIFICATION);
         pragma Assert(Ref=P.Publisher_Service_Role and Tag=0 and Rights=2);
         pragma Assert(Slot=CAP_SLOT_SERVICE_REG and not Requested);
         Mints:=Mints+1; Result:=0;
      end;
   begin
      Mints:=0;
''' + registration + '''   end Register_Service;
   procedure Issue (Role : Unsigned_64; Startup : Boolean; Viewer : Boolean := False) is
   begin
      param0:=Unsigned_32(Role); systemStartup:=Startup; approveLogViewer:=Viewer;
      Mints:=0; Audits:=0; Lookups:=0; Last_Tag:=0; Last_Role:=0; Last_Reason:=0;
'''
suffix = '''   end Issue;
begin
   for Trusted in Boolean loop
      Register_Service(Trusted,"com.cubit.metrics");
      pragma Assert(Mints=(if Trusted then 1 else 0));
      Register_Service(Trusted,"com.cubit.metricx");
      pragma Assert(Mints=0);
      Register_Service(Trusted,"");
      pragma Assert(Mints=0);
      Register_Service(Trusted,"com.cubit.metrics.extra");
      pragma Assert(Mints=0);
   end loop;
   for Startup in Boolean loop
      Issue (P.Publisher_Service_Role, Startup);
      pragma Assert (Mints=1 and Audits=0 and P.Is_Publisher(Last_Tag));
      pragma Assert (Last_Role=P.Publisher_Service_Role);
      pragma Assert (not P.May_Invoke(Last_Tag,P.Query_Summaries));
      for Viewer in Boolean loop
         Issue (P.Observer_Service_Role, Startup, Viewer);
         if Startup then
            pragma Assert (Mints=1 and Audits=0 and P.Is_Observer(Last_Tag));
            pragma Assert (Last_Role=P.Publisher_Service_Role);
            pragma Assert (not P.May_Invoke(Last_Tag,P.Publish_Batch));
         else
            pragma Assert (Mints=0 and Audits=1 and Lookups=0);
            pragma Assert (Last_Reason=AUTH_REASON_STARTUP_REQUIRED);
         end if;
      end loop;
   end loop;
   for N in 1 .. 1000 loop
      Next_Metric_Issuance:=Unsigned_32(N);
      Issue(P.Publisher_Service_Role,False);
      pragma Assert(Last_Tag=P.Publisher_Tag(Unsigned_64(N)));
      Issue(P.Observer_Service_Role,True);
      pragma Assert(Last_Tag=P.Observer_Tag(Unsigned_64(N+1)));
   end loop;
   for Role in P.Publisher_Service_Role .. P.Observer_Service_Role loop
      Next_Metric_Issuance:=Unsigned_32'Last;
      Issue(Role,True);
      pragma Assert(Mints=1 and Next_Metric_Issuance=0);
      pragma Assert((Last_Tag and P.Issuance_Mask)=P.Issuance_Mask);
      Issue(Role,True);
      pragma Assert(Mints=0 and Audits=1 and Last_Reason=AUTH_REASON_MINT_FAILED);
      pragma Assert(Next_Metric_Issuance=0);
   end loop;
   Next_Metric_Issuance:=1; Present:=False;
   Issue(P.Publisher_Service_Role,False);
   pragma Assert(Mints=0 and Audits=1 and Lookups=20);
   pragma Assert(Last_Reason=AUTH_REASON_SERVICE_MISSING);
   Present:=True;
   Issue(P.Publisher_Service_Role,False);
   pragma Assert(Last_Tag=P.Publisher_Tag(2)); -- missing-service attempt not reused
   Issue(CuBit.Log_Protocol.Observer_Service_Role,False,True);
   pragma Assert(Mints=1 and Last_Role=DRIVER_LOGSTORE);
   Issue(CuBit.Audio_Control.Service_Role,False,True);
   pragma Assert(Mints=0 and Last_Reason=AUTH_REASON_STARTUP_REQUIRED);
   Issue(CuBit.Clock_Control.Service_Role,False,True);
   pragma Assert(Mints=0 and Last_Reason=AUTH_REASON_STARTUP_REQUIRED);
   Ada.Text_IO.Put_Line("PASS actual procmgr metrics issuance: approval, routing, tags, exhaustion, missing service and viewer isolation");
end Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-procmgr-metrics-') as tmp:
    d = Path(tmp)
    for unit in ['cubit.ads','cubit-authority_policy.ads','cubit-metric_protocol.ads',
                 'cubit-metric_records.ads','cubit-metric_records.adb',
                 'cubit-log_protocol.ads','cubit-log_records.ads','cubit-log_records.adb','cubit-protocols.ads']:
        shutil.copy2(runtime/unit,d/unit)
    for name in ['audio_control','clock_control']:
        text=(runtime/f'cubit-{name}.ads').read_text()
        constants='\n'.join(line for line in text.splitlines() if 'Service_Role :' in line or 'Authority_Tag :' in line)
        (d/f'cubit-{name}.ads').write_text(f'with Interfaces; use Interfaces; package CuBit.{name} is\n{constants}\nend CuBit.{name};\n')
    (d/'check.adb').write_text(prefix+block+suffix)
    (d/'check.gpr').write_text('''project Check is
 for Source_Dirs use ("."); for Object_Dir use "obj";
 for Exec_Dir use "."; for Main use ("check.adb");
 package Compiler is for Default_Switches ("Ada") use
 ("-gnat2022", "-gnata", "-gnato", "-O0"); end Compiler;
end Check;''')
    subprocess.run(['gprbuild','-q','-p','-P',str(d/'check.gpr')],check=True)
    subprocess.run([str(d/'check')],check=True)
    if '--prove' in sys.argv:
        for unit in ['issuance_proof.ads', 'issuance_proof.adb']:
            shutil.copy2(Path(__file__).parent / unit, d / unit)
        subprocess.run(['gnatprove', '-P', str(d/'check.gpr'), '-u',
                        'issuance_proof.adb', '--level=2', '--report=all',
                        '--checks-as-errors=on', '-j1'], check=True)
        print((d/'obj/gnatprove/gnatprove.out').read_text())
    mutants = {
        'ordinary observer approval': block.replace('or isMetricObserver)', ')', 1),
        'observer routes to wrong service': block.replace(
            'elsif isMetricObserver then\n                                          CuBit.Metric_Protocol.Publisher_Service_Role',
            'elsif isMetricObserver then\n                                          CuBit.Metric_Protocol.Observer_Service_Role'),
        'exhaustion wraps to one': block.replace('then 0 else Next_Metric_Issuance + 1',
                                                'then 1 else Next_Metric_Issuance + 1'),
    }
    for name, mutant in mutants.items():
        assert mutant != block, name
        (d/'check.adb').write_text(prefix+mutant+suffix)
        subprocess.run(['gprbuild','-q','-f','-p','-P',str(d/'check.gpr')],check=True)
        result=subprocess.run([str(d/'check')],capture_output=True,text=True)
        assert result.returncode != 0 and 'ADA.ASSERTIONS.ASSERTION_ERROR' in result.stderr, (name,result)
        print('PASS negative control rejected: '+name)
