from pathlib import Path
import re,json,hashlib,sys
w=Path(sys.argv[1]);source_root=Path(sys.argv[2]);s=(source_root/'userspace/services/procmgr/main.adb').read_text();fragments={}
def span(label,start,end):
 a=s.index(start);b=s.index(end,a)+len(end);text=s[a:b];fragments[label]=dict(sha256=hashlib.sha256(text.encode()).hexdigest(),start_line=s[:a].count('\n')+1);return text
constants=span('constants','   MANIFEST_MAGIC   :','   REQ_NOTIFICATION : constant Unsigned_8 := 7;')+'\n   SHT_PROGBITS : constant Unsigned_32 := 1;\n'
readers=span('readers','   function readU16','   end readU8;');finder=span('finder','   procedure findSection','   end findSection;');manifest=span('manifest','   type Manifest_Reading','   end readManifest;');branch=span('resource_branch','         if View.Request_Of (Index) = 0 then','         R := View.Caps (View.Request_Of (Index));').split('         R :=')[0]
units=sorted(set(re.findall(r'CuBit\.([A-Za-z_]+)\.',manifest))|{'Startup_Tables'});withs='\n'.join('with CuBit.'+x+';'for x in units)
header='''pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with CCL.Resource_Sections;
with CCL.Device_Sections;
with ACPI_CMOS_Manifest_Windows;
with ACPI_CMOS_Handover_Protocol;
'''+withs+'''
procedure Procmgr_Manifest_Path_Tests is
   package ST renames CuBit.Startup_Tables;
   use type ST.Capability_Kind;
   package CMOS_Windows renames ACPI_CMOS_Manifest_Windows;
   package W renames ACPI_CMOS_Handover_Protocol;
   use type CMOS_Windows.Selection_Status;
   package IO renames Ada.Streams.Stream_IO;
   use type Ada.Streams.Stream_Element_Offset;
   function Read return Ada.Streams.Stream_Element_Array is
      F : IO.File_Type;
   begin
      IO.Open(F,IO.In_File,Ada.Command_Line.Argument(1));
      declare Data : Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(IO.Size(F)));
         Last : Ada.Streams.Stream_Element_Offset;
      begin IO.Read(F,Data,Last);IO.Close(F);if Last/=Data'Last then raise Program_Error;end if;return Data;end;
   end Read;
   Image : aliased Ada.Streams.Stream_Element_Array := Read;
   imageBuf : constant System.Address := Image(Image'First)'Address;
'''
tail='''
   package SB is type Mint_Outcome is (Granted,Refused_By_Policy,Not_Available);end SB;
   use type SB.Mint_Outcome;
   View : Manifest_View; Reading : Manifest_Reading;
   CMOS_Window : CMOS_Windows.Selection;
   Calls : Natural := 0;
   procedure Mint_ACPI_Resource_Window(Index : ST.Entry_Index;Handle : out Unsigned_64;Outcome : out SB.Mint_Outcome)is
   begin
      if Index/=5 then raise Program_Error with "wrong typed window index";end if;
      Calls:=Calls+1;Handle:=77;Outcome:=SB.Granted;
   end Mint_ACPI_Resource_Window;
   procedure Dispatch(Index:ST.Entry_Index;Handle:out Unsigned_64;Outcome:out SB.Mint_Outcome)is
      Item : constant ST.Startup_Entry := View.Declared.Entries(Index);
   begin
      Handle:=0;Outcome:=SB.Refused_By_Policy;
@BRANCH@
   end Dispatch;
   Handle : Unsigned_64; Outcome : SB.Mint_Outcome;
   procedure Check(OK:Boolean)is begin if not OK then raise Program_Error with "actual ELF/manifest resource route";end if;end Check;
begin
   readManifest(Unsigned_64(Image'Length),View,Reading);
   if Ada.Command_Line.Argument(2)="bad-startup" then Check(Reading=Startup_Table_Invalid);
   elsif Ada.Command_Line.Argument(2)="bad-elf" then Check(Reading=Section_Headers_Outside_Image);
   else
      Check(Reading=Manifest_Read);Check(View.Request_Of(5)=0);
      CMOS_Window:=CMOS_Windows.Select_Window(View.CMOS_Declaration,True,True,True,(W.Legacy_64,False));
      Dispatch(5,Handle,Outcome);
      if Ada.Command_Line.Argument(2)="allow" then
         Check(Outcome=SB.Granted and Handle=77 and Calls=1);
         CMOS_Window:=CMOS_Windows.Select_Window(View.CMOS_Declaration,False,True,True,(W.Legacy_64,False));
         Calls:=0;Dispatch(5,Handle,Outcome);Check(Outcome=SB.Refused_By_Policy and Handle=0 and Calls=0);
      else Check(Outcome=SB.Refused_By_Policy and Handle=0 and Calls=0);end if;
   end if;
   Ada.Text_IO.Put_Line("ACTUAL_ELF_RESOURCE_ROUTE PASS " & Ada.Command_Line.Argument(2));
end Procmgr_Manifest_Path_Tests;
'''
original='''         if View.Request_Of (Index) = 0 then
            Outcome := (if Item.Kind = ST.DMA then SB.Not_Available else SB.Refused_By_Policy);
            return;
         end if;
'''
for name,body in [('candidate',branch),('old-zero-map-mutant',original)]:
 d=w/'elf-path'/name;d.mkdir(parents=True,exist_ok=True);(d/'procmgr_manifest_path_tests.adb').write_text(header+constants+readers+finder+manifest+tail.replace('@BRANCH@',body));g=(w/'manifest-admission/manifest_admission.gpr').read_text();g=g.replace('project Manifest_Admission is','project ELF_Path is').replace('end Manifest_Admission;','end ELF_Path;').replace('for Source_Dirs use (','for Source_Dirs use ("'+str(d)+'",').replace('for Source_Files use (','for Source_Files use ("procmgr_manifest_path_tests.adb",').replace('for Main use ("manifest_sections.adb")','for Main use ("procmgr_manifest_path_tests.adb")').replace('"-gnatwa","-gnatwe",','');(d/'elf_path.gpr').write_text(g)
(w/'ELF-EXTRACTED-FRAGMENTS.json').write_text(json.dumps(dict(source='userspace/services/procmgr/main.adb',source_sha256=hashlib.sha256(s.encode()).hexdigest(),fragments=fragments,limitation='Actual findSection/readManifest/zero-map dispatch branch; called capability minter is modeled, not actual kernel/cap installation. Old branch mutant must compile and fail positive assertion.'),indent=2)+'\n')
# Build genuine ELF with actual CCL compiler section bytes; no copied binaries.
out=w/'manifest-output1';asm=w/'elf-path/manifest.S';asm.write_text(''.join('.section .cubit.'+name+',"a"\n.incbin "'+str(out/file)+'"\n'for name,file in [('caps','caps.bin'),('resources','resources.bin'),('devices','devices.bin'),('startup','startup.bin')])+'.section .note.GNU-stack,"",@progbits\n')
print('Generated actual-source ELF route adapter and old-zero-map mutant')
