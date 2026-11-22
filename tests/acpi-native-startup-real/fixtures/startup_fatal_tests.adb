pragma Ada_2022;
with Ada.Command_Line;with Ada.Exceptions;with Ada.Text_IO;with Ada.Streams.Stream_IO;
with Interfaces;with AML_Decode;with ACPI_Hosted_Heap;with ACPI_Native_Instance;
with ACPI_One_Service;with ACPI_Native_Delays;with ACPI_Snapshot_Startup;
with ACPI_Fatal_Diagnostics;with Startup_Test_Firmware;with Owner_Test_Control;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Startup_Fatal_Tests is
   package T renames Owner_Test_Control;package One renames ACPI_One_Service;
   package L renames ACPI_Snapshot_Startup;package D renames ACPI_Fatal_Diagnostics;
   use type T.Scenario,T.Level,L.Outcome,L.R.Tick,One.Runtime.Status.Phase,D.Origin;
   use type Interfaces.Unsigned_64,Interfaces.Unsigned_32,Interfaces.Unsigned_8;
   use type AML_Decode.Integer_Width,D.Catalog_Identifier,D.Namespace_Position;
   Checks,Critical_Lines:Natural:=0;Fault_Seen:Boolean:=False;
   procedure Dump(Prefix:String) is
      package IO renames Ada.Streams.Stream_IO;
   begin
      for ID in 1..Startup_Test_Firmware.Table_Count loop
         declare
            Raw:constant Startup_Test_Firmware.Bytes:=Startup_Test_Firmware.Image(ID);
            Data:Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Raw'Length));
            File:IO.File_Type;
         begin
            for I in Raw'Range loop Data(Ada.Streams.Stream_Element_Offset(I-Raw'First+1)):=Ada.Streams.Stream_Element(Raw(I));end loop;
            IO.Create(File,IO.Out_File,Prefix&(if ID=Startup_Test_Firmware.DSDT_ID then ".dsdt" else ".fadt"));
            IO.Write(File,Data);IO.Close(File);
         end;
      end loop;
   end Dump;
   procedure Logs is
   begin for I in 1..T.Log_Count loop Ada.Text_IO.Put_Line(T.Log(I).Text(1..T.Log(I).Length));end loop;end Logs;
   procedure Check(Good:Boolean;Label:String) is
   begin Checks:=Checks+1;if not Good then Logs;raise Program_Error with Label;end if;end Check;
begin
   Check(Ada.Command_Line.Argument_Count in 3..4,"scenario width logger arguments");
   T.Selected:=T.Scenario'Value(Ada.Command_Line.Argument(1));
   if T.Selected not in T.Module_Fatal|T.Root_Fatal|T.Bus_Fatal|T.Device_Fatal then raise Program_Error with "unsupported Fatal scenario";end if;
   if Ada.Command_Line.Argument(2)="32" then T.Width:=AML_Decode.Bits_32;
   elsif Ada.Command_Line.Argument(2)="64" then T.Width:=AML_Decode.Bits_64;
   else raise Program_Error with "unknown AML width";end if;
   T.Broken_Critical:=Boolean'Value(Ada.Command_Line.Argument(3));
   if Ada.Command_Line.Argument_Count=4 then Dump(Ada.Command_Line.Argument(4));end if;
   begin ACPI_Native_Instance.Start;
   exception when E:Program_Error=>
      Fault_Seen:=True;
      Check(T.Broken_Critical and Ada.Exceptions.Exception_Message(E)="modeled critical logger failure",
        "only exact trusted critical-logger failure propagates");
   end;
   Check(Fault_Seen=T.Broken_Critical,"Fatal is typed; only modeled terminal logger contract raises");
   Check(T.Loads=1 and T.Copies=2,"Fatal fixture copied complete DSDT and FADT once");
   Check(T.Passed_Deadline=60_000_010,"original finite startup deadline");
   Check(T.Last_Load.Fatal.Present,"real interpreter Fatal copied before cleanup");
   declare Seen:D.Observation renames T.Last_Load.Fatal;begin
      Check(Seen.Item.Literal.Kind=16#A7# and Seen.Item.Literal.Code=16#F234_5678# and Seen.Item.Evaluated_Argument=85,
        "exact Fatal type code and evaluated scalar argument");
      Check(Seen.Item.Width=T.Width,"Fatal integer width retained");
      Check(Seen.Source.Kind=(if T.Selected=T.Module_Fatal then D.Definition_Block else D.Device_Initializer),"module versus initializer Fatal origin");
      if T.Selected=T.Module_Fatal then Check(Seen.Source.Table_ID=1,"Fatal belongs to DSDT catalog entry");
      else Check((if T.Selected=T.Root_Fatal then Seen.Source.Initializer_Scope=0 else Seen.Source.Initializer_Scope>0),"root versus non-root initializer source preserved");end if;
   end;
   Check(T.Last_Load.Status=(if T.Selected=T.Module_Fatal then L.Startup_Refused else L.Loaded),
     "module failure versus complete catalog with failed initializer");
   Check(T.Null_Inspections=0,"failed startup cannot submit consumer completion");
   Check(T.Critical_Attempts=1 and T.Recovery_Attempts=0,"one Fatal report and no invented recovery");
   for I in 1..T.Log_Count loop
      if T.Log(I).Severity=T.Critical then
         Critical_Lines:=Critical_Lines+1;
         Check(T.Log(I).Text(1..T.Log(I).Length)=D.Format(T.Last_Load.Fatal) and T.Log(I).Length<=D.Maximum_Text_Length,
           "exact detached bounded Fatal diagnostic survives return or logger failure");
      end if;
   end loop;
   Check(Critical_Lines=1,"one Critical record");
   if not T.Broken_Critical then
      Check(T.Detections=Boolean'Pos(T.Selected/=T.Module_Fatal) and T.Power_Detections=Boolean'Pos(T.Selected/=T.Module_Fatal),
        "discovery only after complete retained catalog");
      Check(T.Registrations=0 and T.Serves=0,"Fatal module or unfinished device walk cannot register or serve");
      Check(One.Runtime.Current=One.Runtime.Status.Stopped and One.Root.Shutdown.May_Retire,"normal Fatal outcome retires physical owner");
      Check(T.Timing_Reports=1,"typed Fatal outcome finalizes timing once");
   else
      Check(T.Detections=0 and T.Power_Detections=0 and T.Registrations=0 and T.Serves=0,"terminal logger failure prevents later startup work");
      Check(One.Runtime.Current=One.Runtime.Status.Fresh and not One.Root.Shutdown.May_Retire,"hosted propagation does not invent cleanup");
      Check(T.Timing_Reports=0,"terminal logger exception does not finalize timing");
   end if;
   declare Until_Time:Interfaces.Unsigned_64;Accepted:Boolean;begin
      ACPI_Native_Delays.Inspect_Window(Until_Time,Accepted);
      Check(Accepted=T.Broken_Critical and Until_Time=(if Accepted then 60_000_010 else 0),"actual delay lifetime follows return versus terminal contract fault");
   end;
   declare N:constant Natural:=T.Log_Count;Clocks:constant Natural:=T.Clock_Reads;Registers:constant Natural:=T.Registrations;Serves:constant Natural:=T.Serves;begin
      ACPI_Native_Instance.Start;
      Check(T.Log_Count=N and T.Clock_Reads=Clocks and T.Loads=1 and T.Copies=2 and T.Registrations=Registers and T.Serves=Serves,
        "second Start cannot replay Fatal or logger failure");
   end;
   if Ada.Command_Line.Argument_Count=4 then
      declare File:Ada.Text_IO.File_Type;begin
         Ada.Text_IO.Create(File,Ada.Text_IO.Out_File,Ada.Command_Line.Argument(4)&".fatal");
         Ada.Text_IO.Put_Line(File,D.Format(T.Last_Load.Fatal));Ada.Text_IO.Close(File);
      end;
   end if;
   Ada.Text_IO.Put_Line("Actual root Fatal checks"&Checks'Image);
end Startup_Fatal_Tests;
