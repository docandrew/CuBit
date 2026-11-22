pragma Ada_2022;
with Ada.Command_Line;with Ada.Exceptions;with Ada.Text_IO;with Ada.Strings.Fixed;with Ada.Streams.Stream_IO;
with Interfaces;with AML_Decode;with ACPI_Hosted_Heap;with ACPI_Native_Instance;
with ACPI_One_Service;with ACPI_Native_Delays;with ACPI_Snapshot_Startup;
with ACPI_Module_Diagnostics;with Startup_Test_Firmware;with Owner_Test_Control;
with ACPI_Native_Regions;with ACPI_Native_Endpoint;with ACPI_Serving_State;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Startup_Error_Tests is
   package T renames Owner_Test_Control;package One renames ACPI_One_Service;
   package L renames ACPI_Snapshot_Startup;package D renames ACPI_Module_Diagnostics;
   use type T.Scenario,T.Level,L.Outcome,L.R.Tick,One.Runtime.Status.Phase,D.Count;
   use type Interfaces.Unsigned_64,ACPI_Serving_State.Mode;
   Checks,Recovery_Lines,Device_Lines:Natural:=0;Fault_Seen,Recovered,Loaded:Boolean:=False;
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
   if T.Selected not in T.Ordinary_Error|T.Unconfigured_Region|T.Recovered|T.Recovered_Then_Failed|T.Recovery_Log_Failure then
      raise Program_Error with "unsupported recovery scenario";
   end if;
   if Ada.Command_Line.Argument(2)="32" then T.Width:=AML_Decode.Bits_32;
   elsif Ada.Command_Line.Argument(2)="64" then T.Width:=AML_Decode.Bits_64;
   else raise Program_Error with "unknown AML width";end if;
   T.Broken_Critical:=Boolean'Value(Ada.Command_Line.Argument(3));
   if Ada.Command_Line.Argument_Count=4 then Dump(Ada.Command_Line.Argument(4));end if;
   Recovered:=T.Selected in T.Recovered|T.Recovered_Then_Failed|T.Recovery_Log_Failure;
   Loaded:=T.Selected/=T.Recovered_Then_Failed;
   begin ACPI_Native_Instance.Start;
   exception when E:Program_Error=>Fault_Seen:=True;
      Check(T.Selected=T.Recovery_Log_Failure and Ada.Exceptions.Exception_Message(E)="modeled recovery logger failure",
        "only exact modeled recovery logger fault propagates");
   end;
   Check(Fault_Seen=(T.Selected=T.Recovery_Log_Failure),"recoverable AML error versus terminal observer contract fault");
   Check(T.Loads=1 and T.Copies=2 and T.Passed_Deadline=60_000_010,"one complete bounded table copy");
   Check(T.Last_Load.Status=(if Loaded then L.Loaded else L.Startup_Refused),"recovered module versus later failed wait");
   Check(not T.Last_Load.Fatal.Present and T.Critical_Attempts=0,"ordinary error or unavailable region never invents Fatal");
   Check(T.Last_Load.Module_Errors.Count=D.Count(Boolean'Pos(Recovered)),"copied exact recoverable module error count");
   Check(T.Recovery_Attempts=Boolean'Pos(Recovered),"one recovery publication attempt");
   for I in 1..T.Log_Count loop
      declare Text:constant String:=T.Log(I).Text(1..T.Log(I).Length);begin
         if Ada.Strings.Fixed.Index(Text,"acpi: stage aml-recovery ok ")=1 then
            Recovery_Lines:=Recovery_Lines+1;
            Check(T.Log(I).Severity=T.Warning and Text=D.Format(T.Last_Load.Module_Errors) and Text'Length<=D.Maximum_Text_Length,
              "exact detached bounded recovery Warning survives later failure");
         elsif Ada.Strings.Fixed.Index(Text,"acpi: stage devices ")=1 then
            Device_Lines:=Device_Lines+1;
            Check((T.Log(I).Severity=T.Warning)=(T.Selected in T.Ordinary_Error|T.Unconfigured_Region) and Text'Length<=256,
              "device errors distinguish incomplete initialization");
         end if;
      end;
   end loop;
   Check(Recovery_Lines=Boolean'Pos(Recovered),"recovery observation is not duplicated");
   if not Fault_Seen then
      Check(T.Detections=Boolean'Pos(Loaded) and T.Power_Detections=Boolean'Pos(Loaded),"discovery only after retained complete catalog");
      Check(Device_Lines=Boolean'Pos(Loaded),"one device result after loaded snapshot");
      Check(T.Registrations=Boolean'Pos(Loaded) and T.Serves=Boolean'Pos(Loaded),"only admitted runtime or observer can register");
      if T.Selected=T.Unconfigured_Region then
         Check(ACPI_Native_Endpoint.Serving_State.Current=ACPI_Serving_State.Local_Runtime
           and ACPI_Native_Regions.Observe.Unavailable=1 and not ACPI_Native_Regions.Observe.Pending,
           "partial binding keeps the missing EC region unavailable without pending IO");
      end if;
      Check(One.Runtime.Current=One.Runtime.Status.Stopped and One.Root.Shutdown.May_Retire,"normal typed error outcome retires real root");
      Check(T.Timing_Reports=1,"normal typed return finalizes timing once");
   else
      Check(T.Detections=0 and T.Power_Detections=0 and T.Registrations=0 and T.Serves=0,"terminal recovery logger fault prevents further startup");
      Check(One.Runtime.Current=One.Runtime.Status.Fresh and not One.Root.Shutdown.May_Retire and T.Timing_Reports=0,
        "terminal hosted fault does not invent cleanup or final timing");
   end if;
   if T.Selected=T.Recovered_Then_Failed then
      Check(T.Stall_Entries=1 and T.Sleep_Entries=0 and T.Clock_Reads>=65_536 and T.Clock_Reads<66_000,"module delay reaches native provider and exhausts its frozen-clock observation budget");
   end if;
   declare Until_Time:Interfaces.Unsigned_64;Accepted:Boolean;begin
      ACPI_Native_Delays.Inspect_Window(Until_Time,Accepted);
      Check(Accepted=Fault_Seen and Until_Time=(if Accepted then 60_000_010 else 0),"delay domain follows typed return versus terminal observer fault");
   end;
   declare N:constant Natural:=T.Log_Count;Clocks:constant Natural:=T.Clock_Reads;Regs:constant Natural:=T.Registrations;Serves:constant Natural:=T.Serves;begin
      ACPI_Native_Instance.Start;
      Check(T.Log_Count=N and T.Clock_Reads=Clocks and T.Loads=1 and T.Copies=2 and T.Registrations=Regs and T.Serves=Serves,
        "second Start cannot replay error recovery or observer failure");
   end;
   if Ada.Command_Line.Argument_Count=4 then
      declare File:Ada.Text_IO.File_Type;begin
         Ada.Text_IO.Create(File,Ada.Text_IO.Out_File,Ada.Command_Line.Argument(4)&".observations");
         for I in 1..T.Log_Count loop Ada.Text_IO.Put_Line(File,T.Log(I).Text(1..T.Log(I).Length));end loop;
         Ada.Text_IO.Close(File);
      end;
   end if;
   Ada.Text_IO.Put_Line("Actual root error checks"&Checks'Image);
end Startup_Error_Tests;
