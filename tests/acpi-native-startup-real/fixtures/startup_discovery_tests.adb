pragma Ada_2022;
with Ada.Command_Line; with Ada.Exceptions; with Ada.Text_IO; with Ada.Strings.Fixed;
with AML_Decode; with Interfaces; with ACPI_Hosted_Heap;
with ACPI_Native_Instance; with ACPI_One_Service; with ACPI_Native_Delays;
with ACPI_Snapshot_Startup; with Owner_Test_Control;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Startup_Discovery_Tests is
   package T renames Owner_Test_Control; package One renames ACPI_One_Service;
   use type T.Scenario,T.Level,ACPI_Snapshot_Startup.Outcome,One.Runtime.Status.Phase;
   use type Interfaces.Unsigned_64;
   Checks:Natural:=0; Fault_Seen:Boolean:=False;
   Stop_Lines,Summary_Lines,Retire_Lines:Natural:=0;
   Power_Case,Exception_Case:Boolean;
   procedure Check(Good:Boolean;Label:String) is
   begin
      Checks:=Checks+1;
      if not Good then
         for I in 1..T.Log_Count loop Ada.Text_IO.Put_Line(T.Log(I).Text(1..T.Log(I).Length));end loop;
         raise Program_Error with Label;
      end if;
   end Check;
begin
   Check(Ada.Command_Line.Argument_Count=3,"scenario width logger arguments");
   T.Selected:=T.Scenario'Value(Ada.Command_Line.Argument(1));
   if T.Selected not in T.Detection_Cleanup|T.Detection_Exception|T.Detection_Recovery|T.Power_Cleanup|T.Power_Exception then
      raise Program_Error with "unsupported discovery scenario";
   end if;
   if Ada.Command_Line.Argument(2)="32" then T.Width:=AML_Decode.Bits_32;
   elsif Ada.Command_Line.Argument(2)="64" then T.Width:=AML_Decode.Bits_64;
   else raise Program_Error with "unknown AML width";end if;
   T.Broken_Critical:=Boolean'Value(Ada.Command_Line.Argument(3));
   Power_Case:=T.Selected in T.Power_Cleanup|T.Power_Exception;
   Exception_Case:=T.Selected in T.Detection_Exception|T.Power_Exception;
   begin ACPI_Native_Instance.Start;
   exception when E:Program_Error=>
      Fault_Seen:=True;
      Check(Exception_Case and Ada.Exceptions.Exception_Message(E)=
         (if Power_Case then "modeled power exception" else "modeled unexpected detection exception"),
         "only exact hosted boundary exception propagates");
   end;
   Check(Fault_Seen=Exception_Case,"typed discovery outcome versus terminal boundary exception");
   Check(T.Loads=1 and T.Last_Load.Status=ACPI_Snapshot_Startup.Loaded,"actual catalog and initialization complete first");
   Check(T.Detections=1 and T.Detected_At>T.Load_Completed_At,"HID discovery runs once after actual snapshot");
   Check(T.Power_Detections=Boolean'Pos(Power_Case),"HID cleanup or exception prevents later power discovery");
   if Power_Case then Check(T.Power_Detected_At>T.Detected_At,"power follows HID pass");end if;
   Check(T.Registrations=0 and T.Serves=0 and T.Null_Inspections=0,"uncertain discovery cannot register or serve");
   Check(T.Critical_Attempts=0 and T.Recovery_Attempts=0 and not T.Last_Load.Fatal.Present,"discovery failure does not invent AML Fatal or recovery");
   Check((if Exception_Case then One.Runtime.Current=One.Runtime.Status.Fresh and not One.Root.Shutdown.May_Retire
       else One.Runtime.Current=One.Runtime.Status.Stopped and One.Root.Shutdown.May_Retire),
      "typed outcomes retire the handle-free modeled discovery owner; terminal exceptions do not finalize");
   Check(T.Timing_Reports=Boolean'Pos(not Exception_Case),"normal typed discovery stop finalizes timing; terminal exception does not");
   for I in 1..T.Log_Count loop
      declare Text:constant String:=T.Log(I).Text(1..T.Log(I).Length);begin
         if Ada.Strings.Fixed.Index(Text,"acpi: stage startup-stop fail ")=1 then
            Stop_Lines:=Stop_Lines+1;
            Check(T.Log(I).Severity=T.Warning and Text=
              (if Power_Case then "acpi: stage startup-stop fail POWER_DISCOVERY recovery=1"
               elsif T.Selected=T.Detection_Cleanup then "acpi: stage startup-stop fail HID_DISCOVERY reason=CLEANUP_FAILED step=NOT_STARTED"
               else "acpi: stage startup-stop fail HID_DISCOVERY reason=RECOVERY_REQUIRED step=NOT_STARTED"),"exact copied discovery refusal");
         elsif Text=(if Power_Case then "acpi: bring-up reached power-discovery (3/4)" else "acpi: bring-up reached hid-discovery (3/4)") then
            Summary_Lines:=Summary_Lines+1;Check(T.Log(I).Severity=T.Warning,"final discovery summary has Warning severity");
         elsif Text="acpi: stage retire ok QUIESCENT" then Retire_Lines:=Retire_Lines+1;end if;
      end;
   end loop;
   Check(Stop_Lines=Boolean'Pos(not Exception_Case) and Summary_Lines=Boolean'Pos(not Exception_Case) and Retire_Lines=Boolean'Pos(not Exception_Case),
      "typed discovery refusal reports once while terminal host exception invents no later logs or retirement");
   declare Until_Time:Interfaces.Unsigned_64;Accepted:Boolean;begin
      ACPI_Native_Delays.Inspect_Window(Until_Time,Accepted);
      Check(Accepted=Exception_Case and Until_Time=(if Exception_Case then 60_000_010 else 0),"typed discovery stop closes delay domain; exception preserves original bounded window");
   end;
   declare
      Clocks:constant Natural:=T.Clock_Reads; Logs:constant Natural:=T.Log_Count;
      Copies:constant Natural:=T.Copies; Detections:constant Natural:=T.Detections;
      Powers:constant Natural:=T.Power_Detections;
   begin
      ACPI_Native_Instance.Start;
      Check(T.Loads=1 and T.Clock_Reads=Clocks and T.Log_Count=Logs and T.Copies=Copies and T.Detections=Detections
         and T.Power_Detections=Powers and T.Registrations=0 and T.Serves=0,"second Start cannot replay partially retained startup");
   end;
   Ada.Text_IO.Put_Line("Actual root discovery checks"&Checks'Image);
end Startup_Discovery_Tests;
