pragma Ada_2022;
with Ada.Command_Line;with Ada.Text_IO;
with AML_Decode;with AML_Delays;with AML_Execute;
with AML_Objects;with ACPI_Service_Core;with ACPI_Hosted_Heap;with Firmware_Tables;
with Ada.Streams;with Ada.Streams.Stream_IO;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Module_Delay_Tests is
   package D renames AML_Decode;package E renames AML_Execute;
   use type D.Byte,D.Bytes,D.Integer_Value,AML_Delays.Outcome,AML_Delays.Delay_Kind;
   use type AML_Delays.Stall_Microseconds,AML_Delays.Sleep_Milliseconds;
   Selected:AML_Delays.Outcome:=AML_Delays.Completed;
   Calls,Clock_Reads,Checks:Natural:=0;
   Last:AML_Delays.Request;
   procedure Provider(Item:AML_Delays.Request;Result:out AML_Delays.Outcome) is
   begin Calls:=Calls+1;Last:=Item;Result:=Selected;end Provider;
   procedure Read_Clock(Value:out D.Integer_Value;Available:out Boolean) is
   begin Clock_Reads:=Clock_Reads+1;Value:=0;Available:=True;end Read_Clock;
   package C is new ACPI_Service_Core
     (Perform_Delay=>Provider,Read_Microseconds=>Read_Clock,
      Namespace_Node_Capacity=>128,Retained_Result_Capacity=>16);
   use type C.Stage_Status,C.Freeze_Status,C.Values.Access_Status,C.Values.Module_Outcome;
   use type E.Execution_Status,C.Namespace.Lookup_Status;
   procedure Check(Good:Boolean;Why:String) is
   begin Checks:=Checks+1;if not Good then raise Program_Error with Why;end if;end Check;
   Scenario:constant String:=Ada.Command_Line.Argument(1);
   Width:constant Positive:=Positive'Value(Ada.Command_Line.Argument(2));
   Enabled:constant Boolean:=Boolean'Value(Ada.Command_Line.Argument(4));
   function Enc(Text:String) return D.Bytes is
      B:D.Bytes(1..Text'Length);
   begin for I in B'Range loop B(I):=Character'Pos(Text(Text'First+I-1));end loop;return B;end Enc;
   function Body_Code return D.Bytes is
      Tail:constant D.Bytes:=D.Bytes'(16#70#,1)&Enc("MARK");
   begin
      if Scenario="stall" or Scenario="budget" then return D.Bytes'(16#5B#,16#21#,16#0A#,7)&Tail;
      elsif Scenario="sleep" then return D.Bytes'(16#5B#,16#22#,16#0A#,2)&Tail;
      elsif Scenario="sequence" then return D.Bytes'(16#5B#,16#21#,1,16#5B#,16#22#,16#0A#,2)&Tail;
      elsif Scenario="nested" then return D.Bytes'(16#14#,9)&Enc("WAIT")&D.Bytes'(0,16#5B#,16#21#,1)&Enc("WAIT")&Tail;
      elsif Scenario="invalid" then return D.Bytes'(16#5B#,16#21#,16#0B#,0,1)&Tail;
      elsif Scenario="timer" then return D.Bytes'(16#70#,16#5B#,16#33#)&Enc("MARK")&Tail;
      elsif Scenario="debug" then return D.Bytes'(16#70#,1,16#5B#,16#31#)&Tail;
      else raise Program_Error with "unknown scenario";end if;
   end Body_Code;
   function Table return Firmware_Tables.Bytes is
      Body_Bytes:constant D.Bytes:=D.Bytes'(1=>8)&Enc("MARK")&D.Bytes'(1=>0)&Body_Code;
      Raw:Firmware_Tables.Bytes(1..36+Body_Bytes'Length):=[others=>0];
      N:Natural:=Raw'Length;Sum:Firmware_Tables.Byte:=0;
   begin
      Raw(1..4):=[16#44#,16#53#,16#44#,16#54#];
      for I in 5..8 loop Raw(I):=Firmware_Tables.Byte(N mod 256);N:=N/256;end loop;
      Raw(9):=Firmware_Tables.Byte(Width);
      for I in Body_Bytes'Range loop Raw(36+I):=Body_Bytes(I);end loop;
      for B of Raw loop Sum:=Sum+B;end loop;Raw(10):=0-Sum;return Raw;
   end Table;
   Data:constant Firmware_Tables.Bytes:=Table;
   Service:aliased C.State(1,Data'Length,Data'Length,1,
      Value_Object_Capacity=>AML_Objects.Max_Objects,
      Value_Byte_Capacity=>AML_Objects.Max_Bytes,Value_Element_Capacity=>AML_Objects.Max_Elements);
   Staged:C.Stage_Status;Frozen:C.Freeze_Status;Report:C.Execution_Report;
   Status:C.Values.Access_Status;
   Expected:E.Execution_Status:=E.Unsupported;
   Wanted_Calls:Natural:=0;
   Success:Boolean:=False;
begin
   Check(Ada.Command_Line.Argument_Count in 4|5,"scenario width provider feature arguments");
   Check(Width in 1|2,"DSDT integer width");
   Selected:=AML_Delays.Outcome'Value(Ada.Command_Line.Argument(3));
   if Ada.Command_Line.Argument_Count=5 then
      declare
         package IO renames Ada.Streams.Stream_IO;
         File:IO.File_Type;
         Raw:Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Data'Length));
      begin
         for I in Data'Range loop Raw(Ada.Streams.Stream_Element_Offset(I)):=Ada.Streams.Stream_Element(Data(I));end loop;
         IO.Create(File,IO.Out_File,Ada.Command_Line.Argument(5));IO.Write(File,Raw);IO.Close(File);
      end;
   end if;
   if Scenario="budget" then Expected:=E.Budget_Exceeded;
   elsif Scenario in "timer"|"debug" then Expected:=E.Unsupported;
   elsif not Enabled then Expected:=E.Unsupported;
   elsif Scenario="invalid" then Expected:=E.Invalid_Delay;
   else
      Wanted_Calls:=(if Scenario="sequence" and Selected=AML_Delays.Completed then 2 else 1);
      Expected:=(case Selected is when AML_Delays.Completed=>E.Module_Completed,
        when AML_Delays.Unavailable=>E.Unsupported,when AML_Delays.Failed=>E.Delay_Failed);
      Success:=Selected=AML_Delays.Completed;
   end if;
   C.Stage_Table(Service,1,C.DSDT,Data,Staged);Check(Staged=C.Staged,"stage immutable DSDT");
   C.Freeze_Catalog(Service,Frozen);Check(Frozen=C.Catalog_Frozen_OK,"freeze catalog");
   C.Execute_Frozen(Service,(if Scenario="budget" then 0 else 1000),Report,Status);
   Check(Status=C.Values.Available,"typed interpreter outcome");
   Check(Calls=Wanted_Calls,"configured delay provider call count");
   Check(Report.Modules.Outcome=(if Success then C.Values.Modules_Completed else C.Values.Module_Rejected),"module completion status");
   Check((if Success then Report.Modules.Failure=E.No_Return else Report.Modules.Failure=Expected),"exact failure class");
   Check(C.Valid(Service) and C.Retained_Results(Service)=0,"valid owner and no escaped results");
   Check(Clock_Reads=0,"loading still forbids Timer provider access");
   Check(Report.Modules.Charged<=(if Scenario="budget" then 0 else 1000),"one finite operation budget");
   if Wanted_Calls>0 then
      Check(Last.Kind=(if Scenario="sleep" or (Scenario="sequence" and Success) then AML_Delays.Sleep_Delay else AML_Delays.Stall_Delay),"normalized delay kind");
      if Last.Kind=AML_Delays.Stall_Delay then
         Check(Last.Microseconds=(if Scenario in "stall"|"budget" then 7 else 1),"normalized Stall amount");
      else Check(Last.Milliseconds=2,"normalized Sleep amount");end if;
   end if;
   if Success then
      declare Found:C.Namespace.Lookup_Result;Value:C.Values.Result;begin
         C.Find_Object(Service,C.Namespace.Root,"MARK",Found,Status);
         Check(Status=C.Values.Available and Found.Status=C.Namespace.Found,"continuation marker lookup");
         C.Observe_Named_Value(Service,Found.Node,Value,Status);
         Check(Status=C.Values.Available and then Value.Status=E.Returned and then Value.Number=1,"execution continues after completed delay");
      end;
   end if;
   declare Again:C.Execution_Report;begin
      C.Execute_Frozen(Service,1000,Again,Status);
      Check(Status=C.Values.Wrong_Phase and Calls=Wanted_Calls,"failed or successful module cannot replay delay");
   end;
   Ada.Text_IO.Put_Line("Module delay "&Scenario&" checks"&Checks'Image);
end Module_Delay_Tests;
