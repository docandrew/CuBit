pragma Ada_2022;
with AML_Execute;
with ACPI_Delay_Domains;with ACPI_SCI_Events;
package body ACPI_One_Service is
   use type Interfaces.Unsigned_64,C.Context_Key,T.Read_Register,T.Write_Register,T.Byte;
   Anchor:C.Context_Key:=C.No_Context;Claimed,Closed:Boolean:=False;
   Domain:ACPI_Delay_Domains.State;
   Window:Interfaces.Unsigned_64:=0;
   Output:Boolean:=False;Next_Query:Natural:=1;
   function Current_Event_Deadline return Interfaces.Unsigned_64 is (Window);
   function Ready(Service:C.State) return Boolean is
     (not Closed and then Anchor/=C.No_Context and then C.Current_Context(Service,Anchor));
   procedure Bind(Service:C.State;Completed:out Boolean) is
   begin Anchor:=C.Capture_Context(Service);Completed:=Anchor/=C.No_Context;
      ACPI_Delay_Domains.Start(Domain,Now,Now+1_000_000,True,Completed);end Bind;
   procedure Claim(Service:C.State;Accepted:out Boolean) is
   begin Accepted:=Ready(Service) and then not Claimed;if Accepted then Claimed:=True;end if;end Claim;
   procedure Close_Owner is begin Closed:=True;end Close_Owner;
   procedure Enter_Delays(Accepted:out Boolean) is
   begin ACPI_Delay_Domains.Enter_Runtime(Domain,Now,True,Accepted);end Enter_Delays;
   procedure Begin_Delays(Deadline:Interfaces.Unsigned_64;Accepted:out Boolean) is
   begin Window:=Deadline;
      Last_Window_Length := (if Deadline > Now then Deadline - Now else 0);
      if Last_Window_Length=500_000 then Query_Windows:=Query_Windows+1;end if;
      Maximum_Window_Length := Interfaces.Unsigned_64'Max (Maximum_Window_Length, Last_Window_Length);
      ACPI_Delay_Domains.Begin_Event(Domain,Now,Deadline,True,Accepted);end Begin_Delays;
   procedure End_Delays(Accepted:out Boolean) is
   begin ACPI_Delay_Domains.End_Event(Domain,Now,True,Accepted);Accepted:=Accepted and not End_Fails;end End_Delays;
   procedure Stop_Delays is begin ACPI_Delay_Domains.Stop(Domain);end Stop_Delays;
   procedure Evaluate(Service:aliased in out C.State;Item:C.Node_Descriptor;Budget:Natural;
      Result:out C.Values.Result;Status:out C.Values.Access_Status) is
   begin
      Eval_Calls:=Eval_Calls+1;
      if C.Values.Node_Name(Item)="_HID" and then Fail_Next_HID then
         Fail_Next_HID:=False;Result:=(Status=>AML_Execute.Field_Indeterminate,Charged=>0);
         Status:=C.Values.Available;return;
      end if;
      if C.Values.Node_Name(Item)="_HID" and then Delay_In_Sample_HID/=0 then
         Now:=Now+Delay_In_Sample_HID;Delay_In_Sample_HID:=0;
         -- Model an EC-backed identity read crossing its admitted IO window.
         -- Pure metadata taking longer than a window is not itself a fault.
         if Now>=Window then
            Result:=(Status=>AML_Execute.Field_Indeterminate,Charged=>0);
            Status:=C.Values.Available;return;
         end if;
      end if;
      C.Evaluate_Named(Service,Item,Budget,Result,Status);
   end Evaluate;
   package body Root is
      package body Hardware is
         function Admitting return Boolean is (not Closed);
      end Hardware;
      package body Fields is
         function EC_Boot_Observation return EC_Boot_Report is ((Status=>(if Boot_Ready then EC_Ready else EC_Absent),others=><>));
         function Control_Idle return Boolean is (not Guard and then EC_Commands.Quiescent);
         function Command_Quiet return Boolean is (not Guard);
         function Backing_Owned return Boolean is (Guard);
         procedure Clock(Time:out T.Microseconds;Available:out Boolean) is
         begin Now:=Now+1;Time:=T.Microseconds(Now);Available:=True;end Clock;
         procedure Enter_Command(For_Query,For_Write:Boolean;Deadline:T.Microseconds;Accepted:out Boolean) is
            use type T.Microseconds;
         begin
            Enters:=Enters+1;Accepted:=For_Query and not For_Write and not Guard and Deadline>0 and Interfaces.Unsigned_64(Deadline)=Window;
            if Accepted then Guard:=True;end if;
         end Enter_Command;
         procedure Leave_Command(Released:out Boolean) is
         begin Leaves:=Leaves+1;Released:=not Leave_Fails;if Released then Guard:=False;end if;end Leave_Command;
         procedure Read(Register:T.Read_Register;Value:out T.Byte;Result:out T.Access_Result) is
         begin
            Reads:=Reads+1;Value:=0;Result:=T.Completed;
            if not Guard then Result:=T.Refused;return;end if;
            if Status_Unknown then Result:=T.Indeterminate;return;end if;
            if Register=T.Status_Register then
               Value:=(if Next_Query<=2 then T.Event_Pending else 0);
               if Output then Value:=Value or T.Output_Full;end if;
               if Next_Query>2 and then New_Edge_On_Empty then
                  ACPI_SCI_Events.Latched:=True;New_Edge_On_Empty:=False;
               end if;
            else
               if not Output then Result:=T.Refused;return;end if;
               Output:=False;Value:=(if Event_Zero then 0 elsif Next_Query=1 then 16#58# else 16#59#);
               Next_Query:=(if Event_Zero then 3 else Next_Query+1);
            end if;
         end Read;
         procedure Write(Register:T.Write_Register;Value:T.Byte;Result:out T.Access_Result) is
         begin
            Writes:=Writes+1;Result:=T.Refused;
            if Guard and Register=T.Command_Register and Value=T.Query_Command then Output:=True;Result:=T.Completed;end if;
         end Write;
         procedure Wait_EC(Target:T.Microseconds;Available:out Boolean) is
         begin
            Now:=Interfaces.Unsigned_64'Max(Now+1,Interfaces.Unsigned_64(Target));
            Available:=not Pause_Fails and then Now<Window;
         end Wait_EC;
      end Fields;
   end Root;
end ACPI_One_Service;
