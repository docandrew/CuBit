pragma Ada_2022;
with ACPI_Service;with Interfaces;with ACPI_EC_Types;with ACPI_Local_EC_Commands;
with ACPI_Local_Runtime;with ACPI_Requests.With_Local_Runtime;
-- Hosted model of native Fields, hardware/clock and delay boundary only.
-- The command engine, local runtime, Core, selector and source body are real.
package ACPI_One_Service is
   package C renames ACPI_Service;package T renames ACPI_EC_Types;
   Boot_Ready:Boolean:=True;
   Now:Interfaces.Unsigned_64:=100;
   Expire_After_Candidate : Boolean := False;
   Delay_After_Candidate : Interfaces.Unsigned_64 := 0;
   Delay_In_Sample_HID:Interfaces.Unsigned_64:=0;
   Button_Test_Delay:Interfaces.Unsigned_64:=0;
   Button_Test_Unsafe,Fail_Next_HID:Boolean:=False;
   Query_Windows : Natural := 0;
   Maximum_Window_Length, Last_Window_Length : Interfaces.Unsigned_64 := 0;
   function Current_Event_Deadline return Interfaces.Unsigned_64;
   Read_Clock_Available : Boolean := True;
   Clock_Observations : Natural := 0;
   Reads,Writes,Enters,Leaves,Eval_Calls:Natural:=0;
   Pause_Fails,Leave_Fails,Status_Unknown,End_Fails,Event_Zero,New_Edge_On_Empty:Boolean:=False;
   Guard:Boolean:=False;
   function Ready(Service:C.State) return Boolean;
   procedure Bind(Service:C.State;Completed:out Boolean);
   procedure Claim(Service:C.State;Accepted:out Boolean);
   procedure Close_Owner;
   procedure Enter_Delays(Accepted:out Boolean);
   procedure Begin_Delays(Deadline:Interfaces.Unsigned_64;Accepted:out Boolean);
   procedure End_Delays(Accepted:out Boolean);
   procedure Stop_Delays;
   package Root is
      package Hardware is
         function Admitting return Boolean;
      end Hardware;
      package Fields is
         type EC_Boot_Status is (EC_Ready,EC_Absent);
         type EC_Boot_Report is record
            Status:EC_Boot_Status:=EC_Ready;
            Control:Interfaces.Unsigned_64:=16#66#;
            UID:Interfaces.Unsigned_32:=0;
            GPE:T.Byte:=16#6E#;
            Data:Interfaces.Unsigned_64:=16#62#;
         end record;
         function EC_Boot_Observation return EC_Boot_Report;
         procedure Read(Register:T.Read_Register;Value:out T.Byte;Result:out T.Access_Result);
         procedure Write(Register:T.Write_Register;Value:T.Byte;Result:out T.Access_Result);
         procedure Clock(Time:out T.Microseconds;Available:out Boolean);
         procedure Enter_Command(For_Query,For_Write:Boolean;Deadline:T.Microseconds;Accepted:out Boolean);
         procedure Leave_Command(Released:out Boolean);
         function Command_Quiet return Boolean;
         function Backing_Owned return Boolean;
         package EC_Commands is new ACPI_Local_EC_Commands(Read,Write,Clock,Enter_Command,Leave_Command,Command_Quiet,Backing_Owned);
         function Control_Idle return Boolean;
         procedure Wait_EC(Target:T.Microseconds;Available:out Boolean);
      end Fields;
   end Root;
   procedure Evaluate(Service:aliased in out C.State;Item:C.Node_Descriptor;Budget:Natural;
      Result:out C.Values.Result;Status:out C.Values.Access_Status);
   package Runtime is new ACPI_Local_Runtime(C,Ready,Root.Fields.Control_Idle,Claim,Close_Owner,
      Enter_Delays,Begin_Delays,End_Delays,Stop_Delays,Evaluate,C.Invoke_Retained,C.Invoke_With_Arguments);
   package Runtime_Access is new ACPI_Requests.With_Local_Runtime(Runtime.Enter_Runtime,Runtime.Begin_Event,Runtime.End_Event);
end ACPI_One_Service;
