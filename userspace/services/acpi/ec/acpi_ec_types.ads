package ACPI_EC_Types with SPARK_Mode is
   type Byte is mod 2 ** 8;
   type Cell_Offset is range 0 .. 255;
   type Microseconds is range 0 .. 2 ** 63 - 1;
   type Work_Budget is range 0 .. 1_000_000;
   type Read_Register is (Status_Register, Data_Register);
   type Write_Register is (Command_Register, Data_Register);
   type Access_Result is (Completed, Refused, Indeterminate);
   type Operation is (Read_Cell, Write_Cell, Query_Event);
   type Outcome is
     (Idle, Running, Succeeded, No_Event, Stopped, Deadline_Reached,
      Budget_Exhausted, Clock_Unavailable, Clock_Reversed, Ownership_Lost,
      Access_Refused, Access_Unknown, Unexpected_Output, Burst_Active);
   type Engine_Phase is
     (Dormant, Initial_Status, Send_Command, Stamp_Write, Wait_Input,
      Send_Address, Send_Value, Wait_Output, Read_Output, Confirm_Idle,
      Complete_Exchange);
   -- Fixed detached diagnostics only; no hardware access, clock sampling,
   -- command advancement, result acknowledgement or ownership transfer.
   type Transaction_Observation is record
      Stage, Last_Active : Engine_Phase := Dormant;
      Status : Outcome := Idle;
      Kind : Operation := Read_Cell;
      Cell : Cell_Offset := 0;
      Command : Byte := 0;
      Deadline, Last_Time, Last_Sample, Began, Wake : Microseconds := 0;
      Remaining : Work_Budget := 0;
      Has_Time, Sample_Available, Has_Began : Boolean := False;
      Status_Attempted, Status_Known : Boolean := False;
      Last_Status : Byte := 0;
      Status_Result : Access_Result := Refused;
      Effects_Possible, Recovery, Stopping : Boolean := False;
   end record;
   Input_Full : constant Byte := 16#02#;
   Output_Full : constant Byte := 16#01#;
   Burst_Mode : constant Byte := 16#10#;
   Event_Pending : constant Byte := 16#20#;
   Read_Command : constant Byte := 16#80#;
   Write_Command : constant Byte := 16#81#;
   Query_Command : constant Byte := 16#84#;
   Input_Assertion_Delay : constant Microseconds := 1;
end ACPI_EC_Types;
