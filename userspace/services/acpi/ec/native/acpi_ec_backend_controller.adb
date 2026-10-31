pragma Ada_2022;
with ACPI_Region_Protocol;
package body ACPI_EC_Backend_Controller is
   use type ACPI_EC_Types.Microseconds, ACPI_EC_Types.Work_Budget;
   use type Registers.Submission, Calls.Outcome;
   use type Interfaces.Unsigned_64;
   Remaining : ACPI_EC_Types.Work_Budget := 0;
   function Remaining_Pauses return ACPI_EC_Types.Work_Budget is (Remaining);
   procedure Deadline
     (Value : out ACPI_EC_Types.Microseconds; Status : out Admission_Status) is
      Now : ACPI_EC_Types.Microseconds;
      Available : Boolean;
   begin
      Value := 0;
      if Request_Window = 0 or else Request_Budget = 0 or else Pause_Budget = 0 then
         Status := Invalid_Policy; return;
      end if;
      Read_Clock (Now, Available);
      if not Available then Status := Clock_Unavailable;
      elsif Request_Window > ACPI_EC_Types.Microseconds'Last - Now then
         Status := Deadline_Overflow;
      else Value := Now + Request_Window; Status := Admitted;
      end if;
   end Deadline;
   procedure Begin_Call
     (Sender : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Result : out Call_Report) is
      Limit : ACPI_EC_Types.Microseconds;
      Reply : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Denial : constant ACPI_Region_Protocol.Packet :=
        (Label => ACPI_Region_Protocol.Operation_Reply,
         Data => [Interfaces.Unsigned_64 (ACPI_Region_Protocol.Outcome'Pos
           (ACPI_Region_Protocol.Denied)), 0, 0, 0], others => <>);
   begin
      Result := (others => <>);
      Deadline (Limit, Result.Status);
      if Result.Status /= Admitted then
         Reply.tag.label := Denial.Label;
         Reply.tag.length := Denial.Length;
         Reply.words := CuBit.Messages.MessageWords (Denial.Data);
         Result.Replied := Answer_Current (Sender, Reply) /= 0;
         return;
      end if;
      Calls.Begin_Call (Sender, Request, Limit, Request_Budget, Result.Field);
      Result.Replied := Result.Field.Delivered;
      if Result.Field.Status = Calls.Awaiting_Transaction then
         Remaining := Pause_Budget;
      else Result.Status := Refused;
      end if;
   end Begin_Call;
   procedure Start_Query
     (Delivery_Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Status : out Admission_Status; Result : out Registers.Submission) is
      Limit : ACPI_EC_Types.Microseconds;
   begin
      Result := Registers.Denied;
      Deadline (Limit, Status);
      if Status /= Admitted then return; end if;
      Events.Start_Query (Limit, Request_Budget, Delivery_Attempts, Result);
      if Result = Registers.Started then Remaining := Pause_Budget;
      else Status := Refused;
      end if;
   end Start_Query;
   procedure Advance (Result : out Waits.Report; Clock_Available : out Boolean) is
      Now : ACPI_EC_Types.Microseconds;
   begin
      Result := (others => <>);
      Read_Clock (Now, Clock_Available);
      if not Clock_Available then
         Registers.Request_Stop;
         Result.Stop_Requested := True;
         -- Zero is advisory only: the engine samples its actual clock again
         -- before any IO and retains its original deadline/work budget.
         Progress.Advance_Waiter (0, Result.Progress_Result);
      else Waits.Advance (Now, Remaining, Result);
      end if;
   end Advance;
end ACPI_EC_Backend_Controller;
