pragma Ada_2022;
package body ACPI_EC_Transactions with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   procedure Fail (Reason : Outcome; Force_Recovery : Boolean := False) with
     Pre => State = Running and then Output = 0
       and then Remaining <= Given_Budget
       and then Accesses <= Given_Budget - Remaining
       and then Reason not in Idle | Running | Succeeded | No_Event,
     Post => Coherent and then State_Of =
       (State_Of'Old with delta Status => Reason,
        Recovery => State_Of'Old.Recovery or State_Of'Old.Effects or Force_Recovery,
        Stamp_Pending => False)
   is
   begin
      State := Reason;
      Recovery := Recovery or Touched or Force_Recovery;
      Current := Dormant;
   end Fail;

   procedure Start
     (Kind : Operation; Cell : Cell_Offset; Value : Byte;
      Deadline : Microseconds; Budget : Work_Budget; Accepted : out Boolean)
   is
   begin
      Accepted := State = Idle and not Recovery;
      if not Accepted then
         return;
      end if;
      Action := Kind;
      Address := Cell;
      Input := Value;
      Output := 0;
      Staged_Output := 0;
      Limit := Deadline;
      Remaining := Budget;
      Given_Budget := Budget;
      Accesses := 0;
      Have_Time := False;
      Touched := False;
      Stopping := False;
      Earliest := 0;
      State := Running;
      Current := Initial_Status;
   end Start;

   procedure Step is
      Now : Microseconds;
      Available : Boolean;
      Value : Byte;
      Result : Access_Result;
      Register : Write_Register;
      Written : Byte;
      Following : Phase;
      Has_Ownership : Boolean;
   begin
      if State /= Running then
         return;
      end if;
      Clock (Now, Available);
      if not Available then
         Fail (Clock_Unavailable);
         return;
      elsif Have_Time and then Now < Previous then
         Fail (Clock_Reversed);
         return;
      end if;
      Have_Time := True;
      Previous := Now;
      if Now >= Limit then
         Fail (Deadline_Reached);
         return;
      elsif Remaining = 0 then
         Fail (Budget_Exhausted);
         return;
      end if;
      Remaining := Remaining - 1;
      Has_Ownership := Owned;
      if not Has_Ownership then
         Fail (Ownership_Lost);
         return;
      elsif Stopping and not Touched then
         State := Stopped;
         Current := Dormant;
         return;
      end if;
      if Current = Stamp_Write then
         --  This sample is obtained on a later invocation, after Write returned.
         if Now > Microseconds'Last - Input_Assertion_Delay then
            Fail (Deadline_Reached);
         else
            Earliest := Now + Input_Assertion_Delay;
            Current := After_Write;
         end if;
         return;
      elsif Now < Earliest then
         return;
      end if;
      case Current is
         when Initial_Status | Wait_Input | Wait_Output | Confirm_Idle =>
            Accesses := Accesses + 1;
            Read (Status_Register, Value, Result);
            if Result /= Completed then
               --  Unknown status access also invalidates interface ownership.
               Fail ((if Result = Refused then Access_Refused else Access_Unknown),
                     Force_Recovery => Result = Indeterminate);
               return;
            end if;
            if (Value and Burst_Mode) /= 0 then
               Fail (Burst_Active, Force_Recovery => True);
               return;
            end if;
            if Current = Initial_Status then
               if (Value and Output_Full) /= 0 then
                  Fail (Unexpected_Output, Force_Recovery => True);
               elsif (Value and Input_Full) /= 0 then
                  --  An inherited in-flight exchange is not established idle.
                  Recovery := True;
               else
                  Recovery := False;
                  if Action = Query_Event and (Value and Event_Pending) = 0 then
                     State := No_Event;
                     Current := Dormant;
                  else
                     Current := Send_Command;
                  end if;
               end if;
            elsif Current = Wait_Input then
               if (Value and Output_Full) /= 0 then
                  Fail (Unexpected_Output);
               elsif (Value and Input_Full) = 0 then
                  Current := Next_Input;
               end if;
            elsif Current = Wait_Output then
               if (Value and Output_Full) /= 0
                 and (Value and Input_Full) = 0
               then
                  Current := Read_Output;
               end if;
            elsif (Value and Output_Full) /= 0 then
               Fail (Unexpected_Output);
            elsif (Value and Input_Full) = 0 then
               Current := Complete_Exchange;
            end if;
         when Send_Command | Send_Address | Send_Value =>
            Register := Data_Register;
            Following := Wait_Input;
            if Current = Send_Command then
               Register := Command_Register;
               case Action is
                  when Read_Cell => Written := Read_Command;
                  when Write_Cell => Written := Write_Command;
                  when Query_Event => Written := Query_Command;
               end case;
               if Action = Query_Event then
                  Following := Wait_Output;
               else
                  Next_Input := Send_Address;
               end if;
            elsif Current = Send_Address then
               Written := Byte (Address);
               if Action = Read_Cell then
                  Following := Wait_Output;
               else
                  Next_Input := Send_Value;
               end if;
            else
               Written := Input;
               Next_Input := Confirm_Idle;
            end if;
            Accesses := Accesses + 1;
            Write (Register, Written, Result);
            if Result = Completed then
               Touched := True;
               After_Write := Following;
               Current := Stamp_Write;
            else
               if Result = Indeterminate then
                  Touched := True;
               end if;
               Fail ((if Result = Refused then Access_Refused else Access_Unknown));
            end if;
         when Read_Output =>
            Accesses := Accesses + 1;
            Read (Data_Register, Value, Result);
            if Result = Completed then
               Staged_Output := Value;
               Current := Confirm_Idle;
            else
               Fail ((if Result = Refused then Access_Refused else Access_Unknown));
            end if;
         when Complete_Exchange =>
            Output := Staged_Output;
            State := Succeeded;
            Current := Dormant;
         when Dormant | Stamp_Write => null;
      end case;
   end Step;

   procedure Request_Stop is
   begin
      if State = Running then
         Stopping := True;
      end if;
   end Request_Stop;
   function Status return Outcome is (State);
   function Result_Byte return Byte is (Output);
   function Effects_Possible return Boolean is (Touched);
   function Needs_Recovery return Boolean is (Recovery);
   function Stop_Requested return Boolean is (Stopping);
   function Wake_At return Microseconds is (Earliest);
   procedure Acknowledge (Accepted : out Boolean) is
   begin
      Accepted := State /= Running and State /= Idle and not Recovery;
      if Accepted then
         State := Idle;
         Output := 0;
      end if;
   end Acknowledge;
end ACPI_EC_Transactions;
