pragma Ada_2022;
with ACPI_EC_Types;
--  Trusted backend internals: this API conveys no hardware authority. Instantiate
--  once per independently admitted EC interface, and serialize all entry points.
--  Owned must mean exclusive dedicated ownership or an actually held Global Lock.
--  Callbacks are bounded, non-reentrant and truthful about unknown I/O outcomes.
--  Clock returns a fresh monotonic microsecond sample, never a cached pre-I/O time.
generic
   with procedure Read
     (Register : ACPI_EC_Types.Read_Register;
      Value : out ACPI_EC_Types.Byte;
      Result : out ACPI_EC_Types.Access_Result);
   with procedure Write
     (Register : ACPI_EC_Types.Write_Register;
      Value : ACPI_EC_Types.Byte;
      Result : out ACPI_EC_Types.Access_Result);
   with procedure Clock
     (Now : out ACPI_EC_Types.Microseconds; Available : out Boolean);
   with function Owned return Boolean;
package ACPI_EC_Transactions with SPARK_Mode, Initial_Condition => Coherent is
   use ACPI_EC_Types;
   pragma Unevaluated_Use_Of_Old (Allow);
   type State_View is record
      Status : Outcome;
      Kind : Operation;
      Cell : Cell_Offset;
      Input, Output : Byte;
      Effects, Recovery, Stopping, Has_Time, Stamp_Pending : Boolean;
      Deadline, Last_Time, Wake : Microseconds;
      Initial_Budget, Remaining, Accesses : Work_Budget;
   end record with Ghost;
   function State_Of return State_View with Ghost;
   function Coherent return Boolean with Ghost;
   function Start_Transition
     (Before, After : State_View; Kind : Operation; Cell : Cell_Offset;
      Value : Byte; Deadline : Microseconds; Budget : Work_Budget;
      Accepted : Boolean) return Boolean with Ghost, Global => null;
   function Step_Transition (Before, After : State_View) return Boolean
     with Ghost, Global => null;
   function Stop_Transition (Before, After : State_View) return Boolean
     with Ghost, Global => null;
   function Acknowledge_Transition
     (Before, After : State_View; Accepted : Boolean) return Boolean
     with Ghost, Global => null;
   --  Singleton owner prevents overlapping transactions within this instance.
   procedure Start
     (Kind : Operation; Cell : Cell_Offset; Value : Byte;
      Deadline : Microseconds; Budget : Work_Budget; Accepted : out Boolean)
     with Pre => Coherent, Post => Coherent and then
       Start_Transition (State_Of'Old, State_Of, Kind, Cell, Value, Deadline, Budget, Accepted);
   procedure Step with Pre => Coherent, Post => Coherent and then
     Step_Transition (State_Of'Old, State_Of);
   --  Before command submission, stop terminates without I/O; afterwards it
   --  drains the exchange within its original deadline and budget.
   procedure Request_Stop with Pre => Coherent, Post => Coherent and then
     Stop_Transition (State_Of'Old, State_Of);
   function Status return Outcome;
   function Result_Byte return Byte;
   function Effects_Possible return Boolean;
   function Needs_Recovery return Boolean;
   function Stop_Requested return Boolean;
   function Wake_At return Microseconds;
   --  Faulted/uncertain interfaces cannot be reset by acknowledging a result.
   procedure Acknowledge (Accepted : out Boolean) with Pre => Coherent,
     Post => Coherent and then Acknowledge_Transition (State_Of'Old, State_Of, Accepted);
private
   type Phase is
     (Dormant, Initial_Status, Send_Command, Stamp_Write, Wait_Input,
      Send_Address, Send_Value, Wait_Output, Read_Output, Confirm_Idle,
      Complete_Exchange);
   Current : Phase := Dormant;
   After_Write : Phase := Dormant;
   State : Outcome := Idle;
   Action : Operation := Read_Cell;
   Address : Cell_Offset := 0;
   Input : Byte := 0;
   Output : Byte := 0;
   Staged_Output : Byte := 0;
   Limit : Microseconds := 0;
   Previous : Microseconds := 0;
   Earliest : Microseconds := 0;
   Remaining : Work_Budget := 0;
   Have_Time : Boolean := False;
   Touched : Boolean := False;
   Recovery : Boolean := False;
   Stopping : Boolean := False;
   Next_Input : Phase := Dormant;
   Given_Budget : Work_Budget := 0 with Ghost;
   Accesses : Work_Budget := 0 with Ghost;
   function State_Of return State_View is
     ((Status => State, Kind => Action, Cell => Address, Input => Input,
       Output => Output, Effects => Touched, Recovery => Recovery,
       Stopping => Stopping, Has_Time => Have_Time,
       Stamp_Pending => Current = Stamp_Write, Deadline => Limit,
       Last_Time => Previous, Wake => Earliest, Initial_Budget => Given_Budget,
       Remaining => Remaining, Accesses => Accesses));
   function Coherent return Boolean is
     ((State = Running) = (Current /= Dormant)
      and then (if State = Idle then not Recovery)
      and then (if State /= Succeeded then Output = 0)
      and then (if State = Succeeded then Touched and not Recovery
                and Have_Time and Previous < Limit)
      and then (if State = No_Event then not Touched and not Recovery)
      and then (if State = Running and Touched then not Recovery)
      and then (if Current in Initial_Status | Send_Command then not Touched)
      and then (if Current = Send_Command then not Recovery)
      and then Remaining <= Given_Budget
      and then Accesses <= Given_Budget - Remaining
      and then (if Current not in Dormant | Initial_Status | Send_Command then Touched)
      and then (if Current = Stamp_Write then After_Write in Wait_Input | Wait_Output)
      and then (if Current = Wait_Input or (Current = Stamp_Write and After_Write = Wait_Input) then Next_Input in Send_Address | Send_Value | Confirm_Idle)
      and then (if (Current = Wait_Input or (Current = Stamp_Write and After_Write = Wait_Input))
        and Next_Input = Send_Address then Action /= Query_Event)
      and then (if (Current = Wait_Input or (Current = Stamp_Write and After_Write = Wait_Input))
        and Next_Input = Send_Value then Action = Write_Cell)
      and then (if Current in Send_Address then Action /= Query_Event)
      and then (if Current = Send_Value then Action = Write_Cell)
      and then (if Current in Wait_Output | Read_Output
        or (Current = Stamp_Write and After_Write = Wait_Output) then Action /= Write_Cell));
   function Start_Transition
     (Before, After : State_View; Kind : Operation; Cell : Cell_Offset;
      Value : Byte; Deadline : Microseconds; Budget : Work_Budget;
      Accepted : Boolean) return Boolean is
     (Accepted = (Before.Status = Idle and not Before.Recovery)
      and then (if Accepted then After =
        (Status => Running, Kind => Kind, Cell => Cell, Input => Value,
         Output => 0, Effects => False, Recovery => Before.Recovery,
         Stopping => False, Has_Time => False, Stamp_Pending => False, Deadline => Deadline,
         Last_Time => Before.Last_Time, Wake => 0, Initial_Budget => Budget,
         Remaining => Budget, Accesses => 0)
       else After = Before));
   function Step_Transition (Before, After : State_View) return Boolean is
     (After.Kind = Before.Kind and then After.Cell = Before.Cell
      and then After.Input = Before.Input and then After.Deadline = Before.Deadline
      and then After.Initial_Budget = Before.Initial_Budget
      and then After.Stopping = Before.Stopping
      and then After.Remaining <= Before.Remaining
      and then After.Accesses >= Before.Accesses
      and then (if Before.Effects then After.Effects)
      and then (if Before.Status = Running and After.Status = Running then
        Before.Remaining > 0 and After.Remaining = Before.Remaining - 1)
      and then Natural (After.Accesses) <= Natural (Before.Accesses) + 1
      and then (if After.Accesses > Before.Accesses then
        After.Has_Time and then After.Last_Time >= Before.Wake
        and then After.Last_Time < Before.Deadline)
      and then (if Before.Stamp_Pending then After.Accesses = Before.Accesses)
      and then (if Before.Stamp_Pending and After.Status = Running then
        After.Has_Time and then After.Last_Time < Microseconds'Last
        and then After.Wake = After.Last_Time + Input_Assertion_Delay
        and then not After.Stamp_Pending)
      and then (if After.Stamp_Pending and not Before.Stamp_Pending then
        Natural (After.Accesses) = Natural (Before.Accesses) + 1)
      and then (if Before.Has_Time then After.Has_Time and then After.Last_Time >= Before.Last_Time)
      and then (if Before.Status /= Running then After = Before));
   function Stop_Transition (Before, After : State_View) return Boolean is
     (After = (Before with delta Stopping =>
       (if Before.Status = Running then True else Before.Stopping)));
   function Acknowledge_Transition
     (Before, After : State_View; Accepted : Boolean) return Boolean is
     (Accepted = (Before.Status not in Running | Idle and not Before.Recovery)
      and then (if Accepted then After = (Before with delta Status => Idle, Output => 0)
                else After = Before));
end ACPI_EC_Transactions;
