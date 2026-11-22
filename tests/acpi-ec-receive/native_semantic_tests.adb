--  Semantic-only instantiation. Compile with -gnatc; never link or execute.
with ACPI_EC_Types;
with ACPI_EC_Native_Clock;
with ACPI_EC_Clock_Policy;
with ACPI_EC_Transactions;
with ACPI_EC_Registers;
with ACPI_EC_Endpoint;
with ACPI_EC_Native_Calls;
with ACPI_EC_Event_Types;
with ACPI_EC_Event_Publisher;
with ACPI_EC_Backend_Progress;
with ACPI_EC_Backend_Waits;
with ACPI_EC_Backend_Controller;
with ACPI_EC_Backend_Controller.With_Receive;
with CuBit.Messages;
with CuBit.Process_IDs;
procedure Native_Semantic_Tests is
   procedure Read
     (Register : ACPI_EC_Types.Read_Register; Value : out ACPI_EC_Types.Byte;
      Result : out ACPI_EC_Types.Access_Result) is
      pragma Unreferenced (Register);
   begin Value := 0; Result := ACPI_EC_Types.Refused; end Read;
   procedure Write
     (Register : ACPI_EC_Types.Write_Register; Value : ACPI_EC_Types.Byte;
      Result : out ACPI_EC_Types.Access_Result) is
      pragma Unreferenced (Register, Value);
   begin Result := ACPI_EC_Types.Refused; end Write;
   procedure Clock (Now : out ACPI_EC_Types.Microseconds; Available : out Boolean) is
   begin ACPI_EC_Native_Clock.Read (Now, Available); end Clock;
   function Owned return Boolean is (False);
   procedure Publish
     (Item : ACPI_EC_Event_Types.Notice;
      Result : out ACPI_EC_Event_Types.Publication) is
      pragma Unreferenced (Item);
   begin Result := ACPI_EC_Event_Types.Backpressure; end Publish;
   package Engine is new ACPI_EC_Transactions (Read, Write, Clock, Owned);
   package Registers is new ACPI_EC_Registers (Engine);
   package Calls is new ACPI_EC_Endpoint (Registers);
   package Native is new ACPI_EC_Native_Calls (Registers, Calls);
   Sender : constant CuBit.Messages.Process_ID :=
     CuBit.Process_IDs.From_Word (16#ABCD_1234_0000_0001#);
   Consumer : constant CuBit.Process_IDs.Process_ID :=
     CuBit.Process_IDs.From_Word (16#DCBA_4321_0000_0002#);
   package Events is new ACPI_EC_Event_Publisher (Registers, Publish, Consumer);
   package Progress is new ACPI_EC_Backend_Progress (Registers, Native, Events);
   use type Progress.Action_Kind;
   package Waits is new ACPI_EC_Backend_Waits (Registers, Progress, ACPI_EC_Native_Clock.Pause_Until);
   package Controller is new ACPI_EC_Backend_Controller
     (Registers, Native, Events, Progress, Waits, Clock, 100, 100, 2);
   package Receiver is new Controller.With_Receive;
   Receive_Result : Receiver.Report;
   Admission : Controller.Admission_Status;
   Submission : Registers.Submission;
   Managed : Controller.Call_Report;
   Available : Boolean;
   Wait_Result : Waits.Report;
   Remaining : ACPI_EC_Types.Work_Budget := 1;
   Pause : ACPI_EC_Clock_Policy.Pause_Report;
   Field : Native.Report;
   Tick : Progress.Report;
begin
   ACPI_EC_Native_Clock.Pause_Until (1, Remaining, Pause);
   Native.Begin_Call (Sender, CuBit.Messages.NULL_MESSAGE, 1, 1, Field);
   Waits.Advance (0, Remaining, Wait_Result);
   Controller.Begin_Call (Sender, CuBit.Messages.NULL_MESSAGE, Managed);
   Controller.Start_Query (1, Admission, Submission);
   Controller.Advance (Wait_Result, Available);
   Receiver.Turn (Receive_Result);
   Tick := Progress.Next (0);
   if Tick.Action = Progress.No_Work then Progress.Advance (0, Tick); end if;
end Native_Semantic_Tests;
