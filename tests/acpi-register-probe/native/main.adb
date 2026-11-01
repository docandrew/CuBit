pragma Ada_2022;
with Ada.Command_Line;
with Interfaces;
with Startup;
with CuBit.Monotonic;
with CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
with ACPI_Inventory_Transport;
with ACPI_Register_Transport;
with ACPI_Region_Protocol;
with ACPI_Timer_Probe;
with ACPI_Timer_Probe_Types;
procedure Main is
   use Interfaces;
   use ACPI_Timer_Probe_Types;
   package W renames ACPI_Region_Protocol;
   package Registers renames ACPI_Register_Transport;
   function Status_Name (Item : Outcome) return String is
     (case Item is
        when Not_Started => "NOT_STARTED",
        when Running => "RUNNING",
        when Metadata_Failed => "METADATA_FAILED",
        when Invalid_Inventory => "INVALID_INVENTORY",
        when Timer_Missing => "TIMER_MISSING",
        when Unsafe_Timer => "UNSAFE_TIMER",
        when Invalid_Deadline => "INVALID_DEADLINE",
        when Clock_Unavailable => "CLOCK_UNAVAILABLE",
        when Clock_Regressed => "CLOCK_REGRESSED",
        when Deadline_Expired => "DEADLINE_EXPIRED",
        when Read_Rejected => "READ_REJECTED",
        when Read_Failed => "READ_FAILED",
        when Read_Unknown => "READ_UNKNOWN",
        when Invalid_Value => "INVALID_VALUE",
        when Counter_Unchanged => "COUNTER_UNCHANGED",
        when Counter_Changed => "COUNTER_CHANGED");
   Maximum_Registers : constant := 32;
   Budget_Milliseconds : constant Unsigned_64 := 2_000;
   package Inventory is new ACPI_Inventory_Transport (Maximum_Registers);
   IO : Registers.Client.Context;
   Logger : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   procedure Log (Text : String; Success : Boolean) is
      Message : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
        (Text, (if Success then CuBit.Log_Records.Information else CuBit.Log_Records.Warning));
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint (Text & ASCII.LF);
      if Message.Success then CuBit.Logging.Emit (Logger, Message.Value, Submitted); end if;
   end Log;
   procedure Initialization_Failed (Reason : String) is
   begin
      Log ("acpi-register-probe: stage timer fail " & Reason, False);
      Log ("acpi-register-probe: bring-up reached timer (0/1)", False);
      CuBit.Messages.debugPrint ("TEST: FAIL acpi-register-probe" & ASCII.LF);
   end Initialization_Failed;
   procedure Load
     (Deadline : Time_Instant; Complete : out Boolean;
      Count : out Natural; Items : out Descriptions)
   is
      Snapshot : Inventory.Client.Snapshot;
   begin
      Inventory.Load (Startup.Registers, Deadline, Snapshot);
      Complete := Snapshot.Complete;
      Count := Natural (Snapshot.Count);
      Items := [others => (others => <>)];
      for I in 1 .. Count loop Items (I) := Snapshot.Items (I); end loop;
   end Load;
   procedure Read
     (Deadline : Time_Instant; Item : W.Register_Request;
      Status : out Read_Status; Value : out Unsigned_64)
   is
      Reply : Registers.Client.Result;
   begin
      Registers.Dispatch (IO, Deadline, Item, Reply);
      Value := Reply.Value;
      Status := (case Reply.Status is
        when Registers.Client.Completed => Completed,
        when Registers.Client.Rejected => Rejected,
        when Registers.Client.Failed_Completed => Failed,
        when Registers.Client.Indeterminate => Unknown);
   end Read;
   package Probe is new ACPI_Timer_Probe
     (Maximum_Registers, Load, Inventory.Read_Clock, Read);
   State : Probe.Context;
   Result : Report;
   Accepted : Boolean;
   Start : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
begin
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   Registers.Client.Initialize (IO, Startup.Registers, Accepted);
   if not Accepted then
      Initialization_Failed ("INITIALIZATION_REFUSED");
      return;
   elsif Start >= Unsigned_64'Last - Budget_Milliseconds then
      Initialization_Failed ("DEADLINE_OVERFLOW");
      return;
   end if;
   Probe.Run (State, Time_Instant (Start + Budget_Milliseconds), Result);
   declare
      Success : constant Boolean := Result.Status = Counter_Changed;
   begin
      Log ("acpi-register-probe: stage timer " & (if Success then "ok " else "fail ")
        & Status_Name (Result.Status) & " attempts=" & Result.Attempts'Image
        & " reads=" & Result.Completed_Reads'Image & " first=" & Result.First'Image
        & " last=" & Result.Last'Image, Success);
      Log ("acpi-register-probe: bring-up " & (if Success then "complete" else "reached timer (0/1)"), Success);
      CuBit.Messages.debugPrint
        ((if Success then "TEST: PASS" else "TEST: FAIL") & " acpi-register-probe" & ASCII.LF);
      if Success then Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success); end if;
   end;
end Main;
