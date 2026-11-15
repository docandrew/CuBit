pragma Ada_2022;
with Interfaces;
with ACPI_EC_Types;
with ACPI_EC_Transactions;
-- One local owner per independently admitted EC interface. Query and Field
-- requests share this engine, sequence and enclosing command guard. No IPC,
-- hardware authority, controller selection or address translation is created.
-- All actuals are bounded, normally returning and nonreentrant. Enter/Leave
-- use the SAME existing whole-Field/global-lock scope: a nested Field borrows
-- its enclosing hold, while a query opens its own command-wide frame.
generic
   with procedure Read (Register : ACPI_EC_Types.Read_Register;
      Value : out ACPI_EC_Types.Byte; Result : out ACPI_EC_Types.Access_Result);
   with procedure Write (Register : ACPI_EC_Types.Write_Register;
      Value : ACPI_EC_Types.Byte; Result : out ACPI_EC_Types.Access_Result);
   with procedure Clock (Now : out ACPI_EC_Types.Microseconds; Available : out Boolean);
   with procedure Enter_Command (For_Query, For_Write : Boolean;
      Deadline : ACPI_EC_Types.Microseconds; Accepted : out Boolean);
   -- False may retain a guard/lease. Quiet must independently observe the
   -- actual scope, including a failed Enter; it must not inspect our busy flag.
   with procedure Leave_Command (Released : out Boolean);
   with function Command_Quiet return Boolean;
   -- Actual exclusive ownership OR actually held required Global Lock. The
   -- Read/Write callbacks also check the live scope immediately before effect.
   with function Backing_Owned return Boolean;
package ACPI_Local_EC_Commands with SPARK_Mode is
   package T renames ACPI_EC_Types;
   type Token_ID is new Interfaces.Unsigned_64;
   No_Token : constant Token_ID := 0;
   type Phase is (Ready, Running, Result_Ready, Retained, Quarantined, Stopped);
   type Failure is (None, Entry_Refused, Engine_Refused, Cleanup_Refused);
   type Observation is record
      Stage : Phase := Ready;
      Token : Token_ID := No_Token;
      Kind : T.Operation := T.Read_Cell;
      Hardware : T.Outcome := T.Idle;
      Effects_Possible, Recovery, Guard_Held, Collected, Stopping : Boolean := False;
      Why : Failure := None;
   end record;
   type Completion is record
      Present : Boolean := False;
      Token : Token_ID := No_Token;
      Kind : T.Operation := T.Read_Cell;
      Hardware : T.Outcome := T.Idle;
      Value : T.Byte := 0;
      Effects_Possible, Recovery, Guard_Cleared, Stopping : Boolean := False;
   end record;
   type Start_Result is (Started, Busy, Admission_Stopped, Sequence_Exhausted,
      Invalid_Policy, Guard_Refused, Guard_Retained, Engine_Unavailable);
   procedure Start (Kind : T.Operation; Cell : T.Cell_Offset; Value : T.Byte;
      Deadline : T.Microseconds; Budget : T.Work_Budget;
      Result : out Start_Result; Token : out Token_ID);
   -- At most one original engine Step, retaining its deadline/work budget.
   -- No automatic resubmission, acknowledgement, release or query replay.
   procedure Advance;
   function Observe return Observation;
   function Engine_Observation return T.Transaction_Observation;
   function Wake_At return T.Microseconds;
   -- Exactly one result copy. Safe terminal exchanges acknowledge the engine
   -- BEFORE leaving the command guard. Any recovery keeps both owned; a failed
   -- guard release closes admission, even if later passive evidence is quiet.
   procedure Collect (Item : out Completion);
   procedure Request_Stop (Accepted : out Boolean);
   -- Passive observation only; never repeats release or resets the engine.
   -- A failed release may become quiescent through the existing scope owner's
   -- maintenance, but that never reopens command admission.
   procedure Reconcile;
   function Quiescent return Boolean;
private
   function Owns_Command return Boolean;
   package Engine is new ACPI_EC_Transactions (Read,Write,Clock,Owns_Command);
   View : Observation;
   Sequence : Token_ID := No_Token;
   Working : Boolean := False;
   Frame_Held : Boolean := False;
   Stopping : Boolean := False;
end ACPI_Local_EC_Commands;
