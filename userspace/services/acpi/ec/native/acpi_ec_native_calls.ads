pragma Ada_2022;
with Interfaces;
with ACPI_EC_Registers;
with ACPI_EC_Endpoint;
with ACPI_EC_Types;
with CuBit.Messages;
with CuBit.Handles.IPC;
--  Privileged backend process only, not the AML process. Serialize all calls
--  with the concrete catalog/event owner. Sender/Request must be the exact
--  authenticated kernel receive; deadlines/budgets are backend policy.
generic
   with package Registers is new ACPI_EC_Registers (<>);
   with package Calls is new ACPI_EC_Endpoint (Registers);
   with function Save return CuBit.Handles.IPC.Saved_Reply
     is CuBit.Handles.IPC.Save_Reply;
   with function Answer_Saved
     (Item : CuBit.Handles.Reply; Msg : CuBit.Messages.Message) return Boolean
     is CuBit.Handles.IPC.Answer;
   with function Answer_Current
     (Sender : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message)
      return Interfaces.Unsigned_64 is CuBit.Messages.reply;
package ACPI_EC_Native_Calls is
   type Outcome is (Idle, Awaiting_Transaction, Immediate_Reply,
                    Completion_Reply, Save_Refused, Owner_Fault, Correlation_Fault);
   type Report is record
      Status : Outcome := Idle;
      Delivered : Boolean := False;
      Token : Registers.Operation_ID := 0;
      Hardware : ACPI_EC_Types.Outcome := ACPI_EC_Types.Idle;
      Effects_Possible, Recovery_Retained : Boolean := False;
   end record;
   --  Hardware/Effects_Possible describe an attributable completion only.
   --  Recovery_Retained always reflects the catalog's independent recovery
   --  obligation, including when there is no pending saved reply.
   --  Retains one saved reply before Begin_Call can start a transaction. Busy
   --  calls receive an immediate denial without replacing the retained reply.
   --  Save refusal performs no submission or hardware access.
   procedure Begin_Call
     (Sender : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Deadline : ACPI_EC_Types.Microseconds; Budget : ACPI_EC_Types.Work_Budget;
      Result : out Report);
   --  One bounded transaction Step, then at most one attributable completion
   --  reply. No polling loop, sleeping or synchronous peer callback here.
   procedure Advance (Result : out Report);
   function Pending return Boolean;
   function Faulted return Boolean;
   --  Save/answer callbacks return normally and never reenter. Answer consumes
   --  its saved authority even on delivery failure, matching the runtime API.
   --  Never retry a lost reply or clear the catalog's hardware recovery state.
end ACPI_EC_Native_Calls;
