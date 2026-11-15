pragma Ada_2022;
with Interfaces; with ACPI_Sleep_Entry;
generic
   type Service_State (<>) is limited private;
   type Reply_Token is private;
   No_Reply : Reply_Token;
   with procedure Read_Prepared (Service : Service_State; Available : out Boolean;
      Registers : out ACPI_Sleep_Entry.Register_Model;
      A, B : out ACPI_Sleep_Entry.E.Sleep_Type);
   with procedure Consume_Prepared;
   with function Owner_Ready (Service : Service_State) return Boolean;
   with procedure Read_Clock (Microseconds : out Interfaces.Unsigned_64; Available : out Boolean);
   -- IPC wait clock is sampled separately: a fallback microsecond clock can
   -- have another epoch. Never cast one absolute deadline into the other.
   with procedure Read_Wait_Clock (Milliseconds : out Interfaces.Unsigned_64; Available : out Boolean);
   with procedure Save_Current (Token : out Reply_Token; Saved : out Boolean);
   with procedure Answer_Saved (Token : in out Reply_Token; ID : Interfaces.Unsigned_64;
      Status : ACPI_Sleep_Entry.E.Enter_Status; Delivered : out Boolean);
   with procedure Execute_Command (Command : ACPI_Sleep_Entry.Command;
      Deadline_Microseconds : Interfaces.Unsigned_64; Output : out ACPI_Sleep_Entry.Control_Word;
      Status : out ACPI_Sleep_Entry.Completion);
-- Serialized local S5 owner. Callbacks return normally, never reenter or
-- revoke a borrowed owner. Saved authority is consumed even for a dead caller.
-- No callback invokes AML; only copied integers and named hardware actions.
package ACPI_Power_Entry with SPARK_Mode is
   package S renames ACPI_Sleep_Entry; package E renames S.E;
   subtype Word is Interfaces.Unsigned_64;
   No_Deadline : constant Word := Word'Last;
   type Request is record
      Stamp, Length, ID, State, Confirm, Unused : Word := 0;
      Flags : Interfaces.Unsigned_8 := 0;
      Reserved : Interfaces.Unsigned_16 := 0;
   end record;
   type Response is record ID : Word := 0; Status : E.Enter_Status := E.Not_Prepared; end record;
   type Phase is (Fresh, Running, Settling, Reply_Pending, Finished, Reply_Contract_Unknown);
   type Delivery is (No_Answer, Answered, Caller_Gone);
   type Report is record
      Stage : Phase := Fresh;
      Reply : Response;
      Sequence : S.Report;
      Uncertain_Effect, Attempted : Boolean := False;
      Last_Delivery : Delivery := No_Answer;
   end record;
   type State is limited private;
   function Observe (Item : State) return Report;
   function Runnable (Item : State) return Boolean;
   function Blocks_Runtime (Item : State) return Boolean;
   function Quiet (Item : State) return Boolean;
   function Wake_Deadline_Milliseconds (Item : State) return Word;
   procedure Enter (Item : in out State; Service : Service_State; Value : Request;
      Reply : out Response; Deferred : out Boolean);
   procedure Advance (Item : in out State; Service : Service_State; Result : out Delivery);
   procedure Begin_Stop (Item : in out State);
private
   type State is limited record
      Info : Report;
      Sequence : S.State;
      Token : Reply_Token := No_Reply;
      Busy, Stopping, Clock_Seen, Settle_Clock_Valid : Boolean := False;
      Last_Microseconds, Last_Milliseconds, Until_Microseconds, Until_Milliseconds, Settle_Milliseconds : Word := 0;
   end record;
end ACPI_Power_Entry;
