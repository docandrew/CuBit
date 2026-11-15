pragma Ada_2022;
with Interfaces;
with CuBit.Power_Event_Exchange;
-- Local S5 final-entry sequencer. No AML, IPC, address, handle or caller-
-- supplied register identifier. The native owner resolves these named actions
-- through its already-admitted controls; completion evidence is trusted local
-- driver evidence, never a message word. No retries or rollback.
package ACPI_Sleep_Entry with SPARK_Mode is
   package E renames CuBit.Power_Event_Exchange;
   subtype Tick is Interfaces.Unsigned_64;
   subtype Control_Word is Interfaces.Unsigned_16;
   type Register_Model is (No_Registers, PM1A_Only, PM1A_And_B, Hardware_Reduced);
   type Action is (Mask_GPEs, Clear_GPEs, Clear_Wake,
      Read_A, Read_B, Stage_A, Stage_B, Enable_A, Enable_B, Reduced_Enable);
   type Command is record
      Kind : Action := Mask_GPEs;
      Value : Control_Word := 0;
   end record;
   type Completion is (Completed, Not_Attempted, Indeterminate);
   type Outcome is (Fresh, Ready, Awaiting_Completion, Settling,
      Not_Prepared, Missing_Registers, Invalid_Window, Owner_Unsafe,
      Clock_Unavailable, Clock_Reversed, Window_Expired,
      Transfer_Refused, Transfer_Indeterminate, Still_Running);
   Maximum_Commands : constant := 9;
   subtype Command_Count is Natural range 0 .. Maximum_Commands;
   type Report is record
      Status : Outcome := Fresh;
      At_Action : Action := Mask_GPEs;
      Offered, Confirmed_Writes : Command_Count := 0;
      Enable_Offered : Boolean := False;
   end record;
   type State is limited private with Default_Initial_Condition => Valid (State);
   function Valid (Item : State) return Boolean;
   function Observe (Item : State) return Report with Pre => Valid (Item);
   -- Called after authority, request shape and owned prepared-copy validation.
   -- Latches even a refused attempt; another Start cannot reset this owner.
   procedure Start (Item : in out State; Types_Copied : Boolean;
      Registers : Register_Model; Type_A, Type_B : E.Sleep_Type;
      Owner_Ready, Clock_Available : Boolean; Now, Deadline : Tick;
      Result : out Report) with Pre => Valid (Item), Post => Valid (Item);
   -- Offers at most one action, once. While awaiting its confirmation it does
   -- not reoffer it. During settle it checks time without issuing commands.
   procedure Take (Item : in out State; Owner_Ready, Clock_Available : Boolean;
      Now : Tick; Value : out Command; Available : out Boolean; Result : out Report)
     with Pre => Valid (Item), Post => Valid (Item);
   -- Exactly one completion for the outstanding action. Read_Value is used
   -- only for successful Read_A/Read_B. A refusal/unknown result is terminal.
   procedure Confirm (Item : in out State; Status : Completion;
      Read_Value : Control_Word; Clock_Available : Boolean; Now : Tick;
      Result : out Report) with Pre => Valid (Item), Post => Valid (Item);
private
   use type Tick;
   Settle_Microseconds : constant Tick := Tick (E.Settle_Milliseconds) * 1_000;
   type State is limited record
      Info : Report;
      Model : Register_Model := No_Registers;
      A, B : E.Sleep_Type := 0;
      Saved_A, Saved_B : Control_Word := 0;
      Last_Time, Until_Time, Enable_Time : Tick := 0;
   end record;
end ACPI_Sleep_Entry;
