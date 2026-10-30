pragma Ada_2022;
with DW_I2C_Transfer;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset;
with Intel_LPSS_I2C_Profile;
generic
   Capacity : DW_I2C_Transfer.Limit;
   with procedure Clock
     (Now : out DW_I2C_Transfer.Tick; Available : out Boolean);
   with procedure Reset_Read
     (ID : Intel_LPSS_I2C_Reset.Register_ID; Value : out Intel_LPSS_I2C_Reset.Word;
      Status : out Intel_LPSS_I2C_Reset.Access_Status);
   with procedure Reset_Write
     (ID : Intel_LPSS_I2C_Reset.Register_ID; Value : Intel_LPSS_I2C_Reset.Word;
      Status : out Intel_LPSS_I2C_Reset.Access_Status);
   with procedure Config_Read
     (ID : DW_I2C_Configuration.Register_ID; Value : out DW_I2C_Configuration.Word;
      Status : out DW_I2C_Configuration.Access_Status);
   with procedure Config_Write
     (ID : DW_I2C_Configuration.Register_ID; Value : DW_I2C_Configuration.Word;
      Status : out DW_I2C_Configuration.Access_Status);
   with procedure Data_Read
     (ID : DW_I2C_Registers.Read_Register; Value : out DW_I2C_Registers.Word;
      Status : out DW_I2C_Registers.Access_Status);
   with procedure Data_Write
     (ID : DW_I2C_Registers.Write_Register; Value : DW_I2C_Registers.Word;
      Status : out DW_I2C_Registers.Access_Status);
package DW_I2C_Owner with SPARK_Mode is
   package T renames DW_I2C_Transfer;
   package R renames DW_I2C_Registers;
   package C renames DW_I2C_Configuration;
   package L renames Intel_LPSS_I2C_Reset;
   package P renames Intel_LPSS_I2C_Profile;
   -- Work steps are independent of byte counts: each read byte needs a
   -- command and a receive action, plus configuration/quiescence work.
   type Work_Count is range 0 .. 2 ** 32 - 1;
   subtype Work_Limit is Work_Count range 1 .. Work_Count'Last;
   type Bytes is array (T.Count range <>) of T.Byte;
   type Phase is (Cold, Resetting, Probing, Mask_Interrupts, Prepare_Configuration, Configuring, Disable_Controller,
      Wait_Disabled, Verify_Idle, Verify_TX, Verify_RX, Copy_Abort, Clear_Status, Available,
      Enable_Controller, Wait_Enabled, Transferring, Completion_Ready, Faulted);
   type Failure is (None, Timing_Refused, Reset_Failed, Probe_Failed,
      Configuration_Failed, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Budget_Exhausted, Register_Refused, Register_Indeterminate,
      Transfer_Failed, Controller_Not_Quiescent, Abort_During_Quiescence, Accounting_Failed);
   type Admission is (Accepted, Busy, Invalid_Resource, Invalid_Timing,
      Clock_Refused, Expired, Empty_Transfer, Capacity_Refused, Tickets_Exhausted);
   type Startup is record
      Vendor, Device : P.PCI_ID;
      BAR_Base, Window_Bytes : L.Physical_Address;
      Reset_Budget, Configuration_Budget : T.Limit;
      Owner_Budget : Work_Limit;
   end record;
   type Completion is record
      ID : T.Ticket := T.No_Ticket;
      Successful : Boolean := False;
      Reason : Failure := None;
      Write_Count, Read_Count : T.Count := 0;
      Effects_Possible : Boolean := False;
   end record;
   type Report is record
      At_Phase : Phase := Cold;
      Failed_At : Phase := Cold;
      Reason : Failure := None;
      Completion_Pending : Boolean := False;
      Completion_Data : Completion;
      Reset : L.Report;
      Configuration : C.Report;
      Port_Status : R.Diagnostic;
      Engine_Phase : T.Phase := T.Idle;
      Engine_Reason : T.Failure := T.None;
      Last_Owner_Register : C.Register_ID := C.Enable;
      Last_Owner_Value : T.Word := 0;
      Last_Owner_Status : C.Access_Status := C.Access_OK;
      Owner_Access_Attempted : Boolean := False;
      Controller_May_Be_Enabled : Boolean := True;
      Startup_Effects_Possible : Boolean := False;
      Startup_Value_Available : Boolean := False;
      Startup_Last_Value : T.Word := 0;
      -- Sticky across all startup steps, including post-write clock failure.
      -- Separate from per-request completion effects; no reply may infer clean
      -- startup solely from the last access status.
      Cleanup_Abort_Value : T.Word := 0;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   procedure Start (S : in out State; Parameters : Startup; Result : out Admission);
   procedure Advance (S : in out State);
   -- Non-blocking owner for per-request admitted targets on ONE exclusively owned
   -- controller, initially using bounded polling. Startup touches no target or
   -- timing registers; successful requests configure only after disabled/idle
   -- verification. Caller authenticates channel badge before supplying target
   -- and speed. Native loop schedules Advance;
   -- this package never waits, calls another service, or publishes input itself.
   procedure Submit
     (S : in out State; Target : C.Address; Speed_Limit : P.Hertz;
      Write_Data : Bytes; Read_Count : T.Count;
      Deadline : T.Tick; Step_Budget : Work_Limit;
      ID : out T.Ticket; Result : out Admission);
   type Collection is (Collected, Not_Ready, Insufficient_Capacity);
   procedure Take_Completion
     (S : in out State; Read_Data : out Bytes; Item : out Completion;
      Required : out T.Count; Result : out Collection);
   -- Failed/short collection does not replay IO or consume the owned result.
   -- Success disables/verifies the controller before retiring planner state.
   -- Failure leaves Faulted even after collection; supervisor recovery is needed.
   -- Callbacks must be bounded, ordered, exclusive, non-reentrant/non-raising.
   -- Native framing, expected-owner grants, authorization and reply tokens remain
   -- outside this internal owner. No client chooses BAR, target or raw registers.
private
   package Port is new R.Port (Clock, Data_Read, Data_Write);
   type State is limited record
      Info : Report;
      Reset_State : L.State;
      Config_State : C.State;
      Engine : T.State;
      Access_Owner : Port.Access_Owner;
      Capabilities : R.Capabilities;
      Timing : P.Timing;
      Vendor, Device : P.PCI_ID := 0;
      Target : C.Address := 0;
      Write_Buffer, Read_Buffer : Bytes (1 .. Capacity) := [others => 0];
      Writes, Reads : T.Count := 0;
      Deadline, Last_Time : T.Tick := 0;
      Remaining : Work_Count := 0;
      Config_Budget : T.Limit := T.Limit'First;
      Sequence : T.Ticket := T.No_Ticket;
      Request_Active : Boolean := False;
   end record;
end DW_I2C_Owner;
