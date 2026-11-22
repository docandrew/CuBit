pragma Ada_2022;
with DW_I2C_Transfer;
package DW_I2C_Configuration with SPARK_Mode is
   package T renames DW_I2C_Transfer;
   subtype Word is T.Word;
   use type Word;
   type Register_ID is (Interrupt_Mask, Enable, Enable_Status, Controller_Status,
      TX_Level, RX_Level, Data_Command, Standard_High, Standard_Low, Fast_High,
      Fast_Low, SDA_Hold, Control, Target, TX_Threshold, RX_Threshold,
      Abort_Source, Clear_Interrupts);
   type Register_Offset is range 0 .. 16#9C#;
   function Offset (ID : Register_ID) return Register_Offset;
   type Speed_Mode is (Standard, Fast);
   type Address is range 0 .. 127;
   subtype Counter is T.Count range 1 .. 65_535;
   type Hold_Field is range 0 .. 16#FF_FFFF#;
   type Configuration is record
      Speed : Speed_Mode;
      Device : Address;
      High_Count, Low_Count : Counter;
      Program_Hold : Boolean;
      Hold_Count : Hold_Field;
      TX_Depth, RX_Depth : T.FIFO_Depth;
   end record;
   -- Counts must be derived/validated for the actual controller clock and
   -- electrical timing by the owner. No guessed bus-clock defaults here.
   type Stage is (Idle, Mask_IRQ, Disable_Controller, Wait_Disabled, Wait_Idle,
      Check_TX, Check_RX, Drain_RX, Set_High, Set_Low, Set_Hold, Set_Control,
      Set_Target, Set_TX_Threshold, Set_RX_Threshold, Copy_Abort, Clear_IRQ,
      Enable_Controller, Wait_Enabled, Ready, Failed);
   type Failure is (None, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Budget_Exhausted, Register_Refused, Register_Indeterminate, TX_Not_Empty,
      Invalid_RX_Level, Drain_Limit_Reached);
   type Access_Status is (Access_OK, Unavailable, Denied, Unknown_Effect);
   type Admission is (Accepted, Busy, Reset_Not_Established, Expired, Unsupported_Hold);
   Minimum_Hold_Version : constant Word := 16#3131_312A#;
   type Report is record
      At_Stage : Stage := Idle;
      Reason : Failure := None;
      Failed_At : Stage := Idle;
      Last_Register : Register_ID := Interrupt_Mask;
      Last_Value : Word := 0;
      Last_Status : Access_Status := Access_OK;
      Access_Attempted : Boolean := False;
      May_Be_Enabled : Boolean := True;
      Writes_Completed, Drained_Bytes, Steps_Used : T.Count := 0;
      Abort_Copied : Boolean := False;
      Abort_Value : Word := 0;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   procedure Start
     (S : in out State; Settings : Configuration; Reset_Completed : Boolean;
      Controller_Version : Word;
      Now, Deadline : T.Tick; Step_Budget : T.Limit; Result : out Admission);
   procedure Retire (S : in out State; Retired : out Boolean);
   -- Pure state retirement, not evidence about hardware. Only Ready can retire.
   -- The exclusive controller owner must first disable, verify idle/empty FIFOs
   -- and preserve/clear abort/status. Failed and unfinished setup never retire.
   generic
      -- Bounded, exclusive, non-reentrant static adapters for ONE admitted BAR.
      -- Read has side effects for Data_Command and Clear_Interrupts. Do not
      -- invoke the service from these callbacks; no exceptions may escape.
      with procedure Clock (Now : out T.Tick; Available : out Boolean);
      with procedure Read (ID : Register_ID; Value : out Word;
                           Status : out Access_Status);
      with procedure Write (ID : Register_ID; Value : Word;
                            Status : out Access_Status);
   procedure Step (S : in out State);
   -- One primitive register access per Step, no internal waits or retries.
   -- Owner schedules polling through its normal event loop with the SAME
   -- deadline. Only Wait_* and Check_RX may revisit reads; writes and
   -- destructive reads advance once or enter terminal failure. Neither a
   -- failed setup nor Ready can be restarted through Start on the same State.
   Master_Bit : constant Word := 16#1#;
   Standard_Speed_Bits : constant Word := 16#2#;
   Fast_Speed_Bits : constant Word := 16#4#;
   Restart_Enable_Bit : constant Word := 16#20#;
   Slave_Disable_Bit : constant Word := 16#40#;
   Enabled_Bit : constant Word := 16#1#;
   Activity_Bits : constant Word := 16#61#;
private
   type State is limited record
      Info : Report;
      Config : Configuration := (Standard, 0, 1, 1, False, 0, 1, 1);
      Last_Time, Expires : T.Tick := 0;
      Remaining : T.Count := 0;
   end record;
end DW_I2C_Configuration;
