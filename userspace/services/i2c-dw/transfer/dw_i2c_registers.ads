pragma Ada_2022;
with DW_I2C_Transfer;
package DW_I2C_Registers with SPARK_Mode is
   package T renames DW_I2C_Transfer;
   subtype Word is T.Word;
   use type Word;
   subtype Byte is T.Byte;
   type Read_Register is (Component_Type, Component_Parameters, Component_Version,
      Raw_Interrupts, Abort_Source, TX_Level, RX_Level, Read_Data_Command);
   type Write_Register is (Write_Data_Command);
   type Register_Offset is range 0 .. 16#FC#;
   function Offset (ID : Read_Register) return Register_Offset;
   function Offset (ID : Write_Register) return Register_Offset;
   Component_Signature : constant Word := 16#4457_0140#;
   Abort_Interrupt : constant Word := 16#40#;
   Stop_Interrupt : constant Word := 16#200#;
   RX_Underflow_Interrupt : constant Word := 16#1#;
   RX_Overflow_Interrupt : constant Word := 16#2#;
   TX_Overflow_Interrupt : constant Word := 16#8#;
   FIFO_Error_Interrupts : constant Word :=
     RX_Underflow_Interrupt + RX_Overflow_Interrupt + TX_Overflow_Interrupt;
   type Access_Status is (Access_OK, Unavailable, Denied, Unknown_Effect);
   type Outcome is (Success, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
                    Register_Refused, Register_Indeterminate, Wrong_Component,
                    Invalid_FIFO_Level, FIFO_Error, Invalid_Action, Stale_Action);
   type Diagnostic is record
      Status : Outcome := Success;
      Register_ID : Read_Register := Component_Type;
      Bus_Status : Access_Status := Access_OK;
      Access_Attempted : Boolean := False;
      Value : Word := 0;
   end record;
   type Capabilities is record
      Ready : Boolean := False;
      TX_Depth, RX_Depth : T.FIFO_Depth := T.FIFO_Depth'First;
      Version : Word := 0;
   end record;
   type Sample is record
      Ready : Boolean := False;
      Hardware : T.Observation := (0, 0, False, False);
      Interrupt_Value, Abort_Value, TX_Value, RX_Value : Word := 0;
   end record;
   generic
      -- Static adapters bound to one authorized BAR; offsets are mapped inside
      -- the native owner, never accepted as arbitrary physical client addresses.
      -- Callbacks are bounded, non-reentrant and do not raise exceptions. Word
      -- values have the controller's native little-endian register interpretation.
      with procedure Clock (Now : out T.Tick; Available : out Boolean);
      with procedure Read (ID : Read_Register; Value : out Word;
                           Status : out Access_Status);
      with procedure Write (ID : Write_Register; Value : Word;
                            Status : out Access_Status);
   package Port with SPARK_Mode is
      type Access_Owner is limited private;
      procedure Probe (Deadline : T.Tick; Result : out Capabilities;
                       Report : out Diagnostic);
      -- Does not read CLR_* registers or DATA_CMD and cannot acknowledge an IRQ.
      -- Exclusive owner samples after completing its previous FIFO action.
      procedure Capture
        (TX_Depth, RX_Depth : T.FIFO_Depth; Deadline : T.Tick;
         Result : out Sample; Report : out Diagnostic);
      -- Bind one Access_Owner for the entire lifetime of ONE planner State.
      -- Even a refused/pre-deadline-failed attempt consumes its ticket. Never
      -- call Execute twice for the same action, or rebind/reset either owner.
      -- This is an internal owner API, not an exposed hardware capability.
      procedure Execute
        (Owner : in out Access_Owner; Item : T.Action; Data : Byte;
         Deadline : T.Tick; Read_Value : out Byte;
         Result : out T.Access_Result; Report : out Diagnostic);
      -- After a successful FIFO access Result remains Succeeded even when the
      -- post-access clock check fails. The owner must account that real effect
      -- once, then fail/quiesce its request; it must not retry on clock errors.
   private
      type Access_Owner is limited record
         Last_Ticket : T.Ticket := T.No_Ticket;
      end record;
   end Port;
end DW_I2C_Registers;
