pragma Ada_2022;
with Interfaces;
package DW_I2C_Transfer with SPARK_Mode is
   -- Pure single-owner planner. No MMIO, allocation, IPC or hardware authority.
   Maximum_Transfer_Bytes : constant := 65_535;
   Maximum_FIFO_Depth : constant := 256;
   type Count is range 0 .. Maximum_Transfer_Bytes;
   subtype Limit is Count range 1 .. Count'Last;
   type FIFO_Count is range 0 .. Maximum_FIFO_Depth;
   subtype FIFO_Depth is FIFO_Count range 1 .. FIFO_Count'Last;
   type Tick is new Interfaces.Unsigned_64;
   type Ticket is new Interfaces.Unsigned_64;
   No_Ticket : constant Ticket := 0;
   type Phase is (Idle, Active, Complete, Failed);
   type Failure is (None, Deadline_Expired, Clock_Reversed, Invalid_Observation,
                    Premature_Stop, Controller_Abort, Access_Failed,
                    Access_Indeterminate, Ticket_Exhausted);
   type Admission is (Accepted, Busy, Empty_Transfer, Capacity_Refused,
                      Deadline_Refused);
   type Action_Kind is (No_Action, Send_Command, Receive_Byte);
   type Action is record
      Kind : Action_Kind := No_Action;
      ID : Ticket := No_Ticket;
      Index : Count := 0;
      Read_Request : Boolean := False;
      Restart : Boolean := False;
      Stop : Boolean := False;
   end record;
   type Observation is record
      TX_Free : FIFO_Count;
      RX_Available : FIFO_Count;
      Stop_Detected : Boolean;
      Aborted : Boolean;
   end record;
   type Access_Result is (Succeeded, Refused, Indeterminate);
   type State is limited private;
   function Valid (S : State) return Boolean;
   function Current_Phase (S : State) return Phase;
   function Reason (S : State) return Failure;
   function Outstanding_Reads (S : State) return Count with Pre => Valid (S);
   function Sent (S : State) return Count with Pre => Valid (S);
   function Received (S : State) return Count;
   function Pending (S : State) return Boolean;
   procedure Submit
     (S : in out State; Write_Count, Read_Count : Count; Byte_Limit : Limit;
      TX_Depth, RX_Depth : FIFO_Depth; Now, Deadline : Tick;
      Result : out Admission)
     with Pre => Valid (S), Post => Valid (S);
   -- Poll only when there is no uncompleted action. Pending polls emit nothing.
   -- Each hardware observation must describe this controller, sampled after
   -- the preceding completed action. STOP is latched here before RX draining.
   procedure Poll
     (S : in out State; Now : Tick; Hardware : Observation; Next : out Action)
     with Pre => Valid (S), Post => Valid (S);
   -- Owner performs precisely one access per issued ticket, then completes it.
   -- Stale/duplicate completion changes nothing. Unknown access is terminal,
   -- never replayed. Send success means FIFO enqueue, not bus completion.
   procedure Finish_Access
     (S : in out State; ID : Ticket; Result : Access_Result;
      Matched : out Boolean)
     with Pre => Valid (S), Post => Valid (S);
   -- Trusted owner must disable/quiesce/drain the controller before retiring
   -- any terminal request, including failures with physical effects.
   procedure Retire (S : in out State; Controller_Quiescent : Boolean;
                     Retired : out Boolean)
     with Pre => Valid (S), Post => Valid (S);
   subtype Word is Interfaces.Unsigned_32;
   subtype Byte is Interfaces.Unsigned_8;
   Read_Command_Bit : constant Word := 16#100#;
   Stop_Command_Bit : constant Word := 16#200#;
   Restart_Command_Bit : constant Word := 16#400#;
   function Encode_Command (Item : Action; Data : Byte) return Word
     with Pre => Item.Kind = Send_Command;
private
   type State is limited record
      At_Phase : Phase := Idle;
      Error : Failure := None;
      Writes, Reads, Queued_Writes, Queued_Reads, Drained : Count := 0;
      TX_Size, RX_Size : FIFO_Depth := FIFO_Depth'First;
      Last_Time, Expires : Tick := 0;
      Stop_Seen : Boolean := False;
      Sequence : Ticket := No_Ticket;
      In_Flight : Action;
   end record;
end DW_I2C_Transfer;
