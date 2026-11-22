pragma Ada_2022;
with I2C_Transfer_Wire; with I2C_Channel_Wire;
with HID_I2C_Descriptors; with HID_I2C_Channel_Lifetime; with CuBit.Process_IDs;
generic
   Maximum_Transfer : I2C_Transfer_Wire.Capacity;
   -- One session bound to the admitted endpoint/expected peer. Open may wait
   -- only down-layer and only until this explicit finite startup deadline.
   with procedure Open_Channel (Expected_Peer : CuBit.Process_IDs.Process_ID;
      Deadline : I2C_Transfer_Wire.Millisecond; Ready : out Boolean);
   with procedure Store_Write (Data : HID_I2C_Descriptors.Bytes; Stored : out Boolean);
   with procedure Load_Read (Data : out HID_I2C_Descriptors.Bytes; Loaded : out Boolean);
   with procedure Submit_Header (Header : I2C_Channel_Wire.Raw_Request;
      Token : I2C_Transfer_Wire.Word; Accepted : out Boolean);
   with procedure Poll_Answer (Token : I2C_Transfer_Wire.Word;
      Words : out I2C_Transfer_Wire.Completion_Words; Available, Ended : out Boolean);
   -- Available and Ended are mutually exclusive. Answers come only from the
   -- expected peer's channel mapping, copied before interpreting any field.
   with procedure Close_Channel;
   with function Retirement_Confirmed return Boolean;
   -- Close_Channel captures the typed receipt once. Its query is authoritative
   -- kernel retirement of that session's own region, not Is_Open=False.
package HID_I2C_Buffered_Client with SPARK_Mode is
   package W renames I2C_Transfer_Wire;
   package D renames HID_I2C_Descriptors;
   package P renames CuBit.Process_IDs;
   package Transactions is new HID_I2C_Channel_Lifetime (Submit_Header);
   use type Transactions.Phase, W.Count;
   Transfer_Capacity : constant W.Capacity := Maximum_Transfer;
   type Setup_Result is (Buffers_Ready, Already_Initialized, Client_Quarantined,
      Not_Idle, Invalid_Sender, Channel_Refused);
   type Start_Result is (Submitted, Request_Rejected, Client_Not_Ready, Client_Busy,
      Channel_Uncertain, Transaction_Refused);
   type Close_Result is (Closed, Still_Owned);
   type State is limited private;
   function Ready (S : State) return Boolean;
   function Observe (S : State) return Transactions.Report;
   function Succeeded (S : State) return Boolean;
   function Data_Ready (S : State) return Boolean;
   function Rejection (S : State) return Transactions.Reason;
   procedure Initialize (S : in out State; Sender : P.Process_ID;
      Deadline : W.Millisecond; Result : out Setup_Result);
   procedure Start (S : in out State; Write_Data : D.Bytes; Read_Count : W.Count;
      Token, Now : W.Word; Deadline : W.Millisecond; Clock_Available : Boolean;
      Result : out Start_Result);
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean);
   procedure Read (S : State; Data : out D.Bytes; OK : out Boolean);
   -- One bounded private snapshot, exact successful count only. No implicit
   -- acknowledgment; next Start remains blocked while the result is held.
   procedure Acknowledge (S : in out State)
     with Pre => Observe (S).At_Phase = Transactions.Finished;
   procedure Close (S : in out State; Result : out Close_Result);
   -- Idempotent close initiates teardown at most once, then polls its receipt.
   -- A timeout/transport fault also closes once and retains until confirmed.
   -- Exactly one State may claim the backend in this generic instance. A
   -- second State cannot open or close the first State's session.
   -- This client never reallocates, creates grants, clears a whole area,
   -- retries a command, reconnects, or changes its admitted endpoint/peer.
private
   type State is limited record
      Owner : Transactions.State;
      Sender : P.Process_ID := P.No_Process;
      Attempted, Configured, Closing, Close_Attempted, Close_Confirmed : Boolean := False;
      Expected_Reads : W.Count := 0;
      Last_Rejection : Transactions.Reason := Transactions.None;
   end record;
   function Ready (S : State) return Boolean is (S.Configured and then not S.Closing);
   function Observe (S : State) return Transactions.Report is (Transactions.Observe (S.Owner));
   function Succeeded (S : State) return Boolean is (Ready (S) and then Transactions.Succeeded (S.Owner));
   function Data_Ready (S : State) return Boolean is (Succeeded (S) and then S.Expected_Reads > 0);
   function Rejection (S : State) return Transactions.Reason is (S.Last_Rejection);
end HID_I2C_Buffered_Client;
