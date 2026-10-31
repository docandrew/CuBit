pragma Ada_2022;
with GPIO_Client_Controls;
with GPIO_Control_Wire;
with HID_I2C_Device;
with CuBit.Process_IDs;
package HID_GPIO_Input with SPARK_Mode is
   subtype Window is GPIO_Control_Wire.Millisecond range 1 .. GPIO_Control_Wire.Millisecond'Last;
   generic
      with package Controls is new GPIO_Client_Controls (<>);
      with package Device is new HID_I2C_Device (<>);
      Expected_Sender : CuBit.Process_IDs.Process_ID;
      Control_Window, Read_Window, Cleanup_Window : Window;
      with procedure Next_Token (Token : out GPIO_Control_Wire.Word; Available : out Boolean);
   package Owner is
      package W renames GPIO_Control_Wire;
      type Phase is (Cold, Arming, Armed, Reading, Acknowledging, Releasing, Stopped, Unresolved);
      type Failure is (None, Requested_Stop, Clock_Unavailable, Clock_Reversed,
         Time_Exhausted, Token_Unavailable, Submission_Refused, Control_Failed,
         Control_Uncertain, Control_Timed_Out, Device_Failed, Input_Refused, Cleanup_Refused);
      type Report is record
         At_Phase : Phase := Cold;
         Reason : Failure := None;
         Lease, Generation, Last_Acknowledged : W.Word := 0;
         Buffered_Lease, Buffered_Generation : W.Word := 0;
         Stopping, Release_Attempted : Boolean := False;
         Control : Controls.Report;
      end record;
      type State is limited private;
      function Observe (S : State) return Report;
      function Quiescent (S : State) return Boolean;
      function Control_Token (S : State) return W.Word;
      function Wake_Deadline (S : State; Now, Otherwise : W.Word) return W.Word;
      -- Live pending token only; Observe.Control may be a historical result.
      type Start_Result is (Started, Not_Ready, Already_Started, Refused);
      procedure Start (S : in out State; D : in out Device.State; Now : W.Word;
         Clock_Available : Boolean; Result : out Start_Result);
      procedure Step (S : in out State; D : in out Device.State; Now : W.Word; Clock_Available : Boolean);
      type Event_Result is (Buffered, Duplicate, Unrelated);
      procedure Receive_Event (S : in out State; Value : W.Event; Result : out Event_Result);
      procedure Receive_Control (S : in out State; Token : W.Word; Sender : CuBit.Process_IDs.Process_ID;
         Delivery : Controls.Delivery_Status; Label : W.Operation; Protocol : W.Word;
         Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean; Result : out Controls.Receive_Result);
      procedure Request_Stop (S : in out State; D : in out Device.State; Now : W.Word; Clock_Available : Boolean);
      -- Same admitted GPIO endpoint/pin as Device reset; Start only after reset
      -- release and report initialization. All I2C/GPIO work shares a global fresh
      -- token allocator and one cached clock. Outer loop steps Device, then Owner.
      -- Native caller authenticates publisher/badge/frame before Receive_Event.
      -- Buffer one early event across arm/ACK completion ordering. Lease epochs
      -- are monotonic within the bound GPIO process lifetime; stale events cannot
      -- replace a newer buffered lease. ACK only after Device's input read finishes.
      -- Stop suppresses reads/ACKs, drains pending control and releases a known
      -- lease once with its own fresh cleanup window. No timeout implies cancel.
      -- Uncertain control/release remains owned and needs supervision, no replay.
      -- Quiescent covers GPIO only; service must ALSO confirm Device.Quiescent
      -- and close/retire its I2C buffers before releasing other resources.
   private
      type Operation is (No_Operation, Arm_Operation, Ack_Operation, Release_Operation);
      type State is limited record
         Info : Report;
         Client : Controls.State;
         Op : Operation := No_Operation;
         Previous : W.Word := 0;
         Seen_Clock, Device_Stopped : Boolean := False;
      end record;
   end Owner;
end HID_GPIO_Input;
