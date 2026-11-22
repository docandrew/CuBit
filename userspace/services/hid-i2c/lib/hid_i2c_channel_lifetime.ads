pragma Ada_2022;
with I2C_Channel_Wire; with I2C_Transfer_Wire; with CuBit.Process_IDs;
generic
   -- False guarantees the ring entry was not published/accepted. Doorbell
   -- failure after publishing must still return True; it is not cancellation.
   with procedure Submit (Header : I2C_Channel_Wire.Raw_Request;
                          Token : I2C_Transfer_Wire.Word; Accepted : out Boolean);
package HID_I2C_Channel_Lifetime with SPARK_Mode is
   package W renames I2C_Transfer_Wire;
   package CW renames I2C_Channel_Wire;
   package P renames CuBit.Process_IDs;
   type Phase is (Idle, In_Flight, Retiring, Finished);
   type Reason is (None, Invalid_Request, Submission_Refused, Transfer_Complete,
      Clock_Unavailable, Clock_Reversed, Deadline_Expired, Transfer_Refused,
      Malformed_Completion, Inconsistent_Completion, Transport_Failed);
   type Report is record
      At_Phase : Phase := Idle;
      Outcome : Reason := None;
      Token : W.Word := 0;
      Payload_Released, Have_Completion, Effects_Possible, Trust_Lost : Boolean := False;
      Completion : W.Completion := (W.Invalid_Frame, 0, False, False, 0, 0);
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   function Succeeded (S : State) return Boolean;
   type Starting is (Started, Rejected, Already_Owned, Not_Submitted);
   procedure Start
     (S : in out State; Item : CW.Request; Available : W.Capacity;
      Expected_Sender : P.Process_ID; Token, Now : W.Word; Clock_Available : Boolean;
      Result : out Starting);
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean);
   type Receiving is (Unrelated, Wrong_Sender, Not_Awaiting, Released, Quarantined);
   procedure Receive
     (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Label : W.Operation; Protocol : W.Word; Words : W.Message_Words;
      Now : W.Word; Clock_Available : Boolean; Result : out Receiving);
   procedure Transport_Lost (S : in out State);
   type Confirming is (Unrelated, Wrong_Sender, Not_Retiring, Pending, Released);
   procedure Confirm_Retirement
     (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Confirmed : Boolean; Result : out Confirming);
   -- Native adapter supplies Confirmed only from the typed kernel retirement
   -- receipt for this session's closed own region, never from Is_Open=False,
   -- a successful revoke request, a local timer or a peer-supplied claim.
   -- This releases ownership after failure, without synthesizing read data.
   procedure Acknowledge (S : in out State)
     with Pre => Observe (S).At_Phase = Finished and then Observe (S).Payload_Released;
   -- A valid matching completion carries the verified server's quiescence
   -- guarantee. Its sender must be the peer whose read-only channel mapping
   -- was acquired at open. Metadata is copied before this validation.
   -- Late completion releases ownership but cannot restore success after a
   -- clock/deadline failure. Malformed/uncertain completion never proves a
   -- release and is not repaired by a second answer to the consumed ring tag.
   -- No timeout, request to close, endpoint loss or transport error is treated
   -- as authoritative retirement. Those states retain indefinitely here;
   -- native integration must establish kernel grant retirement before using
   -- Confirm_Retirement. There is no retry, reconnect or replay here.
   -- Caller takes one bounded report snapshot before Acknowledge/next Start.
private
   type State is limited record
      Info : Report;
      Item : CW.Request := (0, 0, 0);
      Sender : P.Process_ID := P.No_Process;
      Session_Peer : P.Process_ID := P.No_Process;
      Last_Hardware_Ticket : W.Word := 0;
      Last_Token, Last_Time : W.Word := 0;
      Have_Time : Boolean := False;
   end record;
   function Observe (S : State) return Report is (S.Info);
   function Succeeded (S : State) return Boolean is
     (S.Info.At_Phase = Finished and then S.Info.Payload_Released and then S.Info.Outcome = Transfer_Complete);
end HID_I2C_Channel_Lifetime;
