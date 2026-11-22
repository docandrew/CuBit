pragma Ada_2022;
with I2C_Transfer_Wire;
with CuBit.Grant_References;
with CuBit.Process_IDs;
generic
   -- Bound to one launcher-admitted I2C endpoint, not a supplied wire target.
   -- Submit is nonblocking. False guarantees no accepted async submission.
   with procedure Submit (Item : I2C_Transfer_Wire.Request;
                          Token : I2C_Transfer_Wire.Word; Accepted : out Boolean);
   with procedure Revoke (Item : CuBit.Grant_References.Reference; Accepted : out Boolean);
   with function Retired (Item : CuBit.Grant_References.Reference) return Boolean;
package HID_I2C_Transactions with SPARK_Mode is
   package W renames I2C_Transfer_Wire;
   package G renames CuBit.Grant_References;
   package P renames CuBit.Process_IDs;
   type Phase is (Idle, In_Flight, Retiring, Finished);
   type Reason is (None, Invalid_Request, Transfer_Complete, Submission_Refused, Clock_Unavailable,
      Clock_Reversed, Deadline_Expired, Transport_Failed, Malformed_Completion,
      Transfer_Refused, Inconsistent_Completion, Setup_Abandoned);
   type Start_Result is (Started, Rejected, Already_Owned, Submission_Draining);
   type Completion_Result is (Unrelated, Wrong_Sender, Not_Awaiting,
      Accepted, Deadline_Draining);
   type Delivery_Status is (Delivered, Target_Died, Cancelled, Queue_Overflow, Transport_Error);
   type Report is record
      At_Phase : Phase := Idle;
      Outcome : Reason := None;
      Token : W.Word := 0;
      Effects_Possible : Boolean := False;
      Have_Completion : Boolean := False;
      Completion : W.Completion := (W.Invalid_Frame, 0, False, False, 0, 0);
      Input_Retired, Output_Retired : Boolean := False;
      Input_Revoke_Accepted, Output_Revoke_Accepted : Boolean := False;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   function Succeeded (S : State) return Boolean;
   procedure Start
     (S : in out State; Item : W.Request; Available : W.Capacity;
      Expected_Sender : P.Process_ID; Token, Now : W.Word;
      Clock_Available : Boolean; Result : out Start_Result);
   -- Rejected/Already_Owned leave ownership of the supplied grants with caller.
   -- Started/Submission_Draining take exactly the nonempty spans' references.
   -- Caller supplies fresh process-global tokens and separately allocated own
   -- pages, with no other grant/alias able to modify either span. No read bytes
   -- may be used or pages reused before Finished. False Revoke is not retirement;
   -- adapter Revoke requests it once; Retired queries authoritative completion.
   type Loan (Present : Boolean := False) is record
      case Present is
         when True => Reference : G.Reference;
         when False => null;
      end case;
   end record;
   procedure Abandon (S : in out State; Input, Output : Loan)
      with Pre => Observe (S).At_Phase = Idle;
   -- Take only actually created grants after setup failure, without submission.
   -- Existing token/time history survives. Step drains them before Finished.
   procedure Receive
     (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Delivery : Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out Completion_Result);
   -- Native owner routes an actual kernel completion envelope. Unrelated and
   -- Wrong_Sender do not consume our pending operation; caller dispatches other
   -- tokens elsewhere. Requested_Writes is NOT an acknowledged write count.
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean);
   -- One bounded clock observation and at most two retirement queries per step.
   -- A timeout never implies hardware cancellation, grant retirement or retry.
   -- Retiring can persist indefinitely; keep pages reserved and yield in Main.
   procedure Acknowledge (S : in out State)
      with Pre => Observe (S).At_Phase = Finished;
   -- Caller consumes success/failure and any successful bytes before releasing
   -- this result. Does not reset token freshness or monotonic clock history.
   -- Serialized single owner; callbacks bounded/nonraising and nonreentrant.
private
   type State is limited record
      Info : Report;
      Item : W.Request;
      Sender : P.Process_ID := P.No_Process;
      Last_Token, Last_Time : W.Word := 0;
      Have_Time : Boolean := False;
      Own_Input, Own_Output : Boolean := False;
   end record;
   function Observe (S : State) return Report is (S.Info);
   function Succeeded (S : State) return Boolean is
     (S.Info.At_Phase = Finished and then S.Info.Outcome = Transfer_Complete);
end HID_I2C_Transactions;
