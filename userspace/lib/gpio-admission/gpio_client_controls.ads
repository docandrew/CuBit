pragma Ada_2022;
with GPIO_Control_Wire;
with CuBit.Process_IDs;
generic
   -- Bounded, nonraising, nonreentrant. False means no accepted submission.
   with procedure Submit (Item : GPIO_Control_Wire.Request;
      Token : GPIO_Control_Wire.Word; Accepted : out Boolean);
package GPIO_Client_Controls with SPARK_Mode is
   package W renames GPIO_Control_Wire;
   package P renames CuBit.Process_IDs;
   use type W.Word, W.Completion, W.Control_Kind, W.Completion_Status;
   use type P.Process_ID;
   type Phase is (Idle, Waiting, Finished, Quarantined);
   type Reason is (None, Invalid_Start, Submission_Refused, Clock_Unavailable,
      Clock_Reversed, Deadline_Expired, Transport_Failed, Malformed_Completion,
      Inconsistent_Completion, Remote_Completed, Remote_Refused, Remote_Uncertain);
   type Report is record
      At_Phase : Phase := Idle;
      Outcome : Reason := None;
      Token : W.Word := 0;
      Effects_Possible, Have_Completion, Late : Boolean := False;
      Completion : W.Completion := (W.Arm, W.Invalid_Frame, 0, 0);
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   -- Ghost observations for this State's token high-water and exact binding.
   -- Process-global token allocation and kernel envelope truth remain external.
   function Last_Submitted_Token (S : State) return W.Word with Ghost;
   function Bound_Sender (S : State) return P.Process_ID with Ghost;
   function Wake_Deadline (S : State; Now, Otherwise : W.Word) return W.Word
     with Global => null, Post => Wake_Deadline'Result <= Otherwise
       and then (if Observe (S).At_Phase = Finished then Wake_Deadline'Result <= Now
          elsif Observe (S).At_Phase in Idle | Quarantined then Wake_Deadline'Result = Otherwise);
   -- Read-only scheduling hint. Finished results need immediate consumption.
   -- Quarantine retains ownership but has no repeatedly expired timer.
   type Start_Result is (Started, Rejected, Already_Owned, Not_Submitted);
   procedure Start (S : in out State; Item : W.Request;
      Expected_Sender : P.Process_ID; Token, Now : W.Word;
      Clock_Available : Boolean; Result : out Start_Result)
     with Post =>
       (Last_Submitted_Token (S) > Last_Submitted_Token (S)'Old or Result not in Started | Not_Submitted)
       and (Last_Submitted_Token (S) = Last_Submitted_Token (S)'Old or Result in Started | Not_Submitted)
       and (Observe (S) = Observe (S)'Old or Result /= Already_Owned)
       and (Bound_Sender (S) = Bound_Sender (S)'Old or Result in Started | Not_Submitted)
       and (case Result is
          when Started => Observe (S).At_Phase = Waiting and then Observe (S).Token = Token
            and then Last_Submitted_Token (S) = Token and then Bound_Sender (S) = Expected_Sender
            and then Observe (S).Effects_Possible
            and then not Observe (S).Have_Completion,
          when Not_Submitted => Observe (S).At_Phase = Finished and then Observe (S).Token = Token
            and then Last_Submitted_Token (S) = Token and then Bound_Sender (S) = Expected_Sender
            and then not Observe (S).Effects_Possible
            and then not Observe (S).Have_Completion and then Observe (S).Outcome = Submission_Refused,
          when Rejected => Observe (S).At_Phase = Idle and then Observe (S).Token = 0,
          when Already_Owned => True);
   type Delivery_Status is (Delivered, Failed);
   type Receive_Result is (Unrelated, Wrong_Sender, Not_Awaiting,
      Accepted, Uncertain_Result);
   procedure Receive (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Delivery : Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out Receive_Result)
     with Global => null,
       Post => Last_Submitted_Token (S) = Last_Submitted_Token (S)'Old
       and (Bound_Sender (S) = Bound_Sender (S)'Old)
       and (Observe (S).Token = Observe (S)'Old.Token)
       and (Observe (S) = Observe (S)'Old or Result not in Unrelated | Wrong_Sender | Not_Awaiting)
       and (Observe (S).Outcome = Observe (S)'Old.Outcome
         or Observe (S)'Old.At_Phase /= Quarantined or Result /= Accepted)
       and (Observe (S).Late or Observe (S)'Old.At_Phase /= Quarantined or Result /= Accepted)
       and (Sender = Bound_Sender (S)'Old or Result /= Accepted)
       and (Delivery = Delivered or Result /= Accepted)
       and (case Result is
          when Accepted => Observe (S).Have_Completion and then Observe (S).At_Phase = Finished,
          when Uncertain_Result => Observe (S).At_Phase = Quarantined,
          when others => True);
   -- Feed only an authoritative kernel completion envelope. Transport failure
   -- matches the kernel token, not a peer sender (kernel failures may have no
   -- peer). Delivered completions also require the exact bound process lifetime.
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean)
     with Global => null,
       Post => Last_Submitted_Token (S) = Last_Submitted_Token (S)'Old
       and (Bound_Sender (S) = Bound_Sender (S)'Old)
       and (Observe (S).Token = Observe (S)'Old.Token)
       and (Observe (S).Effects_Possible = Observe (S)'Old.Effects_Possible)
       and (Observe (S).Have_Completion = Observe (S)'Old.Have_Completion)
       and (Observe (S).Completion = Observe (S)'Old.Completion)
       and (Observe (S).Late = Observe (S)'Old.Late)
       and (Observe (S) = Observe (S)'Old or Observe (S)'Old.At_Phase = Waiting);
   procedure Acknowledge (S : in out State)
      with Global => null, Pre => Observe (S).At_Phase = Finished,
       Post => Last_Submitted_Token (S) = Last_Submitted_Token (S)'Old
       and Bound_Sender (S) = P.No_Process
       and Observe (S).At_Phase = Idle and Observe (S).Token = 0
       and not Observe (S).Effects_Possible and not Observe (S).Have_Completion;
   -- Consuming a result transfers any reported lease/cleanup obligation to the
   -- caller, including late arm success. Late retains the timeout/clock outcome;
   -- it never becomes timely success or evidence of cancellation/quiescence.
   -- Quarantined states cannot be acknowledged/reused without supervision.
   -- Remote Refused/Expired/Clock_Unavailable mean definite no-effect refusal
   -- of THIS operation; server must use Uncertain after possible side effects.
   -- Internal_Mismatch, malformed replies and transport failure quarantine.
   -- Fresh process-global tokens: nonzero, increasing, never Word'Last (kernel
   -- no-completion sentinel), with no wrap. Step/Poll never resubmits a request.
private
   function Matches (Item : W.Request; Value : W.Completion) return Boolean is
     (Value.Kind = Item.Kind and then
        (case Item.Kind is
           when W.Arm => True,
           when W.Release_Pin => Value.Lease = Item.Lease_To_Release
              or else (Value.Status /= W.Completed and then Value.Lease = 0),
           when W.Acknowledge =>
              (Value.Lease = Item.Lease_To_Ack or else (Value.Status /= W.Completed and then Value.Lease = 0))
              and then (Value.Generation = Item.Generation_To_Ack
                 or else (Value.Status /= W.Completed and then Value.Generation = 0))))
     with Global => null;
   subtype Token_High_Water is W.Word range 0 .. W.Word'Last - 1;
   function Valid_State (S : State) return Boolean with Ghost;
   type State is limited record
      Info : Report;
      Item : W.Request;
      Sender : P.Process_ID := P.No_Process;
      Last_Token : Token_High_Water := 0;
      Last_Time : W.Word := 0;
      Have_Time : Boolean := False;
   end record with Type_Invariant => Valid_State (State);
   function Observe (S : State) return Report is (S.Info);
   function Last_Submitted_Token (S : State) return W.Word is (S.Last_Token);
   function Bound_Sender (S : State) return P.Process_ID is (S.Sender);
   function Valid_State (S : State) return Boolean is
     ((if S.Info.At_Phase = Idle then S.Info.Token = 0 and then S.Sender = P.No_Process
         and then not S.Info.Effects_Possible and then not S.Info.Have_Completion and then not S.Info.Late
       else S.Info.Token in 1 .. W.Word'Last - 1 and then S.Info.Token = S.Last_Token
         and then P.Is_Process (S.Sender) and then S.Have_Time)
      and then (if S.Info.Late then S.Info.Have_Completion)
      and then (if S.Info.Have_Completion then
         S.Info.At_Phase in Finished | Quarantined and then W.Valid (S.Info.Completion)
         and then S.Info.Completion.Kind = S.Item.Kind and then Matches (S.Item, S.Info.Completion))
      and then (if S.Info.At_Phase = Waiting then S.Info.Effects_Possible
         and then not S.Info.Have_Completion and then not S.Info.Late and then S.Info.Outcome = None)
      and then (if S.Info.At_Phase = Quarantined then S.Info.Effects_Possible
         and then (if S.Info.Have_Completion then S.Info.Completion.Status in W.Uncertain | W.Internal_Mismatch))
      and then (if S.Info.At_Phase = Finished then
         (if S.Info.Have_Completion then S.Info.Completion.Status not in W.Uncertain | W.Internal_Mismatch
            and then (S.Info.Completion.Status = W.Completed or else not S.Info.Effects_Possible)
          else S.Info.Outcome = Submission_Refused and then not S.Info.Effects_Possible)));
end GPIO_Client_Controls;
