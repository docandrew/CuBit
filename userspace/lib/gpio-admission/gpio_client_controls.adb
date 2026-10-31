pragma Ada_2022;
package body GPIO_Client_Controls with SPARK_Mode is
   use type W.Deadline_Kind, W.Decode_Status;
   function Wake_Deadline (S : State; Now, Otherwise : W.Word) return W.Word is
   begin
      if S.Info.At_Phase = Finished then return W.Word'Min (Now, Otherwise);
      elsif S.Info.At_Phase = Waiting and then S.Item.Limit.Kind = W.Within then
         return W.Word'Min (S.Item.Limit.At_Millisecond, Otherwise);
      else return Otherwise; end if;
   end Wake_Deadline;
   procedure Check_Clock (S : in out State; Now : W.Word; Available : Boolean;
      Error : out Reason) is
   begin
      if not Available then Error := Clock_Unavailable;
      elsif S.Have_Time and then Now < S.Last_Time then Error := Clock_Reversed;
      else S.Have_Time := True; S.Last_Time := Now; Error := None;
      end if;
   end Check_Clock;
   procedure Start (S : in out State; Item : W.Request;
      Expected_Sender : P.Process_ID; Token, Now : W.Word;
      Clock_Available : Boolean; Result : out Start_Result)
   is
      Error : Reason;
      Sent : Boolean;
   begin
      Result := Already_Owned; if S.Info.At_Phase /= Idle then return; end if;
      Result := Rejected; S.Info.Outcome := Invalid_Start;
      if not P.Is_Process (Expected_Sender) or else Token = 0 or else
         Token = W.Word'Last or else Token <= S.Last_Token then return; end if;
      Check_Clock (S, Now, Clock_Available, Error);
      if Error /= None then S.Info.Outcome := Error; return; end if;
      if Item.Limit.Kind = W.Within and then Now >= Item.Limit.At_Millisecond then
         S.Info.Outcome := Deadline_Expired; return;
      end if;
      S.Item := Item; S.Sender := Expected_Sender; S.Last_Token := Token;
      S.Info := (At_Phase => Waiting, Token => Token, others => <>);
      Submit (Item, Token, Sent);
      if Sent then S.Info.Effects_Possible := True; Result := Started;
      else S.Info.At_Phase := Finished; S.Info.Outcome := Submission_Refused; Result := Not_Submitted;
      end if;
   end Start;
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean) is
      Error : Reason;
   begin
      if S.Info.At_Phase /= Waiting then return; end if;
      Check_Clock (S, Now, Clock_Available, Error);
      if Error = None and then S.Item.Limit.Kind = W.Within
        and then Now >= S.Item.Limit.At_Millisecond then Error := Deadline_Expired; end if;
      if Error /= None then S.Info.At_Phase := Quarantined; S.Info.Outcome := Error; end if;
   end Step;
   procedure Receive (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Delivery : Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out Receive_Result)
   is
      Decoded : W.Completion_Decoding;
      Was_Late : Boolean;
   begin
      Result := Unrelated; if Token = 0 or else Token /= S.Info.Token then return; end if;
      Result := Not_Awaiting;
      if S.Info.At_Phase not in Waiting | Quarantined or else S.Info.Have_Completion then return; end if;
      if Delivery = Delivered and then Sender /= S.Sender then Result := Wrong_Sender; return; end if;
      Step (S, Now, Clock_Available); Was_Late := S.Info.At_Phase = Quarantined;
      Result := Uncertain_Result;
      if Delivery /= Delivered then
         S.Info.At_Phase := Quarantined; S.Info.Outcome := Transport_Failed; return;
      end if;
      Decoded := W.Decode_Completion (Label, Protocol, Words);
      if Decoded.Status /= W.Decoded then
         S.Info.At_Phase := Quarantined; S.Info.Outcome := Malformed_Completion; return;
      end if;
      if not Matches (S.Item, Decoded.Value) then
         S.Info.At_Phase := Quarantined; S.Info.Outcome := Inconsistent_Completion; return;
      end if;
      S.Info.Have_Completion := True; S.Info.Completion := Decoded.Value; S.Info.Late := Was_Late;
      if Decoded.Value.Status in W.Uncertain | W.Internal_Mismatch then
         S.Info.At_Phase := Quarantined; S.Info.Outcome := Remote_Uncertain; return;
      end if;
      S.Info.At_Phase := Finished;
      if not Was_Late then
         S.Info.Outcome := (if Decoded.Value.Status = W.Completed then Remote_Completed else Remote_Refused);
      end if;
      if Decoded.Value.Status /= W.Completed then S.Info.Effects_Possible := False; end if;
      Result := Accepted;
   end Receive;
   procedure Acknowledge (S : in out State) is
   begin S.Info := (others => <>); S.Sender := P.No_Process; end Acknowledge;
end GPIO_Client_Controls;
