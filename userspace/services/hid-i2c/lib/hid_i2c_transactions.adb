pragma Ada_2022;
package body HID_I2C_Transactions with SPARK_Mode is
   use type W.Word, W.Count, W.Deadline_Kind, W.Completion_Decode_Status, W.Completion_Status;
   use type G.Reference, P.Process_ID;
   procedure Check_Clock (S : in out State; Now : W.Word; Available : Boolean; Fault : out Reason) is
   begin
      if not Available then Fault := Clock_Unavailable; return; end if;
      if S.Have_Time and then Now < S.Last_Time then Fault := Clock_Reversed; return; end if;
      S.Have_Time := True; S.Last_Time := Now;
      Fault := None;
   end Check_Clock;
   procedure Begin_Retirement (S : in out State) is
   begin
      S.Info.At_Phase := Retiring;
      S.Info.Input_Retired := not S.Own_Input;
      S.Info.Output_Retired := not S.Own_Output;
      if S.Own_Input then Revoke (S.Item.Input, S.Info.Input_Revoke_Accepted); end if;
      if S.Own_Output then Revoke (S.Item.Output, S.Info.Output_Revoke_Accepted); end if;
   end Begin_Retirement;
   procedure Abandon (S : in out State; Input, Output : Loan) is
   begin
      S.Info := (Outcome => Setup_Abandoned, others => <>);
      S.Own_Input := Input.Present; S.Own_Output := Output.Present;
      if Input.Present then S.Item.Input := Input.Reference; end if;
      if Output.Present then S.Item.Output := Output.Reference; end if;
      if Input.Present and then Output.Present and then Input.Reference = Output.Reference then
         S.Own_Input := False;
      end if;
      Begin_Retirement (S);
   end Abandon;
   procedure Start
     (S : in out State; Item : W.Request; Available : W.Capacity;
      Expected_Sender : P.Process_ID; Token, Now : W.Word;
      Clock_Available : Boolean; Result : out Start_Result)
   is
      Fault : Reason;
      Sent : Boolean;
   begin
      Result := Already_Owned;
      if S.Info.At_Phase /= Idle then return; end if;
      Result := Rejected; S.Info.Outcome := Invalid_Request;
      if not W.Fits (Item, Available) or else
        Item.Limit.Kind /= W.Within or else Token = 0 or else Token <= S.Last_Token or else
        not P.Is_Process (Expected_Sender) or else
        (Item.Write_Count > 0 and then Item.Read_Count > 0 and then Item.Input = Item.Output) then return; end if;
      Check_Clock (S, Now, Clock_Available, Fault);
      if Fault /= None then S.Info.Outcome := Fault; return; end if;
      if Now >= Item.Limit.At_Millisecond then S.Info.Outcome := Deadline_Expired; return; end if;
      S.Item := Item; S.Sender := Expected_Sender; S.Last_Token := Token;
      S.Own_Input := Item.Write_Count > 0; S.Own_Output := Item.Read_Count > 0;
      S.Info := (At_Phase => In_Flight, Token => Token, others => <>);
      Submit (Item, Token, Sent);
      if not Sent then
         S.Info.Outcome := Submission_Refused; Begin_Retirement (S); Result := Submission_Draining;
      else S.Info.Effects_Possible := True; Result := Started;
      end if;
   end Start;
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean) is
      Fault : Reason;
   begin
      if S.Info.At_Phase not in In_Flight | Retiring then return; end if;
      Check_Clock (S, Now, Clock_Available, Fault);
      if S.Info.At_Phase = In_Flight then
         if Fault = None and then Now >= S.Item.Limit.At_Millisecond then Fault := Deadline_Expired; end if;
         if Fault /= None then S.Info.Outcome := Fault; Begin_Retirement (S); end if;
      elsif Fault /= None and then S.Info.Outcome = Transfer_Complete then
         S.Info.Outcome := Fault;
      end if;
      if S.Info.At_Phase = Retiring then
         if not S.Info.Input_Retired then S.Info.Input_Retired := Retired (S.Item.Input); end if;
         if not S.Info.Output_Retired then S.Info.Output_Retired := Retired (S.Item.Output); end if;
         if S.Info.Input_Retired and then S.Info.Output_Retired then S.Info.At_Phase := Finished; end if;
      end if;
   end Step;
   procedure Receive
     (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Delivery : Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out Completion_Result)
   is
      Decoded : W.Completion_Decoding;
   begin
      Result := Unrelated;
      if Token = 0 or else Token /= S.Info.Token then return; end if;
      Result := Wrong_Sender;
      if Sender /= S.Sender then return; end if;
      Result := Not_Awaiting;
      if S.Info.At_Phase /= In_Flight then return; end if;
      Step (S, Now, Clock_Available);
      if S.Info.At_Phase /= In_Flight then Result := Deadline_Draining; return; end if;
      if Delivery /= Delivered then S.Info.Outcome := Transport_Failed;
      else
         Decoded := W.Decode_Completion (Label, Protocol, Words);
         if Decoded.Status /= W.Completion_Decoded then S.Info.Outcome := Malformed_Completion;
         else
            S.Info.Have_Completion := True; S.Info.Completion := Decoded.Value;
            S.Info.Effects_Possible := S.Info.Effects_Possible or Decoded.Value.Effects_Possible;
            if Decoded.Value.Status /= W.Completed then S.Info.Outcome := Transfer_Refused;
            elsif not Decoded.Value.Hardware_Succeeded or else Decoded.Value.Ticket = 0 or else
              Decoded.Value.Requested_Writes /= S.Item.Write_Count or else
              Decoded.Value.Delivered_Reads /= S.Item.Read_Count
            then S.Info.Outcome := Inconsistent_Completion;
            else S.Info.Outcome := Transfer_Complete;
            end if;
         end if;
      end if;
      Begin_Retirement (S); Result := Accepted;
   end Receive;
   procedure Acknowledge (S : in out State) is
   begin
      S.Info := (others => <>); S.Sender := P.No_Process;
      S.Own_Input := False; S.Own_Output := False;
   end Acknowledge;
end HID_I2C_Transactions;
