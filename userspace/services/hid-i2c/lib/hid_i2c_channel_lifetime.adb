pragma Ada_2022;
package body HID_I2C_Channel_Lifetime with SPARK_Mode is
   use type W.Word, W.Count, P.Process_ID, W.Completion_Decode_Status, W.Completion_Status;
   procedure Retain (S : in out State; Why : Reason) is
   begin
      if S.Info.Outcome = None then S.Info.Outcome := Why; end if;
      S.Info.At_Phase := Retiring;
   end Retain;
   procedure Check_Clock (S : in out State; Now : W.Word; Available : Boolean; Fault : out Reason) is
   begin
      if not Available then Fault := Clock_Unavailable; return; end if;
      if S.Have_Time and then Now < S.Last_Time then Fault := Clock_Reversed; return; end if;
      S.Have_Time := True; S.Last_Time := Now; Fault := None;
   end Check_Clock;
   procedure Start
     (S : in out State; Item : CW.Request; Available : W.Capacity;
      Expected_Sender : P.Process_ID; Token, Now : W.Word; Clock_Available : Boolean;
      Result : out Starting) is
      Fault : Reason;
      Accepted : Boolean;
   begin
      Result := Already_Owned; if S.Info.At_Phase /= Idle then return; end if;
      Result := Rejected; S.Info.Outcome := Invalid_Request;
      if not CW.Fits (Item, Available) or else Token = 0 or else Token <= S.Last_Token
        or else not P.Is_Process (Expected_Sender)
        or else (S.Session_Peer /= P.No_Process and then Expected_Sender /= S.Session_Peer)
      then return; end if;
      Check_Clock (S, Now, Clock_Available, Fault);
      if Fault /= None then S.Info.Outcome := Fault; return; end if;
      if Now >= Item.Limit then S.Info.Outcome := Deadline_Expired; return; end if;
      S.Item := Item; S.Sender := Expected_Sender; S.Last_Token := Token;
      S.Session_Peer := Expected_Sender;
      S.Info := (At_Phase => In_Flight, Token => Token, others => <>);
      Submit (CW.Encode (Item, Available), Token, Accepted);
      if Accepted then S.Info.Effects_Possible := True; Result := Started;
      else
         S.Info.At_Phase := Finished; S.Info.Outcome := Submission_Refused;
         S.Info.Payload_Released := True; Result := Not_Submitted;
      end if;
   end Start;
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean) is
      Fault : Reason;
   begin
      if S.Info.At_Phase not in In_Flight | Retiring then return; end if;
      Check_Clock (S, Now, Clock_Available, Fault);
      if Fault = None and then Now >= S.Item.Limit then Fault := Deadline_Expired; end if;
      if Fault /= None then Retain (S, Fault); end if;
   end Step;
   function Clean_Refusal (Item : W.Completion) return Boolean is
     (Item.Status in W.Invalid_Frame | W.Invalid_Channel | W.Unsupported_Channel | W.Input_Refused |
        W.Busy | W.Admission_Refused
      and then not Item.Hardware_Succeeded and then not Item.Effects_Possible and then Item.Ticket = 0
      and then Item.Requested_Writes = 0 and then Item.Delivered_Reads = 0);
   procedure Receive
     (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Label : W.Operation; Protocol : W.Word; Words : W.Message_Words;
      Now : W.Word; Clock_Available : Boolean; Result : out Receiving) is
      Decoded : W.Completion_Decoding;
      Success : Boolean;
   begin
      Result := Unrelated; if Token = 0 or else Token /= S.Info.Token then return; end if;
      Result := Wrong_Sender; if Sender /= S.Sender then return; end if;
      Result := Not_Awaiting;
      if S.Info.At_Phase not in In_Flight | Retiring or else S.Info.Trust_Lost then return; end if;
      Step (S, Now, Clock_Available);
      Decoded := W.Decode_Completion (Label, Protocol, Words);
      if Decoded.Status /= W.Completion_Decoded then
         Retain (S, Malformed_Completion); S.Info.Trust_Lost := True; Result := Quarantined; return;
      end if;
      Success := Decoded.Value.Status = W.Completed and then Decoded.Value.Hardware_Succeeded
        and then Decoded.Value.Ticket > S.Last_Hardware_Ticket
        and then Decoded.Value.Requested_Writes = S.Item.Write_Count
        and then Decoded.Value.Delivered_Reads = S.Item.Read_Count;
      if not Success and then not Clean_Refusal (Decoded.Value) then
         Retain (S, Inconsistent_Completion); S.Info.Trust_Lost := True; Result := Quarantined; return;
      end if;
      S.Info.Have_Completion := True; S.Info.Completion := Decoded.Value;
      if Success then S.Last_Hardware_Ticket := Decoded.Value.Ticket; end if;
      S.Info.Effects_Possible := S.Info.Effects_Possible or else Decoded.Value.Effects_Possible;
      if S.Info.Outcome = None then
         S.Info.Outcome := (if Success then Transfer_Complete else Transfer_Refused);
      end if;
      S.Info.Payload_Released := True; S.Info.At_Phase := Finished; Result := Released;
   end Receive;
   procedure Transport_Lost (S : in out State) is
   begin
      if S.Info.At_Phase not in In_Flight | Retiring then return; end if;
      Retain (S, Transport_Failed); S.Info.Trust_Lost := True;
   end Transport_Lost;
   procedure Confirm_Retirement
     (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Confirmed : Boolean; Result : out Confirming) is
   begin
      Result := Unrelated; if Token = 0 or else Token /= S.Info.Token then return; end if;
      Result := Wrong_Sender; if Sender /= S.Sender then return; end if;
      Result := Not_Retiring; if S.Info.At_Phase /= Retiring then return; end if;
      Result := Pending; if not Confirmed then return; end if;
      S.Info.Payload_Released := True; S.Info.At_Phase := Finished; Result := Released;
   end Confirm_Retirement;
   procedure Acknowledge (S : in out State) is
   begin
      -- Preserve ownership even in a checks-suppressed caller that violates
      -- the contract. The native adapter must not reuse that channel area.
      if S.Info.At_Phase /= Finished or else not S.Info.Payload_Released then return; end if;
      S.Info := (others => <>); S.Sender := P.No_Process;
   end Acknowledge;
end HID_I2C_Channel_Lifetime;
