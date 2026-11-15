pragma Ada_2022;
package body HID_I2C_Buffered_Client with SPARK_Mode is
   use type W.Word, Transactions.Starting;
   Session_Claimed : Boolean := False;
   procedure Begin_Close (S : in out State) is
   begin
      S.Closing := True; S.Configured := False;
      if not S.Close_Attempted then S.Close_Attempted := True; Close_Channel; end if;
   end Begin_Close;
   procedure Poll_Close (S : in out State) is
   begin
      if S.Close_Attempted and then not S.Close_Confirmed then
         S.Close_Confirmed := Retirement_Confirmed;
      end if;
   end Poll_Close;
   procedure Initialize (S : in out State; Sender : P.Process_ID;
      Deadline : W.Millisecond; Result : out Setup_Result) is
      Opened : Boolean;
   begin
      Result := Client_Quarantined; if S.Closing then return; end if;
      Result := Already_Initialized; if S.Configured then return; end if;
      Result := Client_Quarantined; if S.Attempted then return; end if;
      if Session_Claimed then return; end if;
      Result := Not_Idle; if Observe (S).At_Phase /= Transactions.Idle then return; end if;
      Result := Invalid_Sender; if not P.Is_Process (Sender) then return; end if;
      Session_Claimed := True; S.Attempted := True; S.Sender := Sender;
      Open_Channel (Sender, Deadline, Opened);
      if not Opened then Begin_Close (S); Result := Channel_Refused; return; end if;
      S.Configured := True; Result := Buffers_Ready;
   end Initialize;
   procedure Start (S : in out State; Write_Data : D.Bytes; Read_Count : W.Count;
      Token, Now : W.Word; Deadline : W.Millisecond; Clock_Available : Boolean;
      Result : out Start_Result) is
      Stored : Boolean;
      Started : Transactions.Starting;
   begin
      Result := Client_Not_Ready; if not Ready (S) then return; end if;
      Result := Client_Busy; if Observe (S).At_Phase /= Transactions.Idle then return; end if;
      Result := Request_Rejected;
      if W.Word (Write_Data'Length) + W.Word (Read_Count) not in 1 .. W.Word (Maximum_Transfer) then return; end if;
      S.Last_Rejection := Transactions.None;
      if Write_Data'Length > 0 then
         Store_Write (Write_Data, Stored);
         if not Stored then
            S.Last_Rejection := Transactions.Transport_Failed;
            Begin_Close (S); Result := Channel_Uncertain; return;
         end if;
      end if;
      S.Expected_Reads := Read_Count;
      Transactions.Start (S.Owner, (W.Count (Write_Data'Length), Read_Count, Deadline),
         Maximum_Transfer, S.Sender, Token, Now, Clock_Available, Started);
      if Started = Transactions.Started then Result := Submitted;
      else
         S.Last_Rejection := Observe (S).Outcome;
         Result := (if Started = Transactions.Already_Owned then Client_Busy else Transaction_Refused);
      end if;
   end Start;
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean) is
      Words : W.Completion_Words;
      Available, Ended : Boolean;
      Received : Transactions.Receiving;
      Confirmed : Transactions.Confirming;
   begin
      if not S.Closing and then Observe (S).At_Phase in Transactions.In_Flight | Transactions.Retiring then
         Poll_Answer (Observe (S).Token, Words, Available, Ended);
         if Ended then Transactions.Transport_Lost (S.Owner);
         elsif Available then
            Transactions.Receive (S.Owner, Observe (S).Token, S.Sender,
               W.Transfer_Reply_Operation, W.Protocol_Version, Words, Now, Clock_Available, Received);
         end if;
      end if;
      Transactions.Step (S.Owner, Now, Clock_Available);
      if Observe (S).At_Phase = Transactions.Retiring then Begin_Close (S); end if;
      Poll_Close (S);
      if S.Closing and then Observe (S).At_Phase = Transactions.Retiring then
         Transactions.Confirm_Retirement (S.Owner, Observe (S).Token, S.Sender, S.Close_Confirmed, Confirmed);
      end if;
   end Step;
   procedure Read (S : State; Data : out D.Bytes; OK : out Boolean) is
   begin
      OK := False;
      if not Data_Ready (S) or else W.Word (Data'Length) /= W.Word (S.Expected_Reads) then
         Data := [others => 0]; return;
      end if;
      Load_Read (Data, OK);
      if not OK then Data := [others => 0]; end if;
   end Read;
   procedure Acknowledge (S : in out State) is
   begin
      if Observe (S).At_Phase /= Transactions.Finished then return; end if;
      Transactions.Acknowledge (S.Owner); S.Expected_Reads := 0;
   end Acknowledge;
   procedure Close (S : in out State; Result : out Close_Result) is
   begin
      Result := Still_Owned;
      if Observe (S).At_Phase in Transactions.In_Flight | Transactions.Retiring then return; end if;
      if Observe (S).At_Phase = Transactions.Finished then Acknowledge (S); end if;
      if not S.Attempted then Result := Closed; return; end if;
      Begin_Close (S); Poll_Close (S);
      if S.Close_Confirmed then S.Sender := P.No_Process; Result := Closed; end if;
   end Close;
end HID_I2C_Buffered_Client;
