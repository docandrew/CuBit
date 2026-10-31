pragma Ada_2022;
package body HID_I2C_Buffered_Client with SPARK_Mode is
   use type Transactions.Start_Result, HID_I2C_Grant_Creation.Status;
   function Valid_Base (Base : W.Word) return Boolean is
     (Base /= 0 and then Base mod Page_Bytes = 0 and then Base <= W.Word'Last - Allocation_Bytes);
   procedure Initialize (S : in out State; Sender : P.Process_ID; Result : out Setup_Result) is
      OK : Boolean;
      Address : W.Word;
   begin
      Result := Client_Quarantined; if S.Poisoned then return; end if;
      Result := Already_Initialized; if S.Configured then return; end if;
      Result := Not_Idle; if Observe (S).At_Phase /= Transactions.Idle then return; end if;
      Result := Invalid_Sender; if not P.Is_Process (Sender) then return; end if;
      if S.Input_Address = 0 then
         Allocate (Allocation_Bytes, Address, OK);
         Result := Input_Allocation_Failed; if not OK then return; end if;
         Result := Invalid_Allocation;
         if not Valid_Base (Address) then S.Poisoned := True; return; end if;
         S.Input_Address := Address;
      end if;
      if S.Output_Address = 0 then
         Allocate (Allocation_Bytes, Address, OK);
         Result := Output_Allocation_Failed; if not OK then return; end if;
         Result := Invalid_Allocation;
         if not Valid_Base (Address) or else
           (Address < S.Input_Address + Allocation_Bytes and then S.Input_Address < Address + Allocation_Bytes)
         then S.Poisoned := True; return; end if;
         S.Output_Address := Address;
      end if;
      S.Sender := Sender; S.Configured := True; Result := Buffers_Ready;
   end Initialize;
   procedure Start (S : in out State; Write_Data : D.Bytes; Read_Count : W.Count;
      Token, Now : W.Word; Deadline : W.Millisecond; Clock_Available : Boolean;
      Result : out Start_Result)
   is
      Input, Output : Transactions.Loan;
      Reference : G.Reference;
      Creation : HID_I2C_Grant_Creation.Status;
      Start_Info : Transactions.Start_Result;
      Request : W.Request;
   begin
      Result := Client_Not_Ready; if not Ready (S) then return; end if;
      Result := Client_Busy; if Observe (S).At_Phase /= Transactions.Idle then return; end if;
      Result := Request_Rejected;
      if W.Word (Write_Data'Length) + W.Word (Read_Count) not in 1 .. W.Word (Maximum_Transfer) then return; end if;
      S.Last_Rejection := Transactions.None; S.Expected_Reads := Read_Count;
      if Read_Count > 0 then Clear (S.Output_Address, Allocation_Bytes); end if;
      if Write_Data'Length > 0 then
         Clear (S.Input_Address, Allocation_Bytes); Store (S.Input_Address, Write_Data);
         Create (S.Input_Address, Page_Count, False, Reference, Creation);
         if Creation /= HID_I2C_Grant_Creation.Created then
            Transactions.Abandon (S.Owner, Input, Output);
            if Creation = HID_I2C_Grant_Creation.Uncertain then
               S.Poisoned := True; Result := Input_Grant_Uncertain;
            else Result := Input_Grant_Refused;
            end if;
            return;
         end if;
         Input := (True, Reference);
      end if;
      if Read_Count > 0 then
         Create (S.Output_Address, Page_Count, True, Reference, Creation);
         if Creation /= HID_I2C_Grant_Creation.Created then
            Transactions.Abandon (S.Owner, Input, Output);
            if Creation = HID_I2C_Grant_Creation.Uncertain then
               S.Poisoned := True; Result := Output_Grant_Uncertain;
            else Result := Output_Grant_Refused;
            end if;
            return;
         end if;
         Output := (True, Reference);
      end if;
      Request := (Input => (if Input.Present then Input.Reference else G.Reference'(others => <>)),
         Output => (if Output.Present then Output.Reference else G.Reference'(others => <>)), Write_Count => W.Count (Write_Data'Length),
         Read_Count => Read_Count, Limit => (W.Within, Deadline));
      Transactions.Start (S.Owner, Request, Maximum_Transfer, S.Sender, Token, Now, Clock_Available, Start_Info);
      case Start_Info is
         when Transactions.Started => Result := Submitted;
         when Transactions.Submission_Draining => Result := Submit_Draining;
         when Transactions.Rejected =>
            S.Last_Rejection := Observe (S).Outcome;
            Transactions.Abandon (S.Owner, Input, Output); Result := Transaction_Refused;
         when Transactions.Already_Owned =>
            -- Impossible for a serialized/nonreentrant owner checked above.
            S.Poisoned := True; Result := Client_Busy;
      end case;
   end Start;
   procedure Receive (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Delivery : Transactions.Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out Transactions.Completion_Result) is
   begin Transactions.Receive (S.Owner, Token, Sender, Delivery, Label, Protocol, Words, Now, Clock_Available, Result); end Receive;
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean) is
   begin Transactions.Step (S.Owner, Now, Clock_Available); end Step;
   procedure Read (S : State; Data : out D.Bytes; OK : out Boolean) is
   begin
      Data := [others => 0]; OK := False;
      if not Data_Ready (S) or else W.Word (Data'Length) /= W.Word (S.Expected_Reads) then return; end if;
      Load (S.Output_Address, Data); OK := True;
   end Read;
   procedure Acknowledge (S : in out State) is
   begin Transactions.Acknowledge (S.Owner); S.Expected_Reads := 0; end Acknowledge;
   procedure Close (S : in out State; Result : out Close_Result) is
      OK : Boolean;
   begin
      Result := Still_Owned;
      if S.Poisoned then Result := Release_Refused; return; end if;
      if Observe (S).At_Phase in Transactions.In_Flight | Transactions.Retiring then return; end if;
      if Observe (S).At_Phase = Transactions.Finished then Acknowledge (S); end if;
      S.Configured := False; Result := Release_Refused;
      if S.Input_Address /= 0 then
         Release (S.Input_Address, Allocation_Bytes, OK); if OK then S.Input_Address := 0; end if;
      end if;
      if S.Output_Address /= 0 then
         Release (S.Output_Address, Allocation_Bytes, OK); if OK then S.Output_Address := 0; end if;
      end if;
      if S.Input_Address = 0 and then S.Output_Address = 0 and then not S.Poisoned then
         S.Sender := P.No_Process; Result := Closed;
      end if;
   end Close;
end HID_I2C_Buffered_Client;
