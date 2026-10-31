pragma Ada_2022;
package body HID_I2C_Device with SPARK_Mode is
   use type W.Word, D.Decode_Status, D.Capacity_Status;
   use type Bus.Setup_Result, Bus.Start_Result, Wire.Frame_Status, L.Completion_Status;
   function Wake_Deadline (S : State; Now, Otherwise : W.Word) return W.Word is
      Retry : constant W.Word := (if Now = W.Word'Last then Now else Now + 1);
      Bound : W.Word := Otherwise;
   begin
      if S.Info.At_Phase = Draining or else Bus.Observe (S.Client).At_Phase = T.Retiring then
         return W.Word'Min (Bound, Retry);
      end if;
      if S.Info.At_Phase not in Cold | Serving | Stopped | Faulted then Bound := W.Word'Min (Bound, S.Deadline); end if;
      return Bound;
   end Wake_Deadline;
   procedure Take_Control (S : in out State; Reply : L.Reply) is
      Kind : constant L.Operation := S.Info.Pending_Control;
   begin
      S.Info.Last_Control := Reply;
      if Reply.Status = L.Pending then return; end if;
      S.Info.Pending_Control := L.No_Control;
      case Kind is
         when L.Arming =>
            if Reply.Lease /= 0 then S.Info.Reset_Ticket := Reply.Lease; S.Info.Reset_Still_Armed := True; end if;
            if Reply.Status = L.Uncertain or else (Reply.Status = L.Completed and then Reply.Lease = 0) then
               S.Info.Latch_Uncertain := True;
            end if;
         when L.Releasing =>
            if Reply.Status = L.Completed and then Reply.Lease /= 0 and then Reply.Lease = S.Info.Reset_Ticket then
               S.Info.Reset_Still_Armed := False; S.Info.Reset_Ticket := 0;
            elsif Reply.Status in L.Completed | L.Uncertain then S.Info.Latch_Uncertain := True;
            end if;
         when L.No_Control => null;
      end case;
   end Take_Control;
   procedure Poll_Control (S : in out State) is
      Reply : L.Reply;
   begin
      case S.Info.Pending_Control is
         when L.Arming => Poll_Arm_Reset (Reply);
         when L.Releasing => Poll_Release_Reset (Reply);
         when L.No_Control => return;
      end case;
      Take_Control (S, Reply);
   end Poll_Control;
   procedure Release_Latch (S : in out State) is
      Reply : L.Reply;
   begin
      if not S.Info.Reset_Still_Armed or else S.Release_Attempted or else S.Info.Pending_Control /= L.No_Control then return; end if;
      S.Release_Attempted := True; S.Info.Pending_Control := L.Releasing;
      Begin_Release_Reset (S.Info.Reset_Ticket, S.Deadline, Reply); Take_Control (S, Reply);
   end Release_Latch;
   procedure Drain (S : in out State) is
   begin
      if Bus.Observe (S.Client).At_Phase in T.In_Flight | T.Retiring then S.Info.At_Phase := Draining; return; end if;
      if Bus.Observe (S.Client).At_Phase = T.Finished then
         S.Info.Last_Transfer := Bus.Observe (S.Client);
         if S.Info.Reason = Requested_Stop and then not Bus.Succeeded (S.Client) then S.Info.Reason := Transfer_Failed; end if;
         Bus.Acknowledge (S.Client);
      end if;
      if S.Info.Pending_Control /= L.No_Control then S.Info.At_Phase := Draining; return; end if;
      Release_Latch (S);
      if S.Info.Pending_Control /= L.No_Control then S.Info.At_Phase := Draining; return; end if;
      if S.Info.Reason = Requested_Stop and then (S.Info.Reset_Still_Armed or else S.Info.Latch_Uncertain) then
         S.Info.Reason := Reset_Control_Unresolved;
      end if;
      S.Info.At_Phase := (if S.Info.Reason = Requested_Stop then Stopped else Faulted);
   end Drain;
   procedure Fail (S : in out State; Reason : Failure) is
   begin
      if S.Info.Reason = None then S.Info.Reason := Reason; end if;
      if not S.Loss_Reported then Input_Lost; S.Loss_Reported := True; end if;
      Drain (S);
   end Fail;
   procedure Check_Time (S : in out State; Now : W.Word; Available : Boolean; OK : out Boolean) is
   begin
      OK := False;
      if not Available then Fail (S, Clock_Unavailable); return; end if;
      if S.Have_Clock and then Now < S.Previous then Fail (S, Clock_Reversed); return; end if;
      S.Have_Clock := True; S.Previous := Now;
      if Now >= S.Deadline then Fail (S, Deadline_Expired); return; end if;
      OK := True;
   end Check_Time;
   procedure Submit (S : in out State; Data : D.Bytes; Reads : D.Byte_Count;
      At_Phase : Phase; Now : W.Word; Clock_Available : Boolean) is
      Token : W.Word;
      Available, OK : Boolean;
   begin
      Check_Time (S, Now, Clock_Available, OK); if not OK then return; end if;
      Next_Token (Token, Available);
      if not Available or else Token = 0 then Fail (S, Token_Unavailable); return; end if;
      S.Info.At_Phase := At_Phase;
      Bus.Start (S.Client, Data, W.Count (Reads), Token, Now, S.Deadline, Clock_Available, S.Info.Last_Start);
      if S.Info.Last_Start /= Bus.Submitted then
         S.Info.Memory_Uncertain := S.Info.Last_Start in Bus.Input_Grant_Uncertain | Bus.Output_Grant_Uncertain;
         Fail (S, Submit_Refused);
      end if;
   end Submit;
   procedure Finish_Arm (S : in out State; Now : W.Word; Clock_Available : Boolean) is
   begin
      if S.Info.Pending_Control /= L.No_Control then return; end if;
      if S.Info.Last_Control.Status /= L.Completed or else not S.Info.Reset_Still_Armed or else S.Info.Latch_Uncertain then
         Fail (S, Reset_Arm_Failed); return;
      end if;
      Submit (S, Wire.Encode_Control (S.Info.Descriptor.Value.Command_Register, Wire.Reset), 0, Resetting, Now, Clock_Available);
   end Finish_Arm;
   procedure Finish_Release (S : in out State; Now : W.Word; Clock_Available : Boolean) is
   begin
      if S.Info.Pending_Control /= L.No_Control then return; end if;
      if S.Info.Reset_Still_Armed or else S.Info.Latch_Uncertain then Fail (S, Reset_Release_Failed); return; end if;
      S.Info.Completed := 4;
      Submit (S, Wire.Select_Register (S.Info.Descriptor.Value.Report_Register), S.Info.Descriptor.Value.Report_Length,
         Fetch_Report, Now, Clock_Available);
   end Finish_Release;
   procedure Start (S : in out State; Sender : CuBit.Process_IDs.Process_ID;
      Descriptor_Register : D.Register_Number; Deadline : W.Millisecond;
      Now : W.Word; Clock_Available : Boolean; Result : out Request_Result) is
      Setup : Bus.Setup_Result;
      OK : Boolean;
   begin
      Result := Busy; if S.Info.At_Phase /= Cold then return; end if;
      S.Deadline := Deadline; Check_Time (S, Now, Clock_Available, OK);
      Result := Not_Ready; if not OK then return; end if;
      Bus.Initialize (S.Client, Sender, Setup);
      if Setup not in Bus.Buffers_Ready | Bus.Already_Initialized then
         S.Info.Memory_Uncertain := Setup in Bus.Invalid_Allocation | Bus.Client_Quarantined;
         Fail (S, Buffer_Setup_Failed); return;
      end if;
      Submit (S, Wire.Select_Register (Descriptor_Register), D.Byte_Count (D.Descriptor_Bytes), Fetch_HID, Now, Clock_Available);
      if S.Info.At_Phase = Fetch_HID then Result := Accepted; end if;
   end Start;
   procedure Receive (S : in out State; Token : W.Word; Sender : CuBit.Process_IDs.Process_ID;
      Delivery : T.Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out T.Completion_Result) is
   begin Bus.Receive (S.Client, Token, Sender, Delivery, Label, Protocol, Words, Now, Clock_Available, Result); end Receive;
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean) is
      OK, Compiled : Boolean;
      Reply : L.Reply;
      Length : Natural;
      Previous_Phase : constant Phase := S.Info.At_Phase;
   begin
      Bus.Step (S.Client, Now, Clock_Available);
      Poll_Control (S);
      if S.Info.At_Phase = Draining then Drain (S); return; end if;
      if S.Info.At_Phase in Cold | Serving | Stopped | Faulted then return; end if;
      Check_Time (S, Now, Clock_Available, OK); if not OK then return; end if;
      if S.Info.At_Phase = Arming_Reset then Finish_Arm (S, Now, Clock_Available); return; end if;
      if S.Info.At_Phase = Releasing_Reset then Finish_Release (S, Now, Clock_Available); return; end if;
      if S.Info.At_Phase = Await_Reset then
         if S.Interrupt_Pending then
            S.Interrupt_Pending := False;
            Submit (S, [], S.Info.Descriptor.Value.Maximum_Input, Read_Reset, Now, Clock_Available);
         end if;
         return;
      end if;
      if Bus.Observe (S.Client).At_Phase /= T.Finished then return; end if;
      S.Info.Last_Transfer := Bus.Observe (S.Client);
      if not Bus.Succeeded (S.Client) then Fail (S, Transfer_Failed); return; end if;
      case Previous_Phase is
         when Fetch_HID => Length := D.Descriptor_Bytes;
         when Read_Reset | Read_Input => Length := Natural (S.Info.Descriptor.Value.Maximum_Input);
         when Fetch_Report => Length := Natural (S.Info.Descriptor.Value.Report_Length);
         when others => Length := 0;
      end case;
      if Length > 0 then
         Bus.Read (S.Client, S.Scratch (1 .. Length), OK);
         if not OK then Fail (S, Read_Refused); return; end if;
      end if;
      Bus.Acknowledge (S.Client);
      case Previous_Phase is
         when Fetch_HID =>
            S.Info.Descriptor := D.Decode (S.Scratch (1 .. Length));
            if S.Info.Descriptor.Status /= D.Decoded then Fail (S, Descriptor_Refused); return; end if;
            if D.Check_Capacities (S.Info.Descriptor.Value, Maximum_Report, Maximum_Input, Output_Capacity) /= D.Fits or else
              W.Word (S.Info.Descriptor.Value.Report_Length) + W.Word (Wire.Register_Selector'Length) > W.Word (Bus.Transfer_Capacity) or else
              W.Word (S.Info.Descriptor.Value.Maximum_Input) > W.Word (Bus.Transfer_Capacity) then
               Fail (S, Capacity_Refused); return;
            end if;
            S.Info.Completed := 1;
            Submit (S, Wire.Encode_Control (S.Info.Descriptor.Value.Command_Register, Wire.Power_On), 0, Power_On, Now, Clock_Available);
         when Power_On =>
            S.Info.Completed := 2;
            S.Info.At_Phase := Arming_Reset; S.Release_Attempted := False;
            S.Info.Pending_Control := L.Arming;
            Begin_Arm_Reset (S.Deadline, Reply); Take_Control (S, Reply);
            Finish_Arm (S, Now, Clock_Available);
         when Resetting => S.Info.Completed := 3; S.Info.At_Phase := Await_Reset;
         when Read_Reset =>
            S.Info.Last_Frame := Wire.Inspect_Input (S.Scratch (1 .. Length), S.Info.Descriptor.Value.Maximum_Input, True);
            if S.Info.Last_Frame.Status /= Wire.Reset_Completed then Fail (S, Reset_Not_Acknowledged); return; end if;
            S.Info.At_Phase := Releasing_Reset; Release_Latch (S);
            Finish_Release (S, Now, Clock_Available);
         when Fetch_Report =>
            Compile_Report (S.Info.Descriptor.Value, S.Scratch (1 .. Length), Compiled);
            if not Compiled then Fail (S, Report_Layout_Refused); return; end if;
            S.Info.Completed := Stage_Count'Last; S.Info.At_Phase := Serving;
         when Read_Input =>
            S.Info.Last_Frame := Wire.Inspect_Input (S.Scratch (1 .. Length), S.Info.Descriptor.Value.Maximum_Input, False);
            case S.Info.Last_Frame.Status is
               when Wire.Input_Report =>
                  Deliver_Input (S.Scratch (1 + Wire.Payload_Offset .. Natural (S.Info.Last_Frame.Declared_Length)));
                  if S.Info.Input_Packets < W.Word'Last then S.Info.Input_Packets := S.Info.Input_Packets + 1; end if;
               when Wire.Empty_Frame => null;
               when Wire.Unexpected_Reset => Fail (S, Unexpected_Device_Reset); return;
               when others => Input_Lost;
            end case;
            S.Info.At_Phase := Serving;
         when others => Fail (S, Transfer_Failed);
      end case;
   end Step;
   procedure Reset_Interrupt (S : in out State; Ticket : W.Word; Result : out Interrupt_Result) is
   begin
      Result := Unrelated;
      if Ticket = 0 or else Ticket /= S.Info.Reset_Ticket or else not S.Info.Reset_Still_Armed or else
        S.Info.At_Phase not in Resetting | Await_Reset then return; end if;
      S.Interrupt_Pending := True; Result := Latched;
   end Reset_Interrupt;
   procedure Request_Input (S : in out State; Deadline : W.Millisecond;
      Now : W.Word; Clock_Available : Boolean; Result : out Request_Result) is
   begin
      Result := Busy; if S.Info.At_Phase = Read_Input then return; end if;
      Result := Not_Ready; if not Ready (S) then return; end if;
      S.Deadline := Deadline;
      Submit (S, [], S.Info.Descriptor.Value.Maximum_Input, Read_Input, Now, Clock_Available);
      if S.Info.At_Phase = Read_Input then Result := Accepted; end if;
   end Request_Input;
   procedure Request_Stop (S : in out State) is
   begin
      if S.Info.At_Phase in Stopped | Faulted then return; end if;
      Fail (S, Requested_Stop);
   end Request_Stop;
   procedure Close (S : in out State; Result : out Bus.Close_Result) is
   begin
      Result := Bus.Still_Owned;
      if not Quiescent (S) then return; end if;
      Bus.Close (S.Client, Result);
   end Close;
end HID_I2C_Device;
