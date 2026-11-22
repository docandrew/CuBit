pragma Ada_2022;
package body HID_GPIO_Reset with SPARK_Mode is
   package W renames GPIO_Control_Wire;
   package L renames GPIO_Lease_Control;
   use type W.Word;
   use type W.Completion_Status;
   use type L.Operation;
   package body Bridge is
      use type Controls.Phase, Controls.Reason, Controls.Start_Result;
      function Observe return Controls.Report is (Controls.Observe (State));
      function Wake_Deadline (Now, Otherwise : GPIO_Control_Wire.Word)
         return GPIO_Control_Wire.Word is
        (Controls.Wake_Deadline (State, Now, Otherwise));
      function Known_Lease return W.Word is
        (if Observe.Have_Completion then Observe.Completion.Lease else Release_Lease);
      procedure Poll (Expected : L.Operation; Reply : out L.Reply) is
         Now : W.Word;
         Available : Boolean;
         Info : Controls.Report;
      begin
         Reply := (L.Uncertain, Known_Lease);
         if Operation /= Expected then return; end if;
         Clock (Now, Available); Controls.Step (State, Now, Available);
         Info := Observe;
         case Info.At_Phase is
            when Controls.Waiting => Reply := (L.Pending, 0);
            when Controls.Quarantined =>
               if not Info.Have_Completion and then Info.Outcome in
                  Controls.Clock_Unavailable | Controls.Clock_Reversed | Controls.Deadline_Expired
               then Reply := (L.Pending, 0);
               else Reply := (L.Uncertain, Known_Lease);
               end if;
            when Controls.Finished =>
               if Info.Have_Completion then
                  Reply := (Status => (if Info.Completion.Status = W.Completed then L.Completed else L.Refused),
                     Lease => Known_Lease);
                  if Reply.Lease = 0 and then Expected = L.Releasing then Reply.Lease := Release_Lease; end if;
               else Reply := (L.Refused, Release_Lease);
               end if;
               Controls.Acknowledge (State); Operation := L.No_Control; Release_Lease := 0;
            when Controls.Idle => Reply := (L.Refused, Release_Lease);
         end case;
      end Poll;
      procedure Start (Item : W.Request; Kind : L.Operation; Now : W.Word;
         Reply : out L.Reply) is
         Token : W.Word;
         Available : Boolean;
         Result : Controls.Start_Result;
      begin
         Reply := (L.Refused, Release_Lease);
         Next_Token (Token, Available); if not Available then return; end if;
         Controls.Start (State, Item, Expected_Sender, Token, Now, True, Result);
         case Result is
            when Controls.Started | Controls.Not_Submitted =>
               Operation := Kind; Poll (Kind, Reply);
            when Controls.Already_Owned => Reply := (L.Uncertain, Known_Lease);
            when Controls.Rejected => null;
         end case;
      end Start;
      procedure Begin_Arm_Reset (Deadline : I2C_Transfer_Wire.Millisecond; Reply : out L.Reply) is
         Now : W.Word;
         Available : Boolean;
      begin
         if Observe.At_Phase /= Controls.Idle then Reply := (L.Uncertain, Known_Lease); return; end if;
         Release_Lease := 0; Clock (Now, Available);
         Reply := (L.Refused, 0); if not Available then return; end if;
         Start ((W.Arm, (W.Within, Deadline)), L.Arming, Now, Reply);
      end Begin_Arm_Reset;
      procedure Poll_Arm_Reset (Reply : out L.Reply) is
      begin Poll (L.Arming, Reply); end Poll_Arm_Reset;
      procedure Begin_Release_Reset (Ticket : I2C_Transfer_Wire.Word;
         Deadline : I2C_Transfer_Wire.Millisecond; Reply : out L.Reply)
      is
         pragma Unreferenced (Deadline);
         Now, Limit : W.Word;
         Available : Boolean;
      begin
         if Observe.At_Phase /= Controls.Idle then Reply := (L.Uncertain, Known_Lease); return; end if;
         Release_Lease := Ticket; Reply := (L.Refused, Ticket);
         Clock (Now, Available);
         if not Available or else Ticket = 0 or else Now > W.Millisecond'Last - Cleanup_Budget then return; end if;
         Limit := Now + Cleanup_Budget;
         Start ((W.Release_Pin, (W.Within, Limit), Ticket), L.Releasing, Now, Reply);
      end Begin_Release_Reset;
      procedure Poll_Release_Reset (Reply : out L.Reply) is
      begin Poll (L.Releasing, Reply); end Poll_Release_Reset;
      procedure Receive (Token : W.Word; Sender : CuBit.Process_IDs.Process_ID;
         Delivery : Controls.Delivery_Status; Label : W.Operation; Protocol : W.Word;
         Words : W.Message_Words; Result : out Controls.Receive_Result)
      is
         Now : W.Word;
         Available : Boolean;
      begin
         Clock (Now, Available);
         Controls.Receive (State, Token, Sender, Delivery, Label, Protocol, Words, Now, Available, Result);
      end Receive;
   end Bridge;
end HID_GPIO_Reset;
