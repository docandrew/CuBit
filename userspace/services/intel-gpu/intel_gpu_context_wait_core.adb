with Intel_GPU_GuC_Context_Event;
package body Intel_GPU_Context_Wait_Core is
   use Interfaces;
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   use type Life.Operation;
   use type Life.Phase;
   use type Driver.Result;
   use type Receiver.Result;
   procedure Execute
     (Object : in out Context_Type; Action : Life.Operation;
      Poll_Limit : Positive; Status : out Result) is
      Started, Previous, Current : Unsigned_64;
      Sent : Boolean := False;
      Outcome : Driver.Result;
      Item : Receiver.Message;
      Received : Receiver.Result;
      Target : Life.Phase;
      procedure Fail (Reason : Result) is
      begin Fail_Context (Object); Status := Reason; end Fail;
      function Within_Deadline return Boolean is
      begin
         if not Owner_Ready then Fail (Ownership_Lost); return False; end if;
         Current := Now_Us;
         if not Owner_Ready then Fail (Ownership_Lost); return False; end if;
         if Current = Unsigned_64'Last or else Current < Previous then
            Fail (Invalid_Clock); return False;
         end if;
         if Current - Started >= 1_000_000 then Fail (Timed_Out); return False; end if;
         Previous := Current;
         return True;
      end Within_Deadline;
   begin
      Status := Rejected;
      if Action = Life.Enable and then
        State (Object) in Life.Policy_Queued | Life.Disabled
      then
         Target := Life.Enabled;
      elsif Action = Life.Disable and then State (Object) = Life.Enabled then
         Target := Life.Disabled;
      else return;
      end if;
      if not Owner_Ready then Fail (Ownership_Lost); return; end if;
      Started := Now_Us; Previous := Started;
      if not Owner_Ready then Fail (Ownership_Lost); return; end if;
      if Started = Unsigned_64'Last then Fail (Invalid_Clock); return; end if;
      for Index in 1 .. Poll_Limit loop
         if not Within_Deadline then return; end if;
         if not Sent then
            Submit (Object, Action, Outcome);
            if not Within_Deadline then return; end if;
            if Outcome = Driver.Queued then Sent := True;
            elsif Outcome /= Driver.Backpressure then Fail (Send_Failed); return;
            end if;
         end if;
         Poll (Item, Received);
         if not Within_Deadline then return; end if;
         if Received = Receiver.Received and then Item.Length > 0 then
            Dispatch (Object, Intel_GPU_GuC_Context_Event.Words
              (Item.Payload (1 .. Item.Length)), Item.Fence, Outcome);
            if not Within_Deadline then return; end if;
            if Outcome = Driver.Faulted or else Outcome = Driver.Rejected then
               Fail (Context_Failed); return;
            end if;
            if Sent and then State (Object) = Target then
               Status := Complete; return;
            end if;
         elsif Received /= Receiver.Empty then Fail (Receive_Failed); return;
         end if;
         Pause;
      end loop;
      Fail (Timed_Out);
   end Execute;
end Intel_GPU_Context_Wait_Core;
