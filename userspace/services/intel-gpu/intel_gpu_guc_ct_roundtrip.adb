package body Intel_GPU_GuC_CT_Roundtrip is
   use Interfaces;
   use type Receiver.Result;
   procedure Execute
     (Object : in out Attempt; Fence : Unsigned_16;
      Poll_Limit : Positive; Reply : out Unsigned_32; Status : out Result) is
      Start, Previous, Current : Unsigned_64;
      Item : Receiver.Message;
      Receive_Status : Receiver.Result;
      OK : Boolean;
      Events : Natural := 0;
   begin
      Reply := 0; Status := Rejected;
      if Object.Started or else Fence = 0 or else not Owner_Ready then return; end if;
      Object.Started := True;
      Start := Now_Us; Previous := Start;
      if Start = Unsigned_64'Last then Status := Invalid_Clock; return; end if;
      Queue (Fence, OK);
      if not Owner_Ready then Status := Ownership_Lost; return; end if;
      if not OK then Status := Queue_Failed; return; end if;
      for Index in 1 .. Poll_Limit loop
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         Current := Now_Us;
         if Current = Unsigned_64'Last or else Current < Previous then
            Status := Invalid_Clock; return;
         end if;
         if Current - Start >= 1_000_000 then Status := Timed_Out; return; end if;
         Previous := Current;
         Poll (Item, Receive_Status);
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         -- Also bound the time spent inside the receive callback.
         Current := Now_Us;
         if Current = Unsigned_64'Last or else Current < Previous then
            Status := Invalid_Clock; return;
         end if;
         if Current - Start >= 1_000_000 then Status := Timed_Out; return; end if;
         Previous := Current;
         if Receive_Status = Receiver.Empty then
            Pause;
         elsif Receive_Status /= Receiver.Received then
            Status := Receive_Failed; return;
         elsif Item.Length = 0 then
            Status := Invalid_Reply; return;
         elsif (Item.Payload (1) and 16#F0000000#) = 16#90000000# then
            if Events = 8 then Status := Event_Overflow; return; end if;
            Retain_Event (Item, OK);
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if not OK then Status := Event_Overflow; return; end if;
            Events := Events + 1;
         elsif Item.Fence /= Fence or else Item.Length /= 1 then
            Status := Invalid_Reply; return;
         else
            Reply := Item.Payload (1);
            case Reply and 16#F0000000# is
               when 16#F0000000# =>
                  Status := (if Reply = 16#F0000000# then Complete else Invalid_Reply);
                  return;
               when 16#E0000000# => Status := Firmware_Failed; return;
               when 16#D0000000# => Status := Retry_Requested; return;
               when 16#B0000000# => Pause;
               when others => Status := Invalid_Reply; return;
            end case;
         end if;
      end loop;
      Status := Timed_Out;
   end Execute;
end Intel_GPU_GuC_CT_Roundtrip;
