pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Channel_Protocol;

package body CuBit.GPU_Sessions is

   use type GQ.Context_State, GQ.Wake_Result;

   --  Spins before a blocking wait: a job completes within the driver's
   --  1 ms turn.
   Reach_Spins : constant := 256;

   function Region (Base : Unsigned_64) return System.Address;
   function Region (Base : Unsigned_64) return System.Address is
     (To_Address (Integer_Address (Base)));

   procedure Open
     (S : in out Session; Endpoint : CuBit.Messages.CapabilitySlot; Opened : out Boolean)
   is
      use type CuBit.Channels.Open_Result;
      Result : CuBit.Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      Opened := S.Opened;
      if S.Opened then
         return;
      end if;
      CuBit.Channels.Open
        (Endpoint, GQ.QUEUE_CONTRACT, CuBit.Channels.Producing, S.Queue, Result,
         Ignore_Refusal, Connector => GQ.Queue_Connector);
      if Result /= CuBit.Channels.Opened then
         return;
      end if;
      S.Endpoint := Endpoint;
      Clients.Attach (S.Client, Region (S.Queue.Own_Base), Region (S.Queue.Peer_Base));
      S.Opened := True;
      Opened := True;
   end Open;

   procedure Close (S : in out Session) is
   begin
      if S.Opened then
         Clients.Detach (S.Client);
         CuBit.Channels.Close (S.Queue);
         S.Opened := False;
      end if;
   end Close;

   function Is_Open (S : Session) return Boolean is (S.Opened);

   function Can_Submit (S : in out Session) return Boolean is
     (S.Opened and then Clients.Can_Submit (S.Client));
   function Next_Signal (S : Session; Context : GQ.Context_Index) return GQ.Timeline_Value is
     (Clients.Next_Signal (S.Client, Context));
   function Reached (S : Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value)
     return Boolean is (S.Opened and then Clients.Reached (S.Client, Context, Target));
   function State (S : Session; Context : GQ.Context_Index) return GQ.Context_State is
     (if S.Opened then Clients.State (S.Client, Context) else GQ.Unused);

   procedure Read_Status
     (S : Session; Context : GQ.Context_Index; Line : out GQ.Status_Line; OK : out Boolean) is
   begin
      Line := (others => <>);
      OK := False;
      if S.Opened then
         Clients.Read_Status (S.Client, Context, Line, OK);
      end if;
   end Read_Status;

   procedure Submit
     (S : in out Session; Item : Clients.Job; Tag : out Clients.Token;
      Signal : out GQ.Timeline_Value; Submitted : out Boolean)
   is
      Kick : Boolean;
   begin
      Tag := 0;
      Signal := 0;
      Submitted := False;
      if not S.Opened then
         return;
      end if;
      Clients.Submit (S.Client, Item, Tag, Signal, Submitted, Kick);
      if Kick then
         CuBit.Channels.Kick (S.Queue);
      end if;
   end Submit;

   procedure Reap (S : in out Session; Item : out Clients.Q.Completion; Got : out Boolean) is
   begin
      Clients.Reap (S.Client, Item, Got);
   end Reap;

   function Wake_Request
     (Context : GQ.Context_Index; Target : GQ.Timeline_Value) return CuBit.Messages.Message;
   function Wake_Request
     (Context : GQ.Context_Index; Target : GQ.Timeline_Value) return CuBit.Messages.Message
   is
      Request : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Request.tag := (label => GQ.OP_GPU_WAKE, length => 2, flags => 0, reserved => 0);
      Request.words (GQ.Wake_Context_Word) := Unsigned_64 (Context);
      Request.words (GQ.Wake_Target_Word) := Unsigned_64 (Target);
      return Request;
   end Wake_Request;

   function Decode (Word : Unsigned_64) return GQ.Wake_Result;
   function Decode (Word : Unsigned_64) return GQ.Wake_Result is
   begin
      for R in GQ.Wake_Result loop
         if Unsigned_64 (GQ.Wake_Result'Enum_Rep (R)) = Word then
            return R;
         end if;
      end loop;
      return GQ.No_Queue;
   end Decode;

   function Wake_Armed (S : Session) return Boolean is
     (not CuBit.Async_Requests.Drained (S.Wake));

   procedure Arm_Wake
     (S : in out Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value;
      Wake_Token : CuBit.Async_Requests.Token; Accepted : out Boolean)
   is
   begin
      Accepted := False;
      if not S.Opened or else not CuBit.Async_Requests.Can_Reserve (S.Wake, Wake_Token) then
         return;
      end if;
      CuBit.Async_Requests.Reserve (S.Wake, Wake_Token, Accepted);
      if not Accepted then
         return;
      end if;
      Accepted := CuBit.Messages.capSubmit (S.Endpoint, Wake_Request (Context, Target), Wake_Token);
      CuBit.Async_Requests.Submitted (S.Wake, Accepted);
   end Arm_Wake;

   procedure Complete_Wake
     (S : in out Session; Receipt : CuBit.Messages.CompletionEntry;
      Consumed : out Boolean; Result : out GQ.Wake_Result)
   is
   begin
      Result := GQ.No_Queue;
      CuBit.Async_Requests.Capture (S.Wake, Receipt.token, Receipt.valid, Consumed);
      if not Consumed then
         return;
      end if;
      CuBit.Async_Requests.Release (S.Wake);
      if Receipt.status = CuBit.Messages.COMPLETION_OK and then
        Receipt.msg.tag.label = GQ.OP_GPU_WAKE
      then
         Result := Decode (Receipt.msg.words (GQ.Wake_Result_Word));
      end if;
   end Complete_Wake;

   --  Reached_Target or Context_Failed when Context has settled.
   function Settled
     (S : Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value;
      Result : out Wait_Result) return Boolean;
   function Settled
     (S : Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value;
      Result : out Wait_Result) return Boolean is
   begin
      Result := Queue_Ended;
      if Clients.Reached (S.Client, Context, Target) then
         Result := Reached_Target;
         return True;
      elsif State (S, Context) in GQ.Faulted | GQ.Hung | GQ.Lost | GQ.Retired then
         Result := Context_Failed;
         return True;
      end if;
      return False;
   end Settled;

   function Now_Ms return Unsigned_64;
   function Now_Ms return Unsigned_64 is
     (CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME));

   procedure Poll_Reached
     (S : Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value;
      Deadline : Unsigned_64; Result : out Wait_Result)
   is
      Ignore : Unsigned_64;
   begin
      Result := Queue_Ended;
      if not S.Opened then
         return;
      end if;
      loop
         if Settled (S, Context, Target, Result) then
            return;
         elsif Deadline /= CuBit.Messages.Wait_Forever and then Now_Ms >= Deadline then
            Result := Deadline_Reached;
            return;
         end if;
         Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_SLEEP, Poll_Pause_Ms);
      end loop;
   end Poll_Reached;

   procedure Wait_Reached
     (S : Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value;
      Deadline : Unsigned_64; Result : out Wait_Result)
   is
      Request : CuBit.Messages.Message;
      Reply : CuBit.Messages.MessageTag;
   begin
      Result := Queue_Ended;
      if not S.Opened then
         return;
      end if;
      for Look in 1 .. Reach_Spins loop
         if Settled (S, Context, Target, Result) then
            return;
         end if;
      end loop;
      loop
         if Settled (S, Context, Target, Result) then
            return;
         end if;
         Request := Wake_Request (Context, Target);
         Reply := CuBit.Messages.capCall (S.Endpoint, Request, Deadline);
         if Reply.label = CuBit.Messages.REPLY_TIMEOUT then
            if not Settled (S, Context, Target, Result) then
               Result := Deadline_Reached;
            end if;
            return;
         elsif Reply.label /= GQ.OP_GPU_WAKE or else
           Decode (Request.words (GQ.Wake_Result_Word)) = GQ.No_Queue
         then
            Result := Queue_Ended;
            return;
         elsif Decode (Request.words (GQ.Wake_Result_Word)) = GQ.Not_Held then
            --  The driver's one wake serves another session: look at the
            --  status line on our own timer, without calling again.
            Poll_Reached (S, Context, Target, Deadline, Result);
            return;
         end if;
         if not Settled (S, Context, Target, Result) and then
           Deadline /= CuBit.Messages.Wait_Forever and then Now_Ms >= Deadline
         then
            --  The deadline holds even against a driver that answers wakes
            --  with nothing reached.
            Result := Deadline_Reached;
            return;
         end if;
      end loop;
   end Wait_Reached;

end CuBit.GPU_Sessions;
