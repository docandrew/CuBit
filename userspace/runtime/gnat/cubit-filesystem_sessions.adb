pragma Ada_2022;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Channel_Protocol;
with CuBit.Published_Clock;
with CuBit.Filesystems;

package body CuBit.Filesystem_Sessions is

   use type Q.Token;

   --  Spins before a blocking wait: back-to-back answers come within the
   --  service's poll window.
   Answer_Spins : constant := 256;

   --  The compiler keeps ring writes before the index stores that publish
   --  them (x86 keeps the order in hardware).
   procedure Barrier;
   procedure Barrier is
   begin
      System.Machine_Code.Asm ("", Volatile => True, Clobber => "memory");
   end Barrier;

   function Client_Word (S : Session; Offset : Natural) return System.Address is
     (To_Address (Integer_Address (S.Queue.Own_Base)) + Storage_Offset (Offset));
   function Server_Word (S : Session; Offset : Natural) return System.Address is
     (To_Address (Integer_Address (S.Queue.Peer_Base)) + Storage_Offset (Offset));

   procedure Open
     (S : in out Session; Endpoint : CuBit.Messages.CapabilitySlot;
      Arena_Pages : Positive; Opened : out Boolean)
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
        (Endpoint, (FQ.TRANSFER_CONTRACT with delta Buffers => Arena_Pages),
         CuBit.Channels.Producing, S.Transfer, Result, Ignore_Refusal,
         Connector => FQ.Transfer_Connector);
      if Result /= CuBit.Channels.Opened then
         return;
      end if;
      CuBit.Channels.Open
        (Endpoint, FQ.QUEUE_CONTRACT, CuBit.Channels.Producing, S.Queue, Result,
         Ignore_Refusal, Connector => FQ.Queue_Connector);
      if Result /= CuBit.Channels.Opened then
         CuBit.Channels.Close (S.Transfer);
         return;
      end if;
      S.Endpoint := Endpoint;
      S.Client := (others => <>);
      S.Kicked := 0;
      S.Opened := True;
      Opened := True;
   end Open;

   procedure Close (S : in out Session) is
   begin
      if S.Opened then
         CuBit.Channels.Close (S.Events);
         CuBit.Channels.Close (S.Queue);
         CuBit.Channels.Close (S.Transfer);
         S.Opened := False;
      end if;
   end Close;

   function Is_Open (S : Session) return Boolean is (S.Opened);

   function Arena (S : Session) return System.Address is
     (if S.Opened then CuBit.Channels.Buffer_Address (S.Transfer, 0) else System.Null_Address);

   function Arena_Bytes (S : Session) return Unsigned_64 is
     (if S.Opened then Unsigned_64 (S.Transfer.Item.Buffers) * FQ.Page_Bytes else 0);

   function Client_Region (S : Session) return System.Address is
     (if S.Opened then Client_Word (S, 0) else System.Null_Address);

   function Server_Region (S : Session) return System.Address is
     (if S.Opened then Server_Word (S, 0) else System.Null_Address);

   function Namespace (S : Session) return Unsigned_32 is
   begin
      if not S.Opened then
         return 0;
      end if;
      declare
         Generation : constant Unsigned_32 with Import, Volatile,
           Address => Server_Word (S, FQ.Server_Namespace_At);
      begin
         return Generation;
      end;
   end Namespace;

   function Can_Submit (S : in out Session) return Boolean is
   begin
      if not S.Opened then
         return False;
      end if;
      declare
         Taken : constant Unsigned_32 with Import, Volatile,
           Address => Server_Word (S, FQ.Server_Taken_At);
         Accepted : Boolean;
      begin
         Q.Accept_Taken (S.Client, Q.Submissions.Index (Taken), Accepted);
      end;
      return Q.Can_Submit (S.Client);
   end Can_Submit;

   function Outstanding (S : Session) return Natural is (S.Client.Pending);

   procedure Submit (S : in out Session; Item : FQ.Request; Tag : out Token) is
      Requests : Q.Submissions.Ring with Import,
        Address => Client_Word (S, FQ.Client_Requests_At);
      Submitted : Unsigned_32 with Import, Volatile,
        Address => Client_Word (S, FQ.Client_Submitted_At);
      Wake : constant Unsigned_32 with Import, Volatile,
        Address => Server_Word (S, FQ.Server_Wake_At);
      Armed : Unsigned_32;
   begin
      Tag := 0;
      if not Can_Submit (S) then
         return;
      end if;
      S.Next_Tag := S.Next_Tag + 1;
      Tag := S.Next_Tag;
      Q.Submit (S.Client, Requests, Tag, Item);
      Barrier;
      Submitted := Unsigned_32 (S.Client.Requests.Produced);
      Barrier;
      --  The count before the wake word: kick only a service that sleeps,
      --  once per arming.
      Armed := Wake;
      if Armed /= 0 and then Armed /= S.Kicked then
         S.Kicked := Armed;
         CuBit.Channels.Kick (S.Queue);
      end if;
   end Submit;

   function Answers_Waiting (S : in out Session) return Boolean is
   begin
      if not S.Opened then
         return False;
      end if;
      declare
         Answered : constant Unsigned_32 with Import, Volatile,
           Address => Server_Word (S, FQ.Server_Answered_At);
         Accepted : Boolean;
      begin
         Q.Completions.Accept_Produced
           (S.Client.Answers, Q.Completions.Index (Answered), Accepted);
      end;
      return S.Client.Answers.Available > 0;
   end Answers_Waiting;

   procedure Reap (S : in out Session; Item : out Q.Completion; Got : out Boolean) is
   begin
      Item := (Tag => 0, Answer => (Status => 0, Reserved => 0, Value => 0, Spare => 0));
      Got := False;
      if not Answers_Waiting (S) then
         return;
      end if;
      declare
         Answers : constant Q.Completions.Ring with Import,
           Address => Server_Word (S, FQ.Server_Answers_At);
         Reaped : Unsigned_32 with Import, Volatile,
           Address => Client_Word (S, FQ.Client_Reaped_At);
      begin
         Barrier;
         Q.Reap (S.Client, Answers, Item, Got);
         Barrier;
         Reaped := Unsigned_32 (S.Client.Answers.Consumed);
      end;
   end Reap;

   procedure Open_Events (S : in out Session; Opened : out Boolean) is
      use type CuBit.Channels.Open_Result;
      Result : CuBit.Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      Opened := S.Events.Active;
      if Opened or else not S.Opened then
         return;
      end if;
      CuBit.Channels.Open
        (S.Endpoint, FQ.EVENT_CONTRACT, CuBit.Channels.Consuming, S.Events, Result,
         Ignore_Refusal, Connector => FQ.Event_Connector);
      Opened := Result = CuBit.Channels.Opened;
   end Open_Events;

   function Events_Open (S : Session) return Boolean is (S.Events.Active);

   procedure Take_Event
     (S : in out Session; Item : out CuBit.Filesystem_Events.Event;
      Name : out CuBit.Filesystem_Events.Name_Bytes;
      Length : out CuBit.Filesystem_Events.Name_Length; Result : out Event_Result)
   is
      package FE renames CuBit.Filesystem_Events;
      use type CuBit.Channels.Take_Result;
      Bytes : FE.Record_Bytes := [others => 0];
      Used : Natural;
      Outcome : CuBit.Channels.Take_Result;
      OK : Boolean;
   begin
      Item := (others => <>);
      Name := [others => 0];
      Length := 0;
      Result := Empty;
      if not S.Events.Active then
         return;
      end if;
      CuBit.Channels.Take (S.Events, Bytes'Address, Bytes'Length, Used, Outcome);
      if Outcome = CuBit.Channels.Empty then
         return;
      elsif Outcome /= CuBit.Channels.Taken then
         Result := Malformed;
         return;
      end if;
      FE.Decode (Bytes, Used, Item, Name, Length, OK);
      Result := (if OK then Event_Result'(Taken) else Event_Result'(Malformed));
   end Take_Event;

   function Wake_Armed (S : Session) return Boolean is
     (not CuBit.Async_Requests.Drained (S.Wake));

   procedure Arm_Wake
     (S : in out Session; Wake_Token : CuBit.Async_Requests.Token; Accepted : out Boolean)
   is
      Request : constant CuBit.Messages.Message :=
        (tag => (label => FQ.OP_FS_WAKE, length => 0, flags => 0, reserved => 0),
         authorityTag => 0, words => [others => 0]);
   begin
      Accepted := False;
      if not S.Opened or else not CuBit.Async_Requests.Can_Reserve (S.Wake, Wake_Token) then
         return;
      end if;
      CuBit.Async_Requests.Reserve (S.Wake, Wake_Token, Accepted);
      if not Accepted then
         return;
      end if;
      Accepted := CuBit.Messages.capSubmit (S.Endpoint, Request, Wake_Token);
      CuBit.Async_Requests.Submitted (S.Wake, Accepted);
   end Arm_Wake;

   procedure Complete_Wake
     (S : in out Session; Receipt : CuBit.Messages.CompletionEntry;
      Consumed, Woken : out Boolean)
   is
   begin
      Woken := False;
      CuBit.Async_Requests.Capture (S.Wake, Receipt.token, Receipt.valid, Consumed);
      if not Consumed then
         return;
      end if;
      CuBit.Async_Requests.Release (S.Wake);
      Woken := Receipt.status = CuBit.Messages.COMPLETION_OK and then
               Receipt.msg.tag.label = CuBit.Filesystems.REPLY_OK;
   end Complete_Wake;

   procedure Wait_Answer
     (S : in out Session; Deadline : Unsigned_64; Result : out Wait_Result)
   is
      Request : CuBit.Messages.Message;
      Reply : CuBit.Messages.MessageTag;
   begin
      Result := Queue_Ended;
      if not S.Opened then
         return;
      end if;
      for Look in 1 .. Answer_Spins loop
         if Answers_Waiting (S) then
            Result := Answer_Waiting;
            return;
         end if;
      end loop;
      loop
         if Answers_Waiting (S) then
            Result := Answer_Waiting;
            return;
         end if;
         Request :=
           (tag => (label => FQ.OP_FS_WAKE, length => 0, flags => 0, reserved => 0),
            authorityTag => 0, words => [others => 0]);
         Reply := CuBit.Messages.capCall (S.Endpoint, Request, Deadline);
         if Reply.label = CuBit.Messages.REPLY_TIMEOUT then
            Result := (if Answers_Waiting (S) then Answer_Waiting else Deadline_Reached);
            return;
         elsif Reply.label /= CuBit.Filesystems.REPLY_OK then
            Result := Queue_Ended;
            return;
         elsif not Answers_Waiting (S) and then
           CuBit.Published_Clock.Milliseconds >= Deadline
         then
            --  The deadline holds even against a service that answers
            --  wakes with nothing waiting.
            Result := Deadline_Reached;
            return;
         end if;
      end loop;
   end Wait_Answer;

end CuBit.Filesystem_Sessions;
