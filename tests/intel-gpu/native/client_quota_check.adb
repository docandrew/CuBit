with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Extent_Allocator;
procedure Client_Quota_Check is
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Ignore : Unsigned_64;
   function Owner return Boolean is (True);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = PID and then Stamp in 16#4947# | 16#4948# then Stamp else 0);
   package B is new Intel_GPU_Buffer_Requests (Session_Of, Owner);
   use type B.Words;
   package R renames Intel_GPU_Buffer_Reply;
   Object : B.Service;
   Calls : Natural := 0;
   procedure Check (Condition : Boolean; Detail : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL native client quota: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT); loop null; end loop;
      end if;
   end Check;
   function Allocate (CPU : Unsigned_64) return Unsigned_64 is
   begin
      Calls := Calls + 1;
      return syscall (SYSCALL_ALLOC_DMA, PID, 9, CPU, 3, 2 ** 32);
   end Allocate;
   package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
   Pool : A.Pool;
   Token : Unsigned_64 := 0;
   Reply_Words : B.Words;
   Handle : Unsigned_64;
   OK : Boolean;
   procedure Exchange (Endpoint : CapabilitySlot; Request : B.Words;
                       Fail_Backing : Boolean := False; Query : Boolean := False) is
      Msg, Response : Message := NULL_MESSAGE;
      From : Process_ID;
      Found, Consumed : Boolean;
      Ticket : B.Ticket;
      View : R.Extent_View;
      Result : R.Backing := (Ready => False);
      Receipt : aliased CompletionEntry;
      Deadline : Unsigned_64;
   begin
      Token := Token + 1;
      Msg.tag := ((if Query then B.Accounting_Label else B.Label), 4, 0, 0);
      Msg.words := [Request (0), Request (1), Request (2), Request (3)];
      Check (capSubmit (Endpoint, Msg, Token), "submit");
      Deadline := syscall (SYSCALL_GETTIME) + 10_000;
      loop
         Poll_Service_Request (From, Msg, Found); exit when Found;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "request deadline");
      end loop;
      Check (Unsigned_64 (From) = PID, "kernel sender");
      if Query then
         Ticket := 0;
         B.Query_Accounting (Object, Unsigned_64 (From), Msg.authorityTag, Msg.tag.label,
           Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
           [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Reply_Words);
      else
         B.Handle (Object, Unsigned_64 (From), Msg.authorityTag, Msg.tag.label,
           Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
           [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Reply_Words, Ticket);
      end if;
      if Ticket /= 0 then
         Check (saveReplyCap (58) = 1, "save reply");
         if not Fail_Backing then
            A.Acquire_Buffer (Pool, PID, B.Ticket_Slot (Ticket),
              Intel_GPU_Buffer_Backing.Page_Count (Request (2) / 4096),
              B.Ticket_Generation (Ticket), View, OK);
            Check (OK, "real backing"); Result := R.From_View (View);
         end if;
         B.Complete (Object, Ticket, Result, Reply_Words, Consumed);
         Check (Consumed, "completion");
      end if;
      Response.tag := ((if Query then B.Accounting_Label else B.Label), 4, 0, 0);
      Response.words := [Reply_Words (0), Reply_Words (1), Reply_Words (2), Reply_Words (3)];
      if Ticket /= 0 then
         Check (replyCap (58, Response) = 1, "saved delivery");
         Check (replyCap (58, Response) /= 1, "single delivery");
      else Check (reply (From, Response) = 1, "immediate delivery"); end if;
      loop
         exit when Poll_Completion (Receipt'Address) /= 0;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "reply deadline");
      end loop;
      Check (Receipt.token = Token and then Receipt.status = COMPLETION_OK and then
        Receipt.msg.tag = Response.tag and then Receipt.msg.words = Response.words, "reply contents");
      Check (Poll_Completion (Receipt'Address) = 0, "no duplicate completion");
   end Exchange;
begin
   debugPrint ("native client quota: real IPC and backing, forced startup policy (NO GPU/ISOLATION)" & ASCII.LF);
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 1, PID, 16#4947#, 3, 15) /= Unsigned_64'Last, "endpoint one");
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 1, PID, 16#4948#, 3, 16) /= Unsigned_64'Last, "endpoint two");
   B.Configure_Client_Budgets (Object, 12288, OK); Check (OK, "quota policy");
   Exchange (15, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.Unavailable, 1, 0, 0], "unknown account not zero usage");
   Exchange (15, [1, B.Create, 8192, 0]);
   Check (Reply_Words (0) = B.OK and Calls = 1, "initial create"); Handle := Reply_Words (2);
   Exchange (15, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.OK, 1, 12288, 8192], "own account query");
   Exchange (16, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.Unavailable, 1, 0, 0], "second endpoint cannot read first account");
   Exchange (15, [1, B.Create, 8192, 0]);
   Check (Reply_Words (0) = B.Unavailable and Calls = 1, "quota denial before backing");
   Check (B.Client_Usage (Object, 16#4947#).Charged = 8192, "denial preserves charge");
   Exchange (15, [1, B.Create, 4096, 0]);
   Check (Reply_Words (0) = B.OK and Reply_Words (2) /= Handle, "smaller create after denial");
   Check (Calls = 1 and B.Client_Usage (Object, 16#4947#).Charged = 12288,
     "remaining quota and retained extent used");
   Exchange (15, [1, B.Close, Handle, 0]); Check (Reply_Words (0) = B.OK, "close");
   Exchange (15, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.OK, 1, 12288, 12288], "closed name retains observable charge");
   Exchange (15, [1, B.Create, 4096, 0]);
   Check (Reply_Words (0) = B.Unavailable and Calls = 1, "close is not retirement");
   Exchange (16, [1, B.Create, 12288, 0], Fail_Backing => True);
   Check (Reply_Words (0) = B.Unavailable, "forced backing failure");
   Check (B.Client_Usage (Object, 16#4948#).Charged = 12288, "failed backing charge retained");
   Exchange (16, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.OK, 1, 12288, 12288], "failed allocation charge query");
   Exchange (16, [1, B.Create, 4096, 0]);
   Check (Reply_Words (0) = B.Unavailable and Calls = 1, "failure does not reopen quota");
   Check (B.Client_Usage (Object, 16#4947#).Charged = 12288, "independent account unchanged");
   Exchange (15, [1, 16#4948#, 0, 0], Query => True);
   Check (Reply_Words = [B.Bad_Request, 1, 0, 0] and Calls = 1, "no caller-selected account");
   Check (Token = 13, "request count");
   debugPrint ("TEST: PASS native client quota thirteen IPC replies own accounting denial recovery retained charges (NO GPU/ISOLATION)" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT); loop null; end loop;
end Client_Quota_Check;
