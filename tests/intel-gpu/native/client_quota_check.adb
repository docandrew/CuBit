with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Handles;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Control;
with CuBit.Handles.IPC;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Extent_Allocator;
procedure Client_Quota_Check is
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   -- Runs in devmgr's place: boot roots allocate, and two badged copies of
   -- the self endpoint stand for two client sessions.
   Roots : constant CuBit.Handles.Boot_Roots.Roots := CuBit.Handles.Boot_Roots.Get;
   function Session_Endpoint (Tag : Unsigned_64) return CuBit.Handles.Endpoint is
     (CuBit.Handles.Control.Derive
        (CuBit.Handles.Badged (Roots.Self_Grantable, CuBit.Handles.Read_Write, Tag)));
   First : constant CuBit.Handles.Endpoint := Session_Endpoint (16#4947#);
   Second : constant CuBit.Handles.Endpoint := Session_Endpoint (16#4948#);
   Saved_Reply : CuBit.Handles.Reply;
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
      return CuBit.Handles.Control.Allocate_DMA
        (CuBit.Process_IDs.From_Word (PID), Roots.Process_Root, 9, CPU, 3, 2 ** 32);
   end Allocate;
   package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
   Pool : A.Pool;
   Token : Unsigned_64 := 0;
   Reply_Words : B.Words;
   Handle : Unsigned_64;
   OK : Boolean;
   procedure Exchange (Target : CuBit.Handles.Endpoint; Request : B.Words;
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
      Msg.tag := (label => (if Query then B.Accounting_Label else B.Label), length => 4,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words := [Request (0), Request (1), Request (2), Request (3)];
      Check (capSubmit (Target, Msg, Token), "submit");
      Deadline := syscall (SYSCALL_GETTIME) + 10_000;
      loop
         Poll_Service_Request (From, Msg, Found); exit when Found;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "request deadline");
      end loop;
      Check (CuBit.Process_IDs.To_Word (From) = PID, "kernel sender");
      if Query then
         Ticket := 0;
         B.Query_Accounting (Object, CuBit.Process_IDs.To_Word (From), Msg.authorityTag, Msg.tag.label,
           Msg.tag.length, Msg.tag.protocol, Msg.tag.reserved,
           [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Reply_Words);
      else
         B.Handle (Object, CuBit.Process_IDs.To_Word (From), Msg.authorityTag, Msg.tag.label,
           Msg.tag.length, Msg.tag.protocol, Msg.tag.reserved,
           [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Reply_Words, Ticket);
      end if;
      if Ticket /= 0 then
         declare
            Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
         begin
            Check (Saved.Saved, "save reply");
            Saved_Reply := Saved.Handle;
         end;
         if not Fail_Backing then
            A.Acquire_Buffer (Pool, PID, B.Ticket_Slot (Ticket),
              Intel_GPU_Buffer_Backing.Page_Count (Request (2) / 4096),
              B.Ticket_Generation (Ticket), View, OK);
            Check (OK, "real backing"); Result := R.From_View (View);
         end if;
         B.Complete (Object, Ticket, Result, Reply_Words, Consumed);
         Check (Consumed, "completion");
      end if;
      Response.tag := (label => (if Query then B.Accounting_Label else B.Label), length => 4,
                       protocol => 0, ipc => <>, reserved => 0);
      Response.words := [Reply_Words (0), Reply_Words (1), Reply_Words (2), Reply_Words (3)];
      if Ticket /= 0 then
         Check (CuBit.Handles.IPC.Answer (Saved_Reply, Response), "saved delivery");
         Check (not CuBit.Handles.IPC.Answer (Saved_Reply, Response), "single delivery");
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
   Check (Roots.Valid and CuBit.Handles.Valid (First), "endpoint one");
   Check (CuBit.Handles.Valid (Second), "endpoint two");
   B.Configure_Client_Budgets (Object, 12288, OK); Check (OK, "quota policy");
   Exchange (First, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.Unavailable, 1, 0, 0], "unknown account not zero usage");
   Exchange (First, [1, B.Create, 8192, 0]);
   Check (Reply_Words (0) = B.OK and Calls = 1, "initial create"); Handle := Reply_Words (2);
   Exchange (First, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.OK, 1, 12288, 8192], "own account query");
   Exchange (Second, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.Unavailable, 1, 0, 0], "second endpoint cannot read first account");
   Exchange (First, [1, B.Create, 8192, 0]);
   Check (Reply_Words (0) = B.Unavailable and Calls = 1, "quota denial before backing");
   Check (B.Client_Usage (Object, 16#4947#).Charged = 8192, "denial preserves charge");
   Exchange (First, [1, B.Create, 4096, 0]);
   Check (Reply_Words (0) = B.OK and Reply_Words (2) /= Handle, "smaller create after denial");
   Check (Calls = 1 and B.Client_Usage (Object, 16#4947#).Charged = 12288,
     "remaining quota and retained extent used");
   Exchange (First, [1, B.Close, Handle, 0]); Check (Reply_Words (0) = B.OK, "close");
   Exchange (First, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.OK, 1, 12288, 12288], "closed name retains observable charge");
   Exchange (First, [1, B.Create, 4096, 0]);
   Check (Reply_Words (0) = B.Unavailable and Calls = 1, "close is not retirement");
   Exchange (Second, [1, B.Create, 12288, 0], Fail_Backing => True);
   Check (Reply_Words (0) = B.Unavailable, "forced backing failure");
   Check (B.Client_Usage (Object, 16#4948#).Charged = 12288, "failed backing charge retained");
   Exchange (Second, [1, 0, 0, 0], Query => True);
   Check (Reply_Words = [B.OK, 1, 12288, 12288], "failed allocation charge query");
   Exchange (Second, [1, B.Create, 4096, 0]);
   Check (Reply_Words (0) = B.Unavailable and Calls = 1, "failure does not reopen quota");
   Check (B.Client_Usage (Object, 16#4947#).Charged = 12288, "independent account unchanged");
   Exchange (First, [1, 16#4948#, 0, 0], Query => True);
   Check (Reply_Words = [B.Bad_Request, 1, 0, 0] and Calls = 1, "no caller-selected account");
   Check (Token = 13, "request count");
   debugPrint ("TEST: PASS native client quota thirteen IPC replies own accounting denial recovery retained charges (NO GPU/ISOLATION)" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT); loop null; end loop;
end Client_Quota_Check;
