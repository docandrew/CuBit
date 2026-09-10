with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Network_Authority; use CuBit.Network_Authority;

procedure Main is
   Connect_Slot : constant CapabilitySlot := 30;
   Listen_Slot : constant CapabilitySlot := 31;
   Inspect_Slot : constant CapabilitySlot := 11;
   OK_Label : constant Unsigned_32 := 16#F000#;
   Error_Label : constant Unsigned_32 := 16#F001#;
   Open_Label : constant Unsigned_32 := 16#0420#;
   Write_Label : constant Unsigned_32 := 16#0421#;
   Read_Label : constant Unsigned_32 := 16#0422#;
   Shut_Label : constant Unsigned_32 := 16#0423#;
   Passed : Boolean := True;
   Request : Message;
   Reply_Tag : MessageTag;
   Listener, Replacement, Channel : Unsigned_64;
   Previous_Channel : Unsigned_64 := 0;
   Inspection : array (0 .. 5) of Unsigned_64 := [others => 0];
   Result : Unsigned_64;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then Passed := False; end if;
      debugPrint ("network-check: " & Name & (if Condition then " PASS" else " FAIL") & ASCII.LF);
   end Check;
   procedure Bind (Slot : CapabilitySlot; Address, Port : Unsigned_64) is
   begin
      Request := NULL_MESSAGE;
      Request.tag.label := OP_BIND; Request.tag.length := 2;
      Request.words (0) := Address; Request.words (1) := Port;
      Reply_Tag := capCall (Slot, Request);
   end Bind;
   procedure Close_Listener (Slot : CapabilitySlot; Handle : Unsigned_64) is
   begin
      Request := NULL_MESSAGE; Request.tag.label := OP_CLOSE_LISTENER;
      Request.tag.length := 1; Request.words (0) := Handle;
      Reply_Tag := capCall (Slot, Request);
   end Close_Listener;
begin
   Bind (Inspect_Slot, 16#0A00_020F#, 8080);
   Check (Reply_Tag.label = Error_Label, "general endpoint cannot listen");
   Result := syscall (SYSCALL_INSPECT_CAPABILITY, syscall (SYSCALL_GETPID), Listen_Slot,
                      Unsigned_64 (To_Integer (Inspection'Address)));
   if Result = 1 and Inspection (0) = 0 then
      Bind (Listen_Slot, 16#0A00_020F#, 8080);
      Check (Reply_Tag.label /= OK_Label, "manifest does not approve itself");
      if Passed then debugPrint ("TEST: PASS network-unapproved" & ASCII.LF); end if;
      return;
   end if;
   Check (Result = 1 and Inspection (0) = 1 and Inspection (2) >= First_Grant_Tag,
          "approved endpoint carries scope tag");
   Bind (Connect_Slot, 16#0A00_020F#, 8080);
   Check (Reply_Tag.label = Error_Label, "outbound cannot listen");
   Bind (Listen_Slot, 16#0A00_020F#, 8081);
   Check (Reply_Tag.label = Error_Label, "wrong listen port denied");
   Bind (Listen_Slot, 16#0A00_0210#, 8080);
   Check (Reply_Tag.label = Error_Label, "wrong listen address denied");
   Bind (Listen_Slot, 0, 8080);
   Check (Reply_Tag.label = Error_Label, "wildcard listen denied");
   Bind (Listen_Slot, 16#0A00_020F#, 65536 + 8080);
   Check (Reply_Tag.label = Error_Label, "port truncation denied");
   Bind (Listen_Slot, 16#0A00_020F#, 8080);
   Check (Reply_Tag.label = OK_Label, "exact listener admitted");
   Listener := Request.words (0);
   Bind (Listen_Slot, 16#0A00_020F#, 8080);
   Check (Reply_Tag.label = Error_Label, "duplicate bind denied");
   Close_Listener (Connect_Slot, Listener);
   Check (Reply_Tag.label = Error_Label, "wrong grant cannot close listener");
   Close_Listener (Listen_Slot, Listener);
   Check (Reply_Tag.label = OK_Label, "owner closes listener");
   Bind (Listen_Slot, 16#0A00_020F#, 8080);
   Replacement := Request.words (0);
   Check (Reply_Tag.label = OK_Label and Replacement /= Listener, "listener handle not reused");
   Close_Listener (Listen_Slot, Listener);
   Check (Reply_Tag.label = Error_Label, "stale listener rejected");
   Close_Listener (Listen_Slot, Replacement);
   Check (Reply_Tag.label = OK_Label, "replacement listener closes");

   Request := NULL_MESSAGE; Request.tag.label := OP_INSTALL_SCOPE;
   Request.tag.length := 3; Request.authorityTag := Policy_Authority_Tag;
   Request.words (0) := syscall (SYSCALL_GETPID);
   Request.words (2) := Descriptor (Broad_Outbound_TCP);
   Reply_Tag := capCall (Inspect_Slot, Request);
   Check (Reply_Tag.label = Error_Label, "forged policy tag rejected");
   Request := NULL_MESSAGE; Request.tag.label := 16#0432#;
   Reply_Tag := capCall (Connect_Slot, Request);
   Check (Reply_Tag.label = Error_Label, "outbound cannot configure network");

   declare
      Allocation : constant Unsigned_64 := syscall (SYSCALL_SBRK, 4096);
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Success : Boolean;
      Buffer : String (1 .. 4096) with Import,
        Address => To_Address (Integer_Address (Allocation));
      Async_Mode : Boolean := True;
      Token : Unsigned_64 := 16#A531_8000_0000_0000#;
      function Exchange (Slot : CapabilitySlot) return MessageTag is
         Completion : CompletionEntry;
         Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
         Ignored : Unsigned_64;
      begin
         if not Async_Mode then return capCall (Slot, Request); end if;
         Token := Token + 1;
         if not capSubmit (Slot, Request, Token) then
            Check (False, "async request submitted");
            return NULL_TAG;
         end if;
         loop
            exit when Poll_Completion (Completion'Address) = 1;
            if syscall (SYSCALL_GETTIME) - Started > 10_000 then
               Check (False, "async completion deadline");
               return NULL_TAG;
            end if;
            Ignored := syscall (SYSCALL_SLEEP, 1);
         end loop;
         Check (Completion.token = Token and then
                Completion.status = COMPLETION_OK,
                "async completion identity");
         Request := Completion.msg;
         Check (Poll_Completion (Completion'Address) = 0,
                "async completion exactly once");
         return Request.tag;
      end Exchange;
      procedure Open (Name : String; Generation : Unsigned_64 := 0) is
      begin
         Buffer (1 .. Name'Length) := Name;
         Request := NULL_MESSAGE; Request.tag.label := Open_Label;
         Request.tag.length := Unsigned_8 (Name'Length);
         Request.words (0) := Reference.slot; Request.words (1) := 4096;
         Request.words (3) := (if Generation = 0 then Reference.generation else Generation);
         Reply_Tag := Exchange (Connect_Slot);
      end Open;
   begin
      Check (Allocation /= Unsigned_64'Last, "transfer allocation");
      if Allocation = Unsigned_64'Last then return; end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (Connect_Slot, Buffer'Address, 1, True, Reference, Success);
      Check (Success, "transfer grant");
      if not Success then return; end if;
      Open ("@net:tcp:10.0.3.2:18443");
      Check (Reply_Tag.label = Error_Label, "outbound wrong prefix denied");
      Open ("@net:tcp:10.0.2.2:18444");
      Check (Reply_Tag.label = Error_Label, "outbound wrong port denied");
      Open ("@net:tcp:example.com:18443");
      Check (Reply_Tag.label = Error_Label, "undeclared DNS denied");
      Open ("@net:tcp:10.0.2.2:18443", Reference.generation + 1);
      Check (Reply_Tag.label = Error_Label, "wrong transfer generation denied");
      for Round in 1 .. 12 loop
         --  Half the lifetimes retain synchronous coverage; the other half
         --  exercise the full async open/write/read/close path used by NetSurf.
         Async_Mode := Round > 6;
         Open ("@net:tcp:10.0.2.2:18443");
         Check (Reply_Tag.label = OK_Label, "permitted outbound connects");
         if Async_Mode then
            Check (Reply_Tag.label = OK_Label, "async outbound connects");
         end if;
         if Reply_Tag.label = OK_Label then
            Channel := Request.words (0);
            Check (Channel > Previous_Channel, "channel handle is fresh");
            if Previous_Channel /= 0 then
               Request := NULL_MESSAGE; Request.tag.label := Write_Label;
               Request.tag.length := 3; Request.words (0) := Previous_Channel;
               Reply_Tag := capCall (Connect_Slot, Request);
               Check (Reply_Tag.label = Error_Label, "stale channel cannot write replacement");
               Request := NULL_MESSAGE; Request.tag.label := Shut_Label;
               Request.tag.length := 1; Request.words (0) := Previous_Channel;
               Reply_Tag := capCall (Connect_Slot, Request);
               Check (Reply_Tag.label = Error_Label, "stale channel cannot close replacement");
            end if;
            Request := NULL_MESSAGE; Request.tag.label := Read_Label;
            Request.tag.length := 3; Request.words (0) := Unsigned_64'Last;
            Reply_Tag := capCall (Connect_Slot, Request);
            Check (Reply_Tag.label = Error_Label, "oversized opaque handle denied without truncation");
            Buffer (1 .. 4) := "PING";
            Request := NULL_MESSAGE; Request.tag.label := Write_Label;
            Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 4;
            Reply_Tag := capCall (Listen_Slot, Request);
            Check (Reply_Tag.label = Error_Label, "wrong grant cannot write channel");
            -- capCall overwrites Request, so rebuild after the intentional denial.
            Request := NULL_MESSAGE; Request.tag.label := Write_Label;
            Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 4;
            Reply_Tag := Exchange (Connect_Slot);
            Check (Reply_Tag.label = OK_Label and Request.words (0) = 4, "authorized write");
            Request := NULL_MESSAGE; Request.tag.label := Read_Label;
            Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 4;
            Reply_Tag := Exchange (Connect_Slot);
            Check (Reply_Tag.label = OK_Label and Request.words (0) = 4 and Buffer (1 .. 4) = "PONG",
                   "authorized reply received");
            if Async_Mode then
               Check (Reply_Tag.label = OK_Label and then Buffer (1 .. 4) = "PONG",
                      "async outbound round trip");
            end if;
            Request := NULL_MESSAGE; Request.tag.label := Shut_Label;
            Request.tag.length := 1; Request.words (0) := Channel;
            Reply_Tag := Exchange (Connect_Slot);
            Check (Reply_Tag.label = OK_Label, "authorized channel closes");
            Previous_Channel := Channel;
         end if;
      end loop;
      Check (Previous_Channel > 8, "channel handles are not bounded table indices");
      CuBit.Memory_Grants.Revoke (Reference, Success);
      Check (Success, "transfer revoke");
   end;
   declare
      Allocation : constant Unsigned_64 := syscall (SYSCALL_SBRK, 4096);
      Buffer : String (1 .. 4096) with Import,
        Address => To_Address (Integer_Address (Allocation));
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Success : Boolean;
      Started : Unsigned_64;
      Received, Count : Natural;
      Payload : String (1 .. 8);
      Completion : CompletionEntry;
      procedure Prepare_Accept (Generation : Unsigned_64 := 0) is
      begin
         Request := NULL_MESSAGE; Request.tag.label := OP_ACCEPT;
         Request.tag.length := 4; Request.words (0) := Listener;
         Request.words (1) := Reference.slot; Request.words (2) := 4096;
         Request.words (3) := (if Generation = 0 then Reference.generation else Generation);
      end Prepare_Accept;
      procedure Accept_Channel (Slot : CapabilitySlot; Generation : Unsigned_64 := 0) is
      begin
         Prepare_Accept (Generation);
         Reply_Tag := capCall (Slot, Request);
      end Accept_Channel;
   begin
      Check (Allocation /= Unsigned_64'Last, "inbound transfer allocation");
      if Allocation = Unsigned_64'Last then return; end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (Listen_Slot, Buffer'Address, 1, True, Reference, Success);
      Check (Success, "inbound transfer grant");
      if not Success then return; end if;
      Bind (Listen_Slot, 16#0A00_020F#, 8080);
      Check (Reply_Tag.label = OK_Label, "inbound listener binds");
      Listener := Request.words (0);
      Accept_Channel (Connect_Slot);
      Check (Reply_Tag.label = Error_Label, "outbound grant cannot accept");
      Accept_Channel (Listen_Slot, Reference.generation + 1);
      Check (Reply_Tag.label = Error_Label, "accept rejects stale transfer");
      Started := syscall (SYSCALL_GETTIME);
      Accept_Channel (Listen_Slot);
      Check (Reply_Tag.label = Error_Label and then syscall (SYSCALL_GETTIME) - Started >= 30_000,
             "accept deadline wakes without incoming traffic");
      Prepare_Accept;
      Success := capSubmit (Listen_Slot, Request, 101);
      Check (Success, "deferred accept submitted");
      if not Success then return; end if;
      Accept_Channel (Listen_Slot);
      Check (Reply_Tag.label = Error_Label, "duplicate pending accept denied");
      Check (Poll_Completion (Completion'Address) = 0,
             "accept remains pending until listener close");
      Close_Listener (Listen_Slot, Listener);
      Check (Reply_Tag.label = OK_Label, "listener closes with pending accept");
      Result := waitCompletion (Completion'Address, 1, 1);
      Check (Result = 1 and then Completion.token = 101 and then
             Completion.status = COMPLETION_OK and then Completion.msg.tag.label = Error_Label,
             "listener close completes pending accept exactly once");
      Check (Poll_Completion (Completion'Address) = 0, "no duplicate accept completion");
      Bind (Listen_Slot, 16#0A00_020F#, 8080);
      Check (Reply_Tag.label = OK_Label, "listener rebinds after pending accept cleanup");
      Listener := Request.words (0);
      debugPrint ("network-check: backlog-expiry ready" & ASCII.LF);
      Result := syscall (SYSCALL_SLEEP, 7000);
      --  Host verifies two never-accepted connections are reset by the service's
      --  five-second timer, before this explicit close can cause their teardown.
      Close_Listener (Listen_Slot, Listener);
      Check (Reply_Tag.label = OK_Label, "expired backlog listener closes");
      Bind (Listen_Slot, 16#0A00_020F#, 8080);
      Check (Reply_Tag.label = OK_Label, "listener rebinds after backlog expiry");
      Listener := Request.words (0);
      --  A new acquisition below must succeed after the timed-out one released.
      for Round in 1 .. 4 loop
         debugPrint ("network-check: inbound ready" & Round'Image & ASCII.LF);
         Accept_Channel (Listen_Slot);
         Check (Reply_Tag.label = OK_Label, "inbound handshake accepted");
         if Reply_Tag.label /= OK_Label then return; end if;
         Channel := Request.words (0);
         Check (Channel > Previous_Channel, "accepted channel identity is fresh");
         --  The accepted connection survives closure of its listening endpoint.
         Close_Listener (Listen_Slot, Listener);
         Check (Reply_Tag.label = OK_Label, "listener closes independently of accepted channel");
         Accept_Channel (Listen_Slot);
         Check (Reply_Tag.label = Error_Label, "closed listener cannot accept");
         Request := NULL_MESSAGE; Request.tag.label := Read_Label;
         Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 8;
         Reply_Tag := capCall (Connect_Slot, Request);
         Check (Reply_Tag.label = Error_Label, "outbound grant cannot read accepted channel");
         Received := 0;
         while Received < Payload'Length loop
            Request := NULL_MESSAGE; Request.tag.label := Read_Label;
            Request.tag.length := 3; Request.words (0) := Channel;
            Request.words (2) := Unsigned_64 (Payload'Length - Received);
            Reply_Tag := capCall (Listen_Slot, Request);
            if Reply_Tag.label /= OK_Label or else Request.words (0) = 0 or else
              Request.words (0) > Unsigned_64 (Payload'Length - Received)
            then
               Check (False, "inbound bounded read"); return;
            end if;
            Count := Natural (Request.words (0));
            Payload (Received + 1 .. Received + Count) := Buffer (1 .. Count);
            Received := Received + Count;
         end loop;
         Check (Payload = "CuBitIPC", "fragmented inbound bytes retained");
         Request := NULL_MESSAGE; Request.tag.label := Read_Label;
         Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 1;
         Reply_Tag := capCall (Listen_Slot, Request);
         Check (Reply_Tag.label = 16#F006#, "peer half-close reports EOF");
         Buffer (1 .. 8) := "ACCEPTED";
         Request := NULL_MESSAGE; Request.tag.label := Write_Label;
         Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 8;
         Reply_Tag := capCall (Listen_Slot, Request);
         Check (Reply_Tag.label = OK_Label and Request.words (0) = 8,
                "accepted reply after peer half-close");
         Request := NULL_MESSAGE; Request.tag.label := Shut_Label;
         Request.tag.length := 1; Request.words (0) := Channel;
         Reply_Tag := capCall (Listen_Slot, Request);
         Check (Reply_Tag.label = OK_Label, "accepted channel closes");
         Previous_Channel := Channel;
         if Round < 4 then
            Bind (Listen_Slot, 16#0A00_020F#, 8080);
            Check (Reply_Tag.label = OK_Label, "inbound listener rebinds");
            Listener := Request.words (0);
         end if;
      end loop;
      CuBit.Memory_Grants.Revoke (Reference, Success);
      Check (Success, "inbound transfer revoke");
   end;
   if Passed then debugPrint ("TEST: PASS network-authority" & ASCII.LF); end if;
end Main;
