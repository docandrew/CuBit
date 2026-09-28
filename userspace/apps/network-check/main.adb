with Interfaces; use Interfaces;
with CCL_Manifest_Bindings;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Net_Channels;
with CuBit.Net_Address;
with CuBit.Network_Authority; use CuBit.Network_Authority;

procedure Main is
   Connect_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Test_Connect;
   Listen_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Test_Listen;
   Datagram_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Test_Datagram;
   Inspect_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Network;
   Resolve_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Test_Resolve;
   Refused_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Test_Refused;
   Resolve_Label : constant Unsigned_32 := 16#0410#;
   OK_Label : constant Unsigned_32 := 16#F000#;
   Error_Label : constant Unsigned_32 := 16#F001#;
   Open_Label : constant Unsigned_32 := 16#0420#;
   Write_Label : constant Unsigned_32 := 16#0421#;
   Shut_Label : constant Unsigned_32 := 16#0423#;
   package Channels renames CuBit.Net_Channels;
   Ring_Size : constant := 4_096;
   Stream_Bytes : constant := Channels.Layout.Header_Bytes + 2 * Ring_Size;
   --  One arena serves every scope: buffer 0 the stream under test, 1 and 2
   --  the declared-capacity checks, 3 a listener.
   Arena_Buffers : constant := 4;
   Listener_Buffer : constant := 3;
   Listener_Bit : constant := 5;
   Arena : Channels.Arena;
   Listener_Stream : Channels.Stream;
   Wait_Token : Unsigned_64 := 16#5A17_0000_0000_0000#;
   function Next_Wait return Unsigned_64 is
   begin
      Wait_Token := Wait_Token + 1;
      return Wait_Token;
   end Next_Wait;
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
   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Unsigned_64'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;
   --  OPEN a listener on Address:Port in arena buffer Buffer.
   procedure Bind (Slot : CapabilitySlot; Address, Port : Unsigned_64;
                   Buffer : Natural := Listener_Buffer) is
      Target : constant String :=
        "@net:tcp-listen:" & Decimal (Shift_Right (Address, 24) and 255) & "." &
        Decimal (Shift_Right (Address, 16) and 255) & "." &
        Decimal (Shift_Right (Address, 8) and 255) & "." & Decimal (Address and 255) &
        ":" & Decimal (Port);
   begin
      Channels.Prepare (Listener_Stream, Arena, Buffer, Listener_Bit, Slot);
      declare
         View : String (1 .. Target'Length) with Import,
           Address => Listener_Stream.Base + Storage_Offset (Channels.Layout.Target_At);
      begin
         View := Target;
      end;
      Request := NULL_MESSAGE; Request.tag.label := Open_Label;
      Request.tag.length := Unsigned_8 (Target'Length);
      Request.words (0) := Arena.Handle; Request.words (1) := Unsigned_64 (Buffer);
      Reply_Tag := capCall (Slot, Request);
      if Reply_Tag.label = OK_Label then
         Listener_Stream.Handle := Request.words (0);
      end if;
   end Bind;
   --  A listener closes like any channel.
   procedure Close_Listener (Slot : CapabilitySlot; Handle : Unsigned_64) is
   begin
      Request := NULL_MESSAGE; Request.tag.label := Shut_Label;
      Request.tag.length := 1; Request.words (0) := Handle;
      Reply_Tag := capCall (Slot, Request);
   end Close_Listener;
begin
   Result := syscall (SYSCALL_INSPECT_CAPABILITY, syscall (SYSCALL_GETPID), Listen_Slot,
                      Unsigned_64 (To_Integer (Inspection'Address)));
   declare
      Allocation : constant Unsigned_64 :=
        syscall (SYSCALL_SBRK, Arena_Buffers * Stream_Bytes);
      Success : Boolean;
   begin
      Check (Allocation /= Unsigned_64'Last, "arena allocation");
      if Allocation = Unsigned_64'Last then return; end if;
      if Result = 1 and Inspection (0) = 0 then
         --  Without approval netstack takes no memory, let alone a listener.
         Channels.Create_Arena
           (Arena, Listen_Slot, To_Address (Integer_Address (Allocation)),
            Ring_Size, Ring_Size, Arena_Buffers, Success);
         Check (not Success, "manifest does not approve itself");
         if Passed then debugPrint ("TEST: PASS network-unapproved" & ASCII.LF); end if;
         return;
      end if;
      Channels.Create_Arena
        (Arena, Connect_Slot, To_Address (Integer_Address (Allocation)),
         Ring_Size, Ring_Size, Arena_Buffers, Success);
      Check (Success, "channel arena lent");
      if not Success then return; end if;
   end;
   Bind (Inspect_Slot, 16#0A00_020F#, 8080);
   Check (Reply_Tag.label = Error_Label, "general endpoint cannot listen");
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
   Bind (Listen_Slot, 16#0A00_020F#, 8080, Buffer => 2);
   Check (Reply_Tag.label = Error_Label, "duplicate listener denied");
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
      Stream : Channels.Stream;
      Ready : Boolean;
      Async_Mode : Boolean := True;
      Token : Unsigned_64 := 16#A531_8000_0000_0000#;
      Text : String (1 .. 4);
      Got : Natural;
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
      procedure Open (Name : String; Arena_Handle : Unsigned_64 := 0;
                      Buffer : Unsigned_64 := 0) is
         Target : String (1 .. Name'Length) with Import,
           Address => Stream.Base + Storage_Offset (Channels.Layout.Target_At);
      begin
         Channels.Reset (Stream);
         Target := Name;
         Request := NULL_MESSAGE; Request.tag.label := Open_Label;
         Request.tag.length := Unsigned_8 (Name'Length);
         Request.words (0) := (if Arena_Handle = 0 then Stream.Arena else Arena_Handle);
         Request.words (1) := Buffer;
         Reply_Tag := Exchange (Connect_Slot);
      end Open;
      procedure Prepare_Shut (Handle : Unsigned_64) is
      begin
         Request := NULL_MESSAGE; Request.tag.label := Shut_Label;
         Request.tag.length := 1; Request.words (0) := Handle;
      end Prepare_Shut;
   begin
      Channels.Prepare (Stream, Arena, 0, 0, Connect_Slot);
      Open ("@net:tcp:10.0.3.2:18443");
      Check (Reply_Tag.label = Error_Label, "outbound wrong prefix denied");
      Open ("@net:tcp:10.0.2.2:18444");
      Check (Reply_Tag.label = Error_Label, "outbound wrong port denied");
      Open ("@net:tcp:example.com:18443");
      Check (Reply_Tag.label = Error_Label, "undeclared DNS denied");
      Open ("@net:tcp:10.0.2.2:18443", Arena_Handle => Arena.Handle + 1_000);
      Check (Reply_Tag.label = Error_Label, "unknown arena handle denied");
      Open ("@net:tcp:10.0.2.2:18443", Buffer => Arena_Buffers);
      Check (Reply_Tag.label = Error_Label, "buffer outside the arena denied");
      --  More lifetimes than netstack's 16 TCP connections, so slots are
      --  reused and stale handles are exercised.
      for Round in 1 .. 40 loop
         --  OPEN by call for the first half, by async submit for the rest;
         --  data always moves through the rings.
         Async_Mode := Round > 20;
         Open ("@net:tcp:10.0.2.2:18443");
         Check (Reply_Tag.label = OK_Label, "permitted outbound connects");
         if Async_Mode then
            Check (Reply_Tag.label = OK_Label, "async outbound connects");
         end if;
         if Reply_Tag.label = OK_Label then
            Channel := Request.words (0);
            Stream.Handle := Channel;
            Check (Channel > Previous_Channel, "channel handle is fresh");
            if Previous_Channel /= 0 then
               Prepare_Shut (Previous_Channel);
               Reply_Tag := capCall (Connect_Slot, Request);
               Check (Reply_Tag.label = Error_Label, "stale channel cannot close replacement");
            end if;
            Prepare_Shut (Unsigned_64'Last);
            Reply_Tag := capCall (Connect_Slot, Request);
            Check (Reply_Tag.label = Error_Label, "oversized opaque handle denied without truncation");
            Prepare_Shut (Channel);
            Reply_Tag := capCall (Listen_Slot, Request);
            Check (Reply_Tag.label = Error_Label, "wrong grant cannot close channel");
            Request := NULL_MESSAGE; Request.tag.label := Write_Label;
            Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 4;
            Reply_Tag := capCall (Connect_Slot, Request);
            Check (Reply_Tag.label = Error_Label, "retired WRITE operation refused");
            Text := "PING";
            Channels.Write (Stream, Text'Address, 4, Got);
            Check (Got = 4, "ring write");
            Channels.Await (Stream, Connect_Slot, Channels.Layout.Want_Readable,
                            syscall (SYSCALL_GETTIME) + 10_000, Next_Wait, Ready);
            Channels.Read (Stream, Text'Address, 4, Got);
            if Got in 1 .. 3 then
               Channels.Await (Stream, Connect_Slot, Channels.Layout.Want_Readable,
                               syscall (SYSCALL_GETTIME) + 10_000, Next_Wait, Ready);
               declare
                  More : Natural;
               begin
                  Channels.Read (Stream, Text (Got + 1)'Address, 4 - Got, More);
                  Got := Got + More;
               end;
            end if;
            Check (Ready and then Got = 4 and then Text = "PONG", "authorized reply received");
            if Async_Mode then
               Check (Got = 4 and then Text = "PONG", "async outbound round trip");
            end if;
            Prepare_Shut (Channel);
            Reply_Tag := Exchange (Connect_Slot);
            Check (Reply_Tag.label = OK_Label, "authorized channel closes");
            Previous_Channel := Channel;
         end if;
      end loop;
      Check (Previous_Channel > 32, "channel handles are not bounded table indices");
   end;
   --  Connected UDP to the loopback-only host peer at 10.0.2.2:18446:
   --  datagram records through the channel's rings.
   declare
      Stream : Channels.Stream;
      Success, Ready, Sent, Truncated, Found : Boolean;
      Datagram, Stale : Unsigned_64 := 0;
      Started : Unsigned_64;
      Got : Natural;
      Text : String (1 .. 64);
      Oversized : constant String (1 .. 1473) := [others => 'O'];
      procedure Open (Slot : CapabilitySlot; Name : String) is
         Target : String (1 .. Name'Length) with Import,
           Address => Stream.Base + Storage_Offset (Channels.Layout.Target_At);
      begin
         Channels.Reset (Stream);
         Target := Name;
         Request := NULL_MESSAGE; Request.tag.label := Open_Label;
         Request.tag.length := Unsigned_8 (Name'Length);
         Request.words (0) := Stream.Arena;
         Request.words (1) := Unsigned_64 (Stream.Buffer);
         Reply_Tag := capCall (Slot, Request);
      end Open;
      procedure Shut (Slot : CapabilitySlot; Handle : Unsigned_64) is
      begin
         Request := NULL_MESSAGE; Request.tag.label := Shut_Label;
         Request.tag.length := 1; Request.words (0) := Handle;
         Reply_Tag := capCall (Slot, Request);
      end Shut;
      procedure Send (Data : String) is
      begin
         Channels.Send_Datagram (Stream, Data'Address, Data'Length, Sent);
      end Send;
      procedure Receive (Max : Natural; Wait_MS : Unsigned_64 := 5_000) is
      begin
         Channels.Await (Stream, Datagram_Slot, Channels.Layout.Want_Readable,
                         syscall (SYSCALL_GETTIME) + Wait_MS, Next_Wait, Ready);
         Channels.Receive_Datagram (Stream, Text'Address, Max, Got, Truncated, Found);
      end Receive;
   begin
      Channels.Prepare (Stream, Arena, 0, 2, Datagram_Slot);
      Open (Datagram_Slot, "@net:udp:10.0.2.2:18447");
      Check (Reply_Tag.label = Error_Label, "udp wrong port denied");
      Open (Datagram_Slot, "@net:udp:10.0.2.3:18446");
      Check (Reply_Tag.label = Error_Label, "udp wrong address denied");
      Open (Datagram_Slot, "@net:udp:example.com:18446");
      Check (Reply_Tag.label = Error_Label, "udp undeclared DNS denied");
      Open (Datagram_Slot, "@net:tcp:10.0.2.2:18446");
      Check (Reply_Tag.label = Error_Label, "udp scope cannot open tcp");
      Open (Connect_Slot, "@net:udp:10.0.2.2:18443");
      Check (Reply_Tag.label = Error_Label, "tcp scope cannot open udp");
      for Round in 1 .. 2 loop
         Open (Datagram_Slot, "@net:udp:10.0.2.2:18446");
         Check (Reply_Tag.label = OK_Label, "udp channel opens");
         if Reply_Tag.label /= OK_Label then return; end if;
         Datagram := Request.words (0);
         Stream.Handle := Datagram;
         Check (Datagram /= Stale, "udp channel handle is fresh");
         if Stale /= 0 then
            Shut (Datagram_Slot, Stale);
            Check (Reply_Tag.label = Error_Label, "stale udp handle cannot close the new channel");
         end if;
         Shut (Connect_Slot, Datagram);
         Check (Reply_Tag.label = Error_Label, "wrong grant cannot close udp channel");
         Send (Oversized);   --  one byte over the UDP limit
         Check (not Sent, "oversized datagram refused");
         Send ("PING");
         Check (Sent, "udp datagram sent");
         Receive (64);
         Check (Ready and then Found and then Got = 4 and then not Truncated and then
                Text (1 .. 4) = "PONG",
                "udp reply received, foreign source filtered");
         Send ("LONG");
         Receive (8);
         Check (Ready and then Found and then Got = 8 and then Truncated and then
                Text (1 .. 8) = "LLLLLLLL",
                "udp truncation reported");
         Started := syscall (SYSCALL_GETTIME);
         Receive (64, 300);
         Check (not Found and then syscall (SYSCALL_GETTIME) - Started >= 250,
                "udp read deadline, truncated datagram consumed whole");
         Shut (Datagram_Slot, Datagram);
         Check (Reply_Tag.label = OK_Label, "udp channel closes");
         Stale := Datagram;
      end loop;
      --  The manifest declares two datagram channels: a third concurrent
      --  open is refused, and closing one returns its place.
      declare
         Extra : array (1 .. 2) of Channels.Stream;
         Held : array (1 .. 2) of Unsigned_64 := [others => 0];
         Target : constant String := "@net:udp:10.0.2.2:18446";
      begin
         for I in Extra'Range loop
            Channels.Prepare (Extra (I), Arena, I, 2 + I, Datagram_Slot);
            declare
               View : String (1 .. Target'Length) with Import,
                 Address => Extra (I).Base + Storage_Offset (Channels.Layout.Target_At);
            begin
               View := Target;
               Request := NULL_MESSAGE; Request.tag.label := Open_Label;
               Request.tag.length := Unsigned_8 (Target'Length);
               Request.words (0) := Arena.Handle;
               Request.words (1) := Unsigned_64 (I);
               Reply_Tag := capCall (Datagram_Slot, Request);
               Check (Reply_Tag.label = OK_Label, "declared udp channels open");
               Held (I) := Request.words (0);
            end;
         end loop;
         --  A buffer an open channel holds cannot be claimed again.
         Request := NULL_MESSAGE; Request.tag.label := Open_Label;
         Request.tag.length := Unsigned_8 (Target'Length);
         Request.words (0) := Arena.Handle; Request.words (1) := 1;
         Reply_Tag := capCall (Datagram_Slot, Request);
         Check (Reply_Tag.label = Error_Label, "buffer in use cannot open a second channel");
         Channels.Release_Arena (Arena, Datagram_Slot, Success);
         Check (not Success, "arena with open channels is not released");
         Open (Datagram_Slot, "@net:udp:10.0.2.2:18446");
         Check (Reply_Tag.label = Error_Label, "open beyond declared connections refused");
         Shut (Datagram_Slot, Held (1));
         Check (Reply_Tag.label = OK_Label, "declared udp channel closes");
         Open (Datagram_Slot, "@net:udp:10.0.2.2:18446");
         Check (Reply_Tag.label = OK_Label, "closed channel returns its place");
         Shut (Datagram_Slot, Request.words (0));
         Shut (Datagram_Slot, Held (2));
         Check (Reply_Tag.label = OK_Label, "declared udp channel closes");
      end;
      Open (Datagram_Slot, "@net:udp:10.0.2.2:18446");
      Stream.Handle := Request.words (0);
      Send ("DONE");
      Check (Sent, "udp peer finished");
      Shut (Datagram_Slot, Stream.Handle);
   end;
   --  Inbound: a listener in arena buffer 3; each connection arrives in
   --  buffer 0, which the program offers for it.
   declare
      Stream : Channels.Stream;
      Success, Ready, Found : Boolean;
      Received, Count : Natural;
      Payload : String (1 .. 8);
      Reply_Text : String (1 .. 8);
      Item : Channels.Arrival;
      procedure Shut (Slot : CapabilitySlot; Handle : Unsigned_64) is
      begin
         Request := NULL_MESSAGE; Request.tag.label := Shut_Label;
         Request.tag.length := 1; Request.words (0) := Handle;
         Reply_Tag := capCall (Slot, Request);
      end Shut;
   begin
      Bind (Listen_Slot, 16#0A00_020F#, 8080);
      Check (Reply_Tag.label = OK_Label, "inbound listener opens");
      Listener := Listener_Stream.Handle;
      Channels.Await (Listener_Stream, Listen_Slot, Channels.Layout.Want_Readable,
                      syscall (SYSCALL_GETTIME) + 300, Next_Wait, Ready);
      Channels.Take_Arrival (Listener_Stream, Item, Found);
      Check (not Ready and then not Found, "no arrival without incoming traffic");
      --  An offer still outstanding goes with its listener: buffer 0 is
      --  free again (round 1 below offers it anew).
      Channels.Prepare (Stream, Arena, 0, 1, Listen_Slot);
      Channels.Offer (Listener_Stream, Arena, 0, Success);
      Check (Success, "buffer offered");
      Close_Listener (Listen_Slot, Listener);
      Check (Reply_Tag.label = OK_Label, "listener closes with an offer outstanding");
      Bind (Listen_Slot, 16#0A00_020F#, 8080);
      Check (Reply_Tag.label = OK_Label, "listener reopens");
      Listener := Listener_Stream.Handle;
      debugPrint ("network-check: backlog-expiry ready" & ASCII.LF);
      Result := syscall (SYSCALL_SLEEP, 7000);
      --  Host verifies two connections nobody offered a buffer for are reset
      --  by the service's five-second timer, before this close could.
      Close_Listener (Listen_Slot, Listener);
      Check (Reply_Tag.label = OK_Label, "expired backlog listener closes");
      Bind (Listen_Slot, 16#0A00_020F#, 8080);
      Check (Reply_Tag.label = OK_Label, "listener reopens after backlog expiry");
      Listener := Listener_Stream.Handle;
      for Round in 1 .. 4 loop
         Channels.Prepare (Stream, Arena, 0, 1, Listen_Slot);
         Channels.Offer (Listener_Stream, Arena, 0, Success);
         Check (Success, "buffer offered for the next connection");
         debugPrint ("network-check: inbound ready" & Round'Image & ASCII.LF);
         loop
            Channels.Take_Arrival (Listener_Stream, Item, Found);
            exit when Found;
            Channels.Await (Listener_Stream, Listen_Slot, Channels.Layout.Want_Readable,
                            syscall (SYSCALL_GETTIME) + 10_000, Next_Wait, Ready);
            exit when not Ready;
         end loop;
         Check (Found, "inbound connection arrives");
         if not Found then return; end if;
         Check (Item.Arena = Arena.Handle and then Item.Buffer = 0,
                "arrival names the offered buffer");
         Check (CuBit.Net_Address."=" (Item.Address, CuBit.Net_Address.Mapped (16#0A00_0202#)) and then Item.Port /= 0,
                "arrival names the peer");
         Channel := Item.Channel;
         Stream.Handle := Channel;
         Check (Channel > Previous_Channel, "accepted channel identity is fresh");
         --  The accepted connection survives closure of its listener.
         Close_Listener (Listen_Slot, Listener);
         Check (Reply_Tag.label = OK_Label, "listener closes independently of accepted channel");
         Close_Listener (Listen_Slot, Listener);
         Check (Reply_Tag.label = Error_Label, "closed listener handle is stale");
         Shut (Connect_Slot, Channel);
         Check (Reply_Tag.label = Error_Label, "outbound grant cannot close accepted channel");
         Received := 0;
         while Received < Payload'Length loop
            Channels.Await (Stream, Listen_Slot, Channels.Layout.Want_Readable,
                            syscall (SYSCALL_GETTIME) + 10_000, Next_Wait, Ready);
            Channels.Read (Stream, Payload (Received + 1)'Address,
                           Payload'Length - Received, Count);
            if not Ready or else Count = 0 then
               Check (False, "inbound bounded read"); return;
            end if;
            Received := Received + Count;
         end loop;
         Check (Payload = "CuBitIPC", "fragmented inbound bytes retained");
         Channels.Await (Stream, Listen_Slot, Channels.Layout.Want_Readable,
                         syscall (SYSCALL_GETTIME) + 10_000, Next_Wait, Ready);
         Channels.Read (Stream, Payload'Address, 1, Count);
         Check (Ready and then Count = 0 and then
                Channels.Status (Stream) = Channels.Layout.Status_Peer_Finished,
                "peer half-close reports EOF");
         Reply_Text := "ACCEPTED";
         Channels.Write (Stream, Reply_Text'Address, 8, Count);
         Check (Count = 8, "accepted reply after peer half-close");
         Shut (Listen_Slot, Channel);
         Check (Reply_Tag.label = OK_Label, "accepted channel closes");
         Previous_Channel := Channel;
         if Round < 4 then
            Bind (Listen_Slot, 16#0A00_020F#, 8080);
            Check (Reply_Tag.label = OK_Label, "inbound listener reopens");
            Listener := Listener_Stream.Handle;
         end if;
      end loop;
      Channels.Release_Arena (Arena, Listen_Slot, Success);
      Check (Success, "idle arena released");
   end;
   --  The resolver, through QEMU's DNS forwarder (10.0.2.3) to the host's
   --  stub, which answers both names itself: "localhost" has an A record,
   --  and a name under .invalid is NXDOMAIN (RFC 6761), so the lookup must
   --  fail at once rather than wait out its retries.
   declare
      Localhost : constant Unsigned_64 := 16#0100_007F#;   --  127.0.0.1, byte 0 first
      First_Retry_MS : constant := 1_000;
      Started, Took : Unsigned_64;
      procedure Resolve (Name : String) is
         Bytes : String (1 .. Name'Length) with Import, Address => Request.words'Address;
      begin
         Request := NULL_MESSAGE; Request.tag.label := Resolve_Label;
         Request.tag.length := Unsigned_8 (Name'Length);
         Bytes := Name;
         Reply_Tag := capCall (Resolve_Slot, Request);
      end Resolve;
   begin
      Resolve ("localhost");
      Check (Reply_Tag.label = OK_Label and then Request.words (0) = Localhost,
             "resolver answers an A record");
      Started := syscall (SYSCALL_GETTIME);
      Resolve ("cubit-check.invalid");
      Took := syscall (SYSCALL_GETTIME) - Started;
      Check (Reply_Tag.label = Error_Label and then Took < First_Retry_MS,
             "NXDOMAIN fails at once (" & Decimal (Took) & " ms)");
      Resolve ("localhost");
      Check (Reply_Tag.label = OK_Label and then Request.words (0) = Localhost,
             "resolver still serves after a failure");
   end;
   --  ICMP errors reach their channel: a datagram to a closed port draws a
   --  port unreachable (QEMU relays the host's refusal), and the connected
   --  UDP channel reports Unreachable.
   declare
      Stream : Channels.Stream;
      Success, Ready, Sent : Boolean;
      Probe : constant String := "anyone?";
      Target : constant String := "@net:udp:10.0.2.2:18449";
      Tries : constant := 30;
      Arena2 : Channels.Arena;
   begin
      declare
         Allocation : constant Unsigned_64 := syscall (SYSCALL_SBRK, Stream_Bytes);
      begin
         Success := Allocation /= Unsigned_64'Last;
         if Success then
            Channels.Create_Arena
              (Arena2, Refused_Slot, To_Address (Integer_Address (Allocation)),
               Ring_Size, Ring_Size, 1, Success);
         end if;
      end;
      Check (Success, "refused-port arena");
      if Success then
         Channels.Prepare (Stream, Arena2, 0, 0, Refused_Slot);
         declare
            Text : String (1 .. Target'Length) with Import,
              Address => Stream.Base + Storage_Offset (Channels.Layout.Target_At);
         begin
            Channels.Reset (Stream);
            Text := Target;
         end;
         Request := NULL_MESSAGE; Request.tag.label := Open_Label;
         Request.tag.length := Unsigned_8 (Target'Length);
         Request.words (0) := Stream.Arena;
         Request.words (1) := Unsigned_64 (Stream.Buffer);
         Reply_Tag := capCall (Refused_Slot, Request);
         Check (Reply_Tag.label = OK_Label, "udp channel to a closed port opens");
         Stream.Handle := Request.words (0);
         for Attempt in 1 .. Tries loop
            exit when Channels.Status (Stream) = Channels.Layout.Status_Unreachable;
            Channels.Send_Datagram (Stream, Probe'Address, Probe'Length, Sent);
            Channels.Await (Stream, Refused_Slot, Channels.Layout.Want_Readable,
                            syscall (SYSCALL_GETTIME) + 100, Next_Wait, Ready);
         end loop;
         Check (Channels.Status (Stream) = Channels.Layout.Status_Unreachable,
                "port unreachable reaches the udp channel");
         Request := NULL_MESSAGE; Request.tag.label := Shut_Label;
         Request.tag.length := 1; Request.words (0) := Stream.Handle;
         Reply_Tag := capCall (Refused_Slot, Request);
         Channels.Release_Arena (Arena2, Refused_Slot, Success);
      end if;
   end;
   if Passed then debugPrint ("TEST: PASS network-authority" & ASCII.LF); end if;
end Main;
