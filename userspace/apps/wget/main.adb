------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace wget app: an HTTPS GET of https://example.com/, printed to
--  its stdout stream and the serial console.
--
--  All networking goes through tls.svc (CuBit.TLS_Protocol): OPEN resolves,
--  connects and verifies the certificate in one deferred call; WRITE, READ
--  and SHUT relay plaintext. wget holds no network scope and no TLS code,
--  only its tls-scope for example.com:443.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.TLS_Protocol;
with CCL_Manifest_Bindings;
with CuBit.Streams;
with CuBit.Protocols;

procedure main is
   use ASCII;

   --  Data buffer size (8 KB = 2 pages)
   DATA_BUF_PAGES : constant := 2;
   DATA_BUF_SIZE  : constant := DATA_BUF_PAGES * 4096;

   --  tls.svc operations (see CuBit.TLS_Protocol)
   OP_NET_OPEN  : Unsigned_32 renames CuBit.TLS_Protocol.Open_Operation;
   OP_NET_WRITE : Unsigned_32 renames CuBit.TLS_Protocol.Write_Operation;
   OP_NET_READ  : Unsigned_32 renames CuBit.TLS_Protocol.Read_Operation;
   OP_NET_SHUT  : Unsigned_32 renames CuBit.TLS_Protocol.Shut_Operation;
   REPLY_OK     : Unsigned_32 renames CuBit.TLS_Protocol.Reply_OK;
   REPLY_EOF    : Unsigned_32 renames CuBit.TLS_Protocol.Reply_EOF;
   CAP_SLOT_NET : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Tls;

   dataBuf     : System.Address := System.Null_Address;
   transfer    : CuBit.Memory_Grants.Grant_Reference;

   ---------------------------------------------------------------------------
   --  printDec - print a small unsigned number in decimal
   ---------------------------------------------------------------------------
   procedure printDec (val : Unsigned_32) is
      buf : String (1 .. 10);
      pos : Natural := buf'Last;
      v   : Unsigned_32 := val;
   begin
      if v = 0 then
         debugPrint ("0");
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      debugPrint (buf (pos + 1 .. buf'Last));
   end printDec;

   ---------------------------------------------------------------------------
   --  streamDec - print a small unsigned number via stdout stream
   ---------------------------------------------------------------------------
   procedure streamDec (val : Unsigned_32) is
      buf : String (1 .. 10);
      pos : Natural := buf'Last;
      v   : Unsigned_32 := val;
   begin
      if v = 0 then
         CuBit.Streams.streamPrint (CuBit.Streams.STREAM_STDOUT, "0");
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      CuBit.Streams.streamPrint (
         CuBit.Streams.STREAM_STDOUT, buf (pos + 1 .. buf'Last));
   end streamDec;

   ---------------------------------------------------------------------------
   --  Variables
   ---------------------------------------------------------------------------
   msg        : Message;
   statusLogged : Boolean := False;
   tag        : MessageTag;
   chanHandle : Unsigned_64;

   SCHEME   : constant String := "example.com:443";
   HTTP_REQ : constant String :=
      "GET / HTTP/1.1" & CR & LF &
      "Host: example.com" & CR & LF &
      "Connection: close" & CR & LF &
      CR & LF;

begin
   debugPrint ("wget: starting..." & LF);

   --  Create stdout stream (4 pages = 16KB ring buffer)
   CuBit.Streams.streamCreateTyped
     (CuBit.Streams.STREAM_STDOUT, 4, CuBit.Streams.TYPE_TEXT_LINE,
      CuBit.Protocols.TEXT_LINE_CONTRACT);

   CuBit.Streams.streamPrint (
      CuBit.Streams.STREAM_STDOUT, "wget: connecting..." & LF);

   --  2. Allocate data buffer via sbrk
   declare
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_SBRK, Unsigned_64 (DATA_BUF_SIZE));
      if ret = Unsigned_64'Last then
         debugPrint ("wget: sbrk failed" & LF);
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_EXIT, 1);
         end;
         return;
      end if;
      dataBuf := To_Address (Integer_Address (ret));
   end;

   --  Zero the buffer
   declare
      buf : array (0 .. DATA_BUF_SIZE - 1) of Unsigned_8 with
         Import, Address => dataBuf;
   begin
      for i in buf'Range loop
         buf (i) := 0;
      end loop;
   end;

   --  3. Lend our data buffer to tls.svc
   declare
      ok : Boolean;
   begin
      CuBit.Memory_Grants.Create_Via_Capability
        (CAP_SLOT_NET, dataBuf, DATA_BUF_PAGES, True, transfer, ok);

      if not ok then
         debugPrint ("wget: transfer grant to tls.svc failed" & LF);
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_EXIT, 1);
         end;
         return;
      end if;
   end;

   debugPrint ("wget: grant created" & LF);

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      rdyIgnore : MessageTag;
   begin
      rdyIgnore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, reserved => 0),
          authorityTag => 0,
          words    => (others => 0)));
   end;

   --  4. Write scheme string into grant buffer, then OP_NET_OPEN
   --  (DNS resolve + TCP connect in one deferred call)
   debugPrint ("wget: opening " & SCHEME & "..." & LF);
   CuBit.Streams.streamPrint (
      CuBit.Streams.STREAM_STDOUT, "Connecting to " & SCHEME & "..." & LF);
   declare
      buf : array (0 .. SCHEME'Length - 1) of Unsigned_8 with
         Import, Address => dataBuf;
   begin
      for i in 0 .. SCHEME'Length - 1 loop
         buf (i) := Unsigned_8 (
            Character'Pos (SCHEME (SCHEME'First + i)));
      end loop;
   end;

   msg := NULL_MESSAGE;
   msg.tag := (label  => OP_NET_OPEN,
               length => Unsigned_8 (SCHEME'Length),
               flags  => 0,
               reserved  => 0);
   msg.words (0) := transfer.slot;
   msg.words (1) := Unsigned_64 (DATA_BUF_SIZE);
   msg.words (3) := transfer.generation;
   tag := capCall (CAP_SLOT_NET, msg);

   if tag.label /= REPLY_OK then
      declare
         use CuBit.TLS_Protocol;
         Reason : constant String :=
           (if tag.label = Reply_Error and then msg.words (0) in
               1 .. Unsigned_64 (Failure'Enum_Rep (Failure'Last))
            then CuBit.TLS_Protocol.Name (Failure'Enum_Val (msg.words (0)))
            else "TLS service unavailable");
      begin
         debugPrint ("wget: open failed: " & Reason & LF);
         CuBit.Streams.streamPrint (
            CuBit.Streams.STREAM_STDOUT, "Connection failed: " & Reason & LF);
      end;
      declare
         ignore : Unsigned_64;
      begin
         ignore := syscall (SYSCALL_EXIT, 1);
      end;
      return;
   end if;

   chanHandle := msg.words (0);
   debugPrint ("wget: channel open, handle=");
   printDec (Unsigned_32 (chanHandle));
   debugPrint (", sending HTTP GET" & LF);
   CuBit.Streams.streamPrint (
      CuBit.Streams.STREAM_STDOUT, "Connected, sending request..." & LF);

   --  5. Write HTTP request into grant buffer, OP_NET_WRITE
   declare
      buf : array (0 .. HTTP_REQ'Length - 1) of Unsigned_8 with
         Import, Address => dataBuf;
   begin
      for i in 0 .. HTTP_REQ'Length - 1 loop
         buf (i) := Unsigned_8 (
            Character'Pos (HTTP_REQ (HTTP_REQ'First + i)));
      end loop;
   end;

   msg := NULL_MESSAGE;
   msg.tag := (label  => OP_NET_WRITE,
               length => 3,
               flags  => 0,
               reserved  => 0);
   msg.words (0) := chanHandle;
   msg.words (1) := 0;   -- offset
   msg.words (2) := Unsigned_64 (HTTP_REQ'Length);
   tag := capCall (CAP_SLOT_NET, msg);

   if tag.label /= REPLY_OK then
      debugPrint ("wget: write failed" & LF);
      declare
         ignore : Unsigned_64;
      begin
         ignore := syscall (SYSCALL_EXIT, 1);
      end;
      return;
   end if;

   debugPrint ("wget: sent ");
   printDec (Unsigned_32 (HTTP_REQ'Length));
   debugPrint (" bytes" & LF);

   --  6. OP_NET_READ loop until EOF
   loop
      --  Handle any pending stream IPC (e.g. OP_STREAM_LIST) before blocking
      declare
         ignore : Boolean;
      begin
         ignore := CuBit.Streams.streamHandleSubscription;
      end;

      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_NET_READ,
                  length => 3,
                  flags  => 0,
                  reserved  => 0);
      msg.words (0) := chanHandle;
      msg.words (1) := 0;      -- offset in grant buffer
      msg.words (2) := Unsigned_64 (DATA_BUF_SIZE);
      tag := capCall (CAP_SLOT_NET, msg);

      if tag.label = REPLY_EOF then
         debugPrint ("wget: EOF, closing" & LF);
         CuBit.Streams.streamPrint (
            CuBit.Streams.STREAM_STDOUT, "[EOF]" & LF);
         exit;
      end if;

      if tag.label /= REPLY_OK then
         debugPrint ("wget: read error" & LF);
         CuBit.Streams.streamPrint (
            CuBit.Streams.STREAM_STDOUT, "[read error]" & LF);
         exit;
      end if;

      declare
         recvLen : constant Natural := Natural (msg.words (0));
      begin
         debugPrint ("wget: received ");
         printDec (Unsigned_32 (recvLen));
         debugPrint (" bytes" & LF);

         CuBit.Streams.streamPrint (
            CuBit.Streams.STREAM_STDOUT, "Received ");
         streamDec (Unsigned_32 (recvLen));
         CuBit.Streams.streamPrint (
            CuBit.Streams.STREAM_STDOUT, " bytes" & LF);

         --  Log the HTTP status line once, for the serial console.
         if not statusLogged and then recvLen > 0 then
            declare
               text : String (1 .. recvLen) with Import, Address => dataBuf;
               stop : Natural := 0;
            begin
               for i in text'Range loop
                  exit when text (i) = CR or else text (i) = LF or else i > 80;
                  stop := i;
               end loop;
               debugPrint ("wget: status " & text (1 .. stop) & LF);
               statusLogged := True;
            end;
         end if;

         --  Write received data to stdout stream
         if recvLen > 0 then
            declare
               ignore : Unsigned_32;
            begin
               ignore := CuBit.Streams.streamWrite (
                  CuBit.Streams.STREAM_STDOUT,
                  dataBuf,
                  Unsigned_32 (recvLen),
                  CuBit.Streams.TYPE_TEXT_LINE);
            end;
         end if;
      end;
   end loop;

   --  7. OP_NET_SHUT
   msg := NULL_MESSAGE;
   msg.tag := (label  => OP_NET_SHUT,
               length => 1,
               flags  => 0,
               reserved  => 0);
   msg.words (0) := chanHandle;
   declare
      ignore : MessageTag;
   begin
      ignore := capCall (CAP_SLOT_NET, msg);
   end;

   --  Drain any pending stream IPC before exit
   declare
      ignore : Boolean;
   begin
      ignore := CuBit.Streams.streamHandleSubscription;
   end;

   debugPrint ("wget: done" & LF);
   CuBit.Streams.streamPrint (
      CuBit.Streams.STREAM_STDOUT, "Done." & LF);
   declare
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_EXIT, 0);
   end;
end main;
