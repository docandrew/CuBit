pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CCL_Manifest_Bindings;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Config;
with CuBit.Filesystems;
with CuBit.TLS_Protocol; use CuBit.TLS_Protocol;
with CuBit.TLS_Scopes;
with SPARKNaCl; use SPARKNaCl;
with SPARKTLS; use SPARKTLS;
with SPARKTLS.Client;
with SPARKTLS.Cert_Verify;
with SPARKTLS.RBG;
with X509;
with TLS_Clock;
with Client_Scopes;

--  tls.svc: TLS client service. Applications ask for a TLS stream to a host
--  name; the service checks the caller's (tls-scope ...) authority, connects
--  through netstack, runs a SPARKTLS client session with WebPKI validation
--  against the system trust store, and relays plaintext. Session keys never
--  leave this process. Single-threaded event loop: client requests, async
--  netstack completions and deadlines.
procedure Main is
   use ASCII;
   use type System.Address;

   Net_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Tcp;
   FS_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Filesystem;

   NET_OPEN : constant Unsigned_32 := 16#0420#;
   NET_WRITE : constant Unsigned_32 := 16#0421#;
   NET_READ : constant Unsigned_32 := 16#0422#;
   NET_SHUT : constant Unsigned_32 := 16#0423#;

   Handshake_Timeout_MS : constant := 20_000;
   Net_Read_Timeout_MS : constant := 30_000;

   --  netstack supports 16 TCP connections system-wide; TLS gets up to half.
   --  Deferred reply slots are 48 .. 48 + 2 * Maximum_Channels - 1.
   Maximum_Channels : constant := 8;
   subtype Channel_Index is Natural range 0 .. Maximum_Channels - 1;

   --  Per-channel netstack transfer: ciphertext out, then ciphertext in.
   Half : constant := 20_480;
   Net_Buffer_Bytes : constant := 2 * Half;
   Net_Buffer_Pages : constant := Net_Buffer_Bytes / 4_096;

   --  Closed: the connection ended (peer close, error or timeout) but the
   --  client still holds the channel ID. Buffered plaintext can still be
   --  read, then EOF or the error; SHUT frees the channel.
   type Phase_Kind is
     (Free, Connecting, Handshaking, Established, Failed, Closed);
   type Net_Operation is (None, Op_Open, Op_Write, Op_Read, Op_Shut);
   for Net_Operation use
     (None => 0, Op_Open => 1, Op_Write => 2, Op_Read => 3, Op_Shut => 4);

   type Channel is record
      Phase : Phase_Kind := Free;
      Id : Unsigned_64 := 0;
      Owner : ProcessID := NO_PROCESS;
      --  The client's transfer buffer, acquired for the channel's lifetime.
      Client : CuBit.Memory_Grants.Grant_Reference;
      Client_Address : System.Address := System.Null_Address;
      Client_Bytes : Natural := 0;
      Name : String (1 .. CuBit.TLS_Scopes.Maximum_Name);
      Name_Length : CuBit.TLS_Scopes.Name_Length := 0;
      --  Our buffer lent to netstack.
      Net_Grant : CuBit.Memory_Grants.Grant_Reference;
      Net_Address : System.Address := System.Null_Address;
      Net_Channel : Unsigned_64 := 0;
      Net_Pending : Net_Operation := None;
      Deadline : Unsigned_64 := Unsigned_64'Last;
      --  Deferred client replies, saved into per-channel reply slots.
      Open_Waiting : Boolean := False;
      Read_Waiting : Boolean := False;
      Read_Offset, Read_Max : Natural := 0;
      Read_Deadline : Unsigned_64 := Unsigned_64'Last;
      --  Received ciphertext in the input half of the netstack buffer that
      --  the session has not accepted yet. A read can carry more than the
      --  session's input buffer takes at once; the rest is fed as records
      --  are processed, and no new read is issued until it is all fed.
      In_Position, In_Length : Natural := 0;
      --  Decrypted data not yet delivered to the client.
      Plain_Length, Plain_Position : Natural := 0;
      Peer_Closed : Boolean := False;
      Error : Failure := Service_Failure;
   end record;

   Channels : array (Channel_Index) of Channel;
   Sessions : array (Channel_Index) of Client_Session;
   Plain : array (Channel_Index) of Byte_Seq (0 .. 16_383);
   Next_Id : Unsigned_64 := 1;
   Scopes : Client_Scopes.Table;
   Roots : aliased Trust_Store;

   --  Static name overrides from the tls.hosts setting ("name=a.b.c.d ...").
   Maximum_Hosts : constant := 8;
   type Host_Entry is record
      Name : String (1 .. CuBit.TLS_Scopes.Maximum_Name);
      Name_Length : Natural := 0;
      Address : String (1 .. 15);
      Address_Length : Natural := 0;
   end record;
   Hosts : array (1 .. Maximum_Hosts) of Host_Entry;
   Host_Count : Natural := 0;

   --  A synchronous call to netstack replaces the reply capability of the
   --  request being handled, so handlers that call out first save it here.
   Held_Reply_Slot : constant CapabilitySlot := 47;

   function Open_Slot (I : Channel_Index) return CapabilitySlot is
     (CapabilitySlot (48 + 2 * I));
   function Read_Slot (I : Channel_Index) return CapabilitySlot is
     (CapabilitySlot (49 + 2 * I));

   function Token (I : Channel_Index; Op : Net_Operation) return Unsigned_64 is
     (Shift_Left (Channels (I).Id, 8) or Shift_Left (Unsigned_64 (I), 4) or
      Unsigned_64 (Net_Operation'Enum_Rep (Op)));

   procedure Send_Reply
     (Slot : CapabilitySlot; Label : Unsigned_32; Length : Unsigned_8;
      W0 : Unsigned_64 := 0; W1 : Unsigned_64 := 0)
   is
      Ignore : Unsigned_64;
   begin
      Ignore := replyCap
        (Slot,
         (tag => (label => Label, length => Length, flags => 0, reserved => 0),
          authorityTag => 0,
          words => [W0, W1, 0, 0]));
   end Send_Reply;

   procedure Fail_Request (Code : Failure; Slot : CapabilitySlot := CapabilitySlot'Last) is
   begin
      Send_Reply (Slot, Reply_Error, 1, Unsigned_64 (Failure'Enum_Rep (Code)));
   end Fail_Request;

   function Map_Error (Code : Error_Code) return Failure is
     (case Code is
         when Certificate_Expired => Certificate_Expired,
         when Bad_Certificate | Certificate_Unknown | Certificate_Verify_Failed =>
            Certificate_Untrusted,
         when Certificate_Revoked | Bad_Certificate_Status_Response |
              Certificate_Required => Certificate_Rejected,
         when Entropy_Failure | Internal_Error | Insufficient_Buffer |
              Bad_Configuration | No_Free_Sessions => Service_Failure,
         when others => Protocol_Alert);

   ---------------------------------------------------------------------------
   --  Channel teardown, in two steps. Retire ends the connection: it
   --  completes a waiting open or read, drops the session and closes the
   --  netstack channel. A channel whose ID the client already holds stays,
   --  Closed, until the client's SHUT calls Free, so a peer close never
   --  makes a client's channel ID disappear under it.
   ---------------------------------------------------------------------------
   procedure Free (I : Channel_Index) is
      C : Channel renames Channels (I);
      Returned : Boolean;
   begin
      if C.Client_Address /= System.Null_Address then
         CuBit.Memory_Grants.Return_Acquisition (C.Client, Returned);
      end if;
      Plain (I) := [others => 0];
      --  Keep the netstack buffer and its grant for reuse by this slot.
      C := (Net_Grant => C.Net_Grant, Net_Address => C.Net_Address,
            others => <>);
   end Free;

   procedure Retire (I : Channel_Index; Code : Failure) is
      C : Channel renames Channels (I);
      Ignore_Tag : MessageTag;
      Msg : Message := NULL_MESSAGE;
      Client_Holds_Id : constant Boolean := not C.Open_Waiting;
   begin
      if C.Open_Waiting then
         C.Open_Waiting := False;
         Fail_Request (Code, Open_Slot (I));
      end if;
      if C.Read_Waiting and then C.Plain_Position >= C.Plain_Length then
         C.Read_Waiting := False;
         if Code = Peer_Closed then
            Send_Reply (Read_Slot (I), Reply_EOF, 0);
         else
            Fail_Request (Code, Read_Slot (I));
         end if;
      end if;
      Drop (Sessions (I));
      if C.Net_Channel /= 0 then
         --  Synchronous shut: netstack replies immediately and releases its
         --  own acquisition of our buffer.
         Msg.tag := (label => NET_SHUT, length => 1, flags => 0, reserved => 0);
         Msg.words (0) := C.Net_Channel;
         Ignore_Tag := capCall (Net_Slot, Msg);
         C.Net_Channel := 0;
      end if;
      C.Error := Code;
      C.Deadline := Unsigned_64'Last;
      if Client_Holds_Id then
         C.Phase := Closed;
      else
         Free (I);
      end if;
   end Retire;

   ---------------------------------------------------------------------------
   --  Submitting netstack operations (async; completions carry Token).
   ---------------------------------------------------------------------------
   procedure Submit
     (I : Channel_Index; Op : Net_Operation; Label : Unsigned_32;
      Length : Unsigned_8; W0, W1, W2, W3 : Unsigned_64)
   is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag := (label => Label, length => Length, flags => 0, reserved => 0);
      Msg.words := [W0, W1, W2, W3];
      if capSubmit (Net_Slot, Msg, Token (I, Op)) then
         Channels (I).Net_Pending := Op;
      else
         Channels (I).Phase := Failed;
         Channels (I).Error := Busy;
      end if;
   end Submit;

   --  Serve a waiting client read from buffered plaintext.
   procedure Serve_Read (I : Channel_Index) is
      C : Channel renames Channels (I);
      Count : Natural;
   begin
      if not C.Read_Waiting or else C.Plain_Position >= C.Plain_Length then
         return;
      end if;
      Count := Natural'Min (C.Read_Max, C.Plain_Length - C.Plain_Position);
      declare
         Target : Byte_Seq (0 .. N32 (Count) - 1) with Import,
           Address => C.Client_Address + Storage_Offset (C.Read_Offset);
      begin
         Target := Plain (I) (N32 (C.Plain_Position) ..
                              N32 (C.Plain_Position + Count) - 1);
      end;
      C.Plain_Position := C.Plain_Position + Count;
      C.Read_Waiting := False;
      Send_Reply (Read_Slot (I), Reply_OK, 1, Unsigned_64 (Count));
   end Serve_Read;

   ---------------------------------------------------------------------------
   --  Drive: run the SPARKTLS state machine until it needs the network or
   --  the client. At most one netstack operation is outstanding per channel.
   ---------------------------------------------------------------------------
   --  Feed pending received ciphertext to the session; True if any was
   --  accepted.
   function Feed_Pending (I : Channel_Index) return Boolean is
      C : Channel renames Channels (I);
      In_Area : Byte_Seq (0 .. Half - 1) with Import,
        Address => C.Net_Address + Storage_Offset (Half);
      Fed : N32 := 0;
      Accepted : Boolean := False;
   begin
      while C.In_Position < C.In_Length loop
         declare
            Chunk : constant Byte_Seq (0 .. N32 (C.In_Length - C.In_Position) - 1) :=
              In_Area (N32 (C.In_Position) .. N32 (C.In_Length) - 1);
         begin
            Feed_Ciphertext (Sessions (I), Chunk, Fed);
         end;
         exit when Fed = 0;
         C.In_Position := C.In_Position + Natural (Fed);
         Accepted := True;
      end loop;
      return Accepted;
   end Feed_Pending;

   procedure Drive (I : Channel_Index) is
      C : Channel renames Channels (I);
      Result : Action;
      Count : N32;
   begin
      while C.Phase in Handshaking | Established and then C.Net_Pending = None loop
         --  Decrypted data must be read before the session advances again,
         --  or the next record overwrites it. While our buffer still holds
         --  undelivered plaintext, wait for the client instead.
         if Has_Plaintext (Sessions (I)) then
            exit when C.Plain_Position < C.Plain_Length;
            Read_Plaintext (Sessions (I), Plain (I), Count);
            C.Plain_Length := Natural (Count);
            C.Plain_Position := 0;
            Serve_Read (I);
         end if;
         SPARKTLS.Client.Advance (Sessions (I), Result);
         case Result is
            when OK =>
               null;
            when Has_Output =>
               declare
                  Out_Area : Byte_Seq (0 .. Half - 1)
                    with Import, Address => C.Net_Address;
               begin
                  Drain_Ciphertext (Sessions (I), Out_Area, Count);
               end;
               if Count > 0 then
                  Submit (I, Op_Write, NET_WRITE, 3, C.Net_Channel, 0,
                          Unsigned_64 (Count), 0);
               end if;
            when Need_Input =>
               if C.In_Position < C.In_Length then
                  --  Ciphertext from the last read is still waiting. The
                  --  session asked for input, so it has processed what it
                  --  holds; if it still takes nothing, a record exceeds its
                  --  buffer. Otherwise keep advancing with the new input.
                  if not Feed_Pending (I) then
                     debugPrint ("tls: channel" & C.Id'Image &
                                 " failed: record exceeds input buffer" & LF);
                     C.Error := Protocol_Alert;
                     C.Phase := Failed;
                     exit;
                  end if;
               else
                  --  Read from the network only when someone needs the
                  --  data: the handshake, or a client read with nothing
                  --  buffered.
                  if C.Phase = Handshaking or else
                    (C.Read_Waiting and then C.Plain_Position >= C.Plain_Length)
                  then
                     Submit (I, Op_Read, NET_READ, 4, C.Net_Channel, Half, Half,
                             syscall (SYSCALL_GETTIME) + Net_Read_Timeout_MS);
                  end if;
                  exit;
               end if;
            when Handshake_Done =>
               C.Phase := Established;
               C.Deadline := Unsigned_64'Last;
               if C.Open_Waiting then
                  C.Open_Waiting := False;
                  Send_Reply (Open_Slot (I), Reply_OK, 1, C.Id);
               end if;
               debugPrint ("tls: channel" & C.Id'Image & " established with " &
                           C.Name (1 .. C.Name_Length) & LF);
            when Plaintext_Ready =>
               exit when C.Plain_Position < C.Plain_Length;
               Read_Plaintext (Sessions (I), Plain (I), Count);
               C.Plain_Length := Natural (Count);
                  C.Plain_Position := 0;
               Serve_Read (I);
            when Error_Alert =>
               C.Error := Map_Error (Last_Error (Sessions (I)));
               debugPrint ("tls: channel" & C.Id'Image & " failed: " &
                           Name (C.Error) & LF);
               C.Phase := Failed;
            when Shutdown =>
               C.Peer_Closed := True;
               C.Error := Peer_Closed;
               C.Phase := Failed;
         end case;
      end loop;
      if C.Phase = Failed and then C.Net_Pending = None then
         --  Remaining plaintext stays readable after Retire.
         Serve_Read (I);
         Retire (I, C.Error);
      end if;
   end Drive;

   ---------------------------------------------------------------------------
   --  Netstack completion for channel I.
   ---------------------------------------------------------------------------
   procedure Complete (I : Channel_Index; Op : Net_Operation; Reply : Message) is
      C : Channel renames Channels (I);
      OK_Reply : constant Boolean := Reply.tag.label = Reply_OK;
   begin
      C.Net_Pending := None;
      case Op is
         when Op_Open =>
            if not OK_Reply then
               C.Error := Connect_Failed;
               C.Phase := Failed;
            else
               C.Net_Channel := Reply.words (0);
               C.Phase := Handshaking;
            end if;
         when Op_Write =>
            if not OK_Reply then
               C.Error := Connect_Failed;
               C.Phase := Failed;
            end if;
         when Op_Read =>
            if Reply.tag.label = Reply_EOF then
               C.Error := Peer_Closed;
               C.Peer_Closed := True;
               C.Phase := Failed;
            elsif not OK_Reply or else Reply.words (0) = 0 or else
              Reply.words (0) > Half
            then
               C.Error := (if C.Phase = Handshaking then Timeout else Connect_Failed);
               C.Phase := Failed;
            else
               --  Whatever the session cannot take yet stays pending; Drive
               --  feeds it as records are processed.
               C.In_Position := 0;
               C.In_Length := Natural (Reply.words (0));
               declare
                  Ignore : constant Boolean := Feed_Pending (I);
               begin
                  null;
               end;
            end if;
         when Op_Shut | None =>
            null;
      end case;
      Drive (I);
   end Complete;

   ---------------------------------------------------------------------------
   --  Client operations.
   ---------------------------------------------------------------------------
   function Find (Owner : ProcessID; Id : Unsigned_64) return Integer is
   begin
      for I in Channel_Index loop
         if Channels (I).Phase /= Free and then Channels (I).Id = Id and then
           Channels (I).Owner = Owner
         then
            return I;
         end if;
      end loop;
      return -1;
   end Find;

   function Lookup_Host (Name : String; Address : out String; Length : out Natural)
     return Boolean is
   begin
      Address := [others => ' '];
      Length := 0;
      for H of Hosts (1 .. Host_Count) loop
         if H.Name_Length = Name'Length and then
           H.Name (1 .. H.Name_Length) = Name
         then
            Address (Address'First .. Address'First + H.Address_Length - 1) :=
              H.Address (1 .. H.Address_Length);
            Length := H.Address_Length;
            return True;
         end if;
      end loop;
      return False;
   end Lookup_Host;

   procedure Handle_Open (From : ProcessID; Request : Message) is
      Length : constant Natural := Natural (Request.tag.length);
      Reference : constant CuBit.Memory_Grants.Grant_Reference :=
        (slot => Request.words (0), generation => Request.words (3));
      Size : constant Unsigned_64 := Request.words (1);
      Address : System.Address;
      Acquired, Returned : Boolean;
      Free_Index : Integer := -1;
      Colon : Natural := 0;
      Port_Value : Natural := 0;
      Name : String (1 .. CuBit.TLS_Scopes.Maximum_Name);
      Name_Length : Natural := 0;
      Port : Unsigned_16;
      Target : String (1 .. 15);
      Target_Length : Natural;
   begin
      if Length = 0 or else Size not in 1 .. 1_048_576 or else
        Unsigned_64 (Length) > Size or else Request.words (2) /= 0
      then
         Fail_Request (Malformed_Request);
         return;
      end if;
      CuBit.Memory_Grants.Acquire
        (Reference, From, 0, Size, CuBit.Memory_Grants.Write_Access,
         Address, Acquired);
      if not Acquired then
         Fail_Request (Malformed_Request);
         return;
      end if;
      declare
         Text : String (1 .. Length) with Import, Address => Address;
      begin
         for I in reverse Text'Range loop
            if Text (I) = ':' then Colon := I; exit; end if;
         end loop;
         if Colon in 2 .. Length - 1 and then Colon - 1 <= Name'Length and then
           Length - Colon <= 5
         then
            for C of Text (Colon + 1 .. Length) loop
               if C not in '0' .. '9' then Port_Value := 0; exit; end if;
               Port_Value := Port_Value * 10 + Character'Pos (C) - Character'Pos ('0');
            end loop;
            Name_Length := Colon - 1;
            for I in 1 .. Name_Length loop
               Name (I) := CuBit.TLS_Scopes.Lower (Text (I));
            end loop;
         end if;
      end;
      if Name_Length = 0 or else Port_Value not in 1 .. 65_535 or else
        not CuBit.TLS_Scopes.Valid_Name (Name (1 .. Name_Length))
      then
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         Fail_Request (Malformed_Request);
         return;
      end if;
      Port := Unsigned_16 (Port_Value);
      if not Client_Scopes.Allows
        (Scopes, Unsigned_64 (From), Name (1 .. Name_Length), Port)
      then
         debugPrint ("tls: scope denied " & Name (1 .. Name_Length) & LF);
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         Fail_Request (Scope_Denied);
         return;
      end if;
      --  Certificate validity depends on trustworthy time.
      if TLS_Clock.Now.Year = 0 then
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         Fail_Request (Clock_Untrusted);
         return;
      end if;
      for I in Channel_Index loop
         if Channels (I).Phase = Free and then
           Channels (I).Net_Address /= System.Null_Address
         then
            Free_Index := I;
            exit;
         end if;
      end loop;
      if Free_Index < 0 or else saveReplyCap (Unsigned_64 (Open_Slot (Free_Index))) /= 1 then
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         Fail_Request (Quota_Exceeded);
         return;
      end if;
      declare
         I : constant Channel_Index := Free_Index;
         C : Channel renames Channels (I);
         Scheme_Prefix : constant String := "@net:tcp:";
         Port_Image : constant String := Port'Image;
         Via_Address : constant Boolean := Lookup_Host
           (Name (1 .. Name_Length), Target, Target_Length);
         Host_Part : constant String :=
           (if Via_Address then Target (1 .. Target_Length)
            else Name (1 .. Name_Length));
         Scheme : constant String := Scheme_Prefix & Host_Part & ":" &
           Port_Image (Port_Image'First + 1 .. Port_Image'Last);
         Scheme_Text : String (1 .. Scheme'Length)
           with Import, Address => C.Net_Address;
      begin
         C.Phase := Connecting;
         C.Id := Next_Id;
         Next_Id := Next_Id + 1;
         C.Owner := From;
         C.Client := Reference;
         C.Client_Address := Address;
         C.Client_Bytes := Natural (Size);
         C.Name (1 .. Name_Length) := Name (1 .. Name_Length);
         C.Name_Length := Name_Length;
         C.Open_Waiting := True;
         C.Deadline := syscall (SYSCALL_GETTIME) + Handshake_Timeout_MS;
         Sessions (I) := SPARKTLS.Client.Configure
           ((Server_Name => To_Name (Name (1 .. Name_Length)),
             Trust => Roots'Unchecked_Access,
             Get_Time => TLS_Clock.Now'Access,
             Verify_Mode => Mode_WebPKI,
             others => <>));
         Scheme_Text := Scheme;
         Submit (I, Op_Open, NET_OPEN, Unsigned_8 (Scheme'Length),
                 C.Net_Grant.slot, Net_Buffer_Bytes, 0, C.Net_Grant.generation);
         if C.Phase = Failed then
            Retire (I, Busy);
         end if;
      end;
   end Handle_Open;

   procedure Handle_Write (From : ProcessID; Request : Message) is
      Index : constant Integer := Find (From, Request.words (0));
      Offset : constant Unsigned_64 := Request.words (1);
      Length : constant Unsigned_64 := Request.words (2);
      Written : N32;
   begin
      if Index < 0 then
         Fail_Request (Unknown_Channel);
         return;
      end if;
      declare
         C : Channel renames Channels (Index);
      begin
         if C.Phase /= Established then
            Fail_Request (if C.Phase = Closed then C.Error else Busy);
         elsif Offset > Unsigned_64 (C.Client_Bytes) or else
           Length > Unsigned_64 (C.Client_Bytes) - Offset or else Length = 0
         then
            Fail_Request (Malformed_Request);
         elsif C.Net_Pending /= None then
            --  One ciphertext write in flight per channel; clients retry.
            Fail_Request (Busy);
         else
            declare
               Data : constant Byte_Seq
                 (0 .. N32 (Unsigned_64'Min (Length, 16_384)) - 1)
                 with Import,
                 Address => C.Client_Address + Storage_Offset (Offset);
            begin
               Write_Plaintext (Sessions (Index), Data, Written);
            end;
            Send_Reply (CapabilitySlot'Last, Reply_OK, 1, Unsigned_64 (Written));
            Drive (Index);
         end if;
      end;
   end Handle_Write;

   procedure Handle_Read (From : ProcessID; Request : Message) is
      Index : constant Integer := Find (From, Request.words (0));
      Offset : constant Unsigned_64 := Request.words (1);
      Max : constant Unsigned_64 := Request.words (2);
   begin
      if Index < 0 then
         Fail_Request (Unknown_Channel);
         return;
      end if;
      declare
         C : Channel renames Channels (Index);
      begin
         if C.Read_Waiting then
            Fail_Request (Busy);
         elsif Offset > Unsigned_64 (C.Client_Bytes) or else
           Max > Unsigned_64 (C.Client_Bytes) - Offset or else Max = 0 or else
           Request.tag.length not in 3 .. 4
         then
            Fail_Request (Malformed_Request);
         elsif C.Phase = Closed and then C.Plain_Position >= C.Plain_Length then
            if C.Error = Peer_Closed then
               Send_Reply (CapabilitySlot'Last, Reply_EOF, 0);
            else
               Fail_Request (C.Error);
            end if;
         elsif C.Phase /= Established and then C.Phase /= Closed then
            Fail_Request (Busy);
         elsif saveReplyCap (Unsigned_64 (Read_Slot (Index))) /= 1 then
            Fail_Request (Service_Failure);
         else
            C.Read_Waiting := True;
            C.Read_Offset := Natural (Offset);
            C.Read_Max := Natural (Max);
            C.Read_Deadline :=
              (if Request.tag.length = 4 then Request.words (3) else Unsigned_64'Last);
            Serve_Read (Index);
            if C.Read_Waiting then
               Drive (Index);
            end if;
         end if;
      end;
   end Handle_Read;

   procedure Handle_Shut (From : ProcessID; Request : Message) is
      Index : constant Integer := Find (From, Request.words (0));
      Count : N32;
      Msg : Message := NULL_MESSAGE;
      Ignore : MessageTag;
   begin
      if Index < 0 then
         Fail_Request (Unknown_Channel);
         return;
      end if;
      if saveReplyCap (Unsigned_64 (Held_Reply_Slot)) /= 1 then
         return;
      end if;
      declare
         C : Channel renames Channels (Index);
      begin
         --  Best-effort close_notify, sent synchronously before releasing.
         if C.Phase = Established and then C.Net_Pending = None and then
           not Write_Limit_Reached (Sessions (Index))
         then
            SPARKTLS.Client.Close_Notify (Sessions (Index));
            declare
               Out_Area : Byte_Seq (0 .. Half - 1)
                 with Import, Address => C.Net_Address;
            begin
               Drain_Ciphertext (Sessions (Index), Out_Area, Count);
            end;
            if Count > 0 then
               Msg.tag := (label => NET_WRITE, length => 3, flags => 0, reserved => 0);
               Msg.words := [C.Net_Channel, 0, Unsigned_64 (Count), 0];
               Ignore := capCall (Net_Slot, Msg);
            end if;
         end if;
         --  A pending netstack operation completes later against a released
         --  channel; its token's stale Id makes it ignored.
         if C.Phase /= Closed then
            Retire (Index, Peer_Closed);
         end if;
         Free (Index);
      end;
      Send_Reply (Held_Reply_Slot, Reply_OK, 0);
   end Handle_Shut;

   procedure Handle_Info (From : ProcessID; Request : Message) is
      Index : constant Integer := Find (From, Request.words (0));
   begin
      if Index < 0 or else Channels (Index).Phase /= Established then
         Fail_Request (Unknown_Channel);
         return;
      end if;
      Send_Reply
        (CapabilitySlot'Last, Reply_OK, 2,
         Unsigned_64 (TLS_Version'Pos (Get_Version (Sessions (Index)))),
         Unsigned_64 (Negotiated_Suite (Sessions (Index))));
   end Handle_Info;

   ---------------------------------------------------------------------------
   --  Policy operations from procmgr.
   ---------------------------------------------------------------------------
   procedure Handle_Set_Scopes (From : ProcessID; Request : Message) is
      Client : constant Unsigned_64 := Request.words (0);
      Count : constant Unsigned_64 := Request.words (1);
      Reference : constant CuBit.Memory_Grants.Grant_Reference :=
        (slot => Request.words (2), generation => Request.words (3));
      Address : System.Address;
      Acquired, Returned, Installed : Boolean;
      List : Client_Scopes.Scope_List;
   begin
      if Client = 0 or else Count not in 1 .. Maximum_Scopes_Per_Client then
         Fail_Request (Malformed_Request);
         return;
      end if;
      CuBit.Memory_Grants.Acquire
        (Reference, From, 0, Count * Scope_Entry_Bytes,
         CuBit.Memory_Grants.Read_Access, Address, Acquired);
      if not Acquired then
         Fail_Request (Malformed_Request);
         return;
      end if;
      declare
         Raw : array (0 .. Natural (Count) * Scope_Entry_Bytes - 1) of Unsigned_8
           with Import, Address => Address;
         Valid : Boolean := True;
      begin
         for E in 0 .. Natural (Count) - 1 loop
            declare
               Base : constant Natural := E * Scope_Entry_Bytes;
               Length : constant Natural := Natural (Raw (Base + 1));
               Text : String (1 .. 64);
            begin
               if Raw (Base) /= 1 or else Length not in 1 .. 64 then
                  Valid := False;
               else
                  for C in 1 .. Length loop
                     Text (C) := Character'Val (Raw (Base + 7 + C));
                  end loop;
                  CuBit.TLS_Scopes.Parse (Text (1 .. Length), List (E + 1), Valid);
               end if;
            end;
            exit when not Valid;
         end loop;
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         if not Valid then
            Fail_Request (Malformed_Request);
            return;
         end if;
      end;
      Client_Scopes.Install
        (Scopes, Client, List, Client_Scopes.Scope_Count (Count), Installed);
      if Installed then
         Send_Reply (CapabilitySlot'Last, Reply_OK, 0);
      else
         Fail_Request (Quota_Exceeded);
      end if;
   end Handle_Set_Scopes;

   procedure Handle_Revoke (Request : Message) is
      Client : constant Unsigned_64 := Request.words (0);
   begin
      if Client = 0 then
         Fail_Request (Malformed_Request);
         return;
      end if;
      if saveReplyCap (Unsigned_64 (Held_Reply_Slot)) /= 1 then
         return;
      end if;
      Client_Scopes.Revoke (Scopes, Client);
      for I in Channel_Index loop
         if Channels (I).Phase /= Free and then
           Unsigned_64 (Channels (I).Owner) = Client
         then
            Retire (I, Scope_Denied);
            if Channels (I).Phase = Closed then
               Free (I);
            end if;
         end if;
      end loop;
      Send_Reply (Held_Reply_Slot, Reply_OK, 0);
   end Handle_Revoke;

   ---------------------------------------------------------------------------
   --  Start-up: trust store, static hosts, buffers.
   ---------------------------------------------------------------------------
   Roots_Path : constant String := "@nvme:0/tls/roots.der";
   Roots_Capacity : constant := 262_144;

   function Load_Roots return Boolean is
      use CuBit.Filesystems;
      Allocation : constant Unsigned_64 := syscall (SYSCALL_SBRK, Roots_Capacity);
      Buffer_Address : constant System.Address :=
        To_Address (Integer_Address (Allocation));
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Granted, OK : Boolean;
      Msg : Message;
      Handle : File_Handle;
      Total : Natural := 0;
      Loaded : Natural;
   begin
      if Allocation = Unsigned_64'Last then
         return False;
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (FS_Slot, Buffer_Address, Roots_Capacity / 4_096, True, Reference, Granted);
      if not Granted then
         return False;
      end if;
      declare
         Path : String (1 .. Roots_Path'Length) with Import, Address => Buffer_Address;
      begin
         Path := Roots_Path;
      end;
      Msg := Open_Request (Reference, Roots_Path'Length, OPEN_READ_ONLY);
      Msg.tag := capCall (FS_Slot, Msg);
      if Msg.tag.label /= CuBit.Filesystems.REPLY_OK then
         debugPrint ("tls: cannot open " & Roots_Path & LF);
         return False;
      end if;
      Handle := File_Handle (Msg.words (0));
      Msg := Read_At_Request (Handle, Reference, Roots_Capacity, 0);
      Msg.tag := capCall (FS_Slot, Msg);
      if Msg.tag.label = CuBit.Filesystems.REPLY_OK and then Msg.words (0) <= Roots_Capacity then
         Total := Natural (Msg.words (0));
      end if;
      Msg := Close_Request (Handle);
      Msg.tag := capCall (FS_Slot, Msg);
      if Total = 0 or else Total = Roots_Capacity then
         debugPrint ("tls: root bundle empty or larger than" &
                     Roots_Capacity'Image & " bytes" & LF);
         return False;
      end if;
      declare
         DER : X509.Byte_Seq (0 .. X509.N32 (Total) - 1)
           with Import, Address => Buffer_Address;
      begin
         SPARKTLS.Cert_Verify.Load_Roots (Roots, DER, Loaded, OK);
      end;
      debugPrint ("tls: loaded" & Loaded'Image & " trust anchors" & LF);
      return OK and then Loaded > 0;
   end Load_Roots;

   procedure Load_Hosts is
      Address : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      use type CuBit.Config.ConfigStatus;
   begin
      CuBit.Config.get ("tls.hosts", Address, Length, Status);
      if Status /= CuBit.Config.OK or else Length = 0 or else Length > 1_024 then
         return;
      end if;
      declare
         Text : String (1 .. Length) with Import, Address => Address;
         Position : Natural := 1;
      begin
         while Position <= Length and then Host_Count < Maximum_Hosts loop
            while Position <= Length and then Text (Position) = ' ' loop
               Position := Position + 1;
            end loop;
            exit when Position > Length;
            declare
               Start : constant Natural := Position;
               Equals, Stop : Natural := 0;
            begin
               while Position <= Length and then Text (Position) /= ' ' loop
                  if Text (Position) = '=' and then Equals = 0 then
                     Equals := Position;
                  end if;
                  Position := Position + 1;
               end loop;
               Stop := Position - 1;
               if Equals > Start and then Equals < Stop and then
                 Equals - Start <= CuBit.TLS_Scopes.Maximum_Name and then
                 Stop - Equals <= 15 and then
                 CuBit.TLS_Scopes.Valid_Name (Text (Start .. Equals - 1)) and then
                 (for all C of Text (Equals + 1 .. Stop) => C in '0' .. '9' | '.')
               then
                  Host_Count := Host_Count + 1;
                  declare
                     H : Host_Entry renames Hosts (Host_Count);
                  begin
                     H.Name_Length := Equals - Start;
                     for I in 1 .. H.Name_Length loop
                        H.Name (I) := CuBit.TLS_Scopes.Lower (Text (Start + I - 1));
                     end loop;
                     H.Address_Length := Stop - Equals;
                     H.Address (1 .. H.Address_Length) := Text (Equals + 1 .. Stop);
                  end;
               else
                  debugPrint ("tls: ignoring malformed tls.hosts entry" & LF);
               end if;
            end;
         end loop;
      end;
   end Load_Hosts;

   function Setup_Buffers return Boolean is
      Allocation : constant Unsigned_64 :=
        syscall (SYSCALL_SBRK, Unsigned_64 (Maximum_Channels * Net_Buffer_Bytes));
      Granted : Boolean;
   begin
      if Allocation = Unsigned_64'Last then
         return False;
      end if;
      for I in Channel_Index loop
         Channels (I).Net_Address := To_Address
           (Integer_Address (Allocation) + Integer_Address (I * Net_Buffer_Bytes));
         CuBit.Memory_Grants.Create_Via_Capability
           (Net_Slot, Channels (I).Net_Address, Net_Buffer_Pages, True,
            Channels (I).Net_Grant, Granted);
         if not Granted then
            return False;
         end if;
      end loop;
      return True;
   end Setup_Buffers;

   function Next_Deadline return Unsigned_64 is
      Deadline : Unsigned_64 := Unsigned_64'Last;
   begin
      for C of Channels loop
         if C.Phase /= Free then
            Deadline := Unsigned_64'Min (Deadline, C.Deadline);
            if C.Read_Waiting then
               Deadline := Unsigned_64'Min (Deadline, C.Read_Deadline);
            end if;
         end if;
      end loop;
      return Deadline;
   end Next_Deadline;

   procedure Expire (Now : Unsigned_64) is
   begin
      for I in Channel_Index loop
         declare
            C : Channel renames Channels (I);
         begin
            if C.Phase in Connecting | Handshaking and then Now >= C.Deadline then
               Retire (I, Timeout);
            elsif C.Read_Waiting and then Now >= C.Read_Deadline then
               C.Read_Waiting := False;
               Fail_Request (Timeout, Read_Slot (I));
            end if;
         end;
      end loop;
   end Expire;

   Entropy_OK : Boolean;
   From : ProcessID;
   Request : Message;
   Found, Progress : Boolean;
   Completion : CompletionEntry;
   Ignore : Unsigned_64;
begin
   debugPrint ("tls: starting" & LF);
   SPARKTLS.RBG.Init (Entropy_OK);
   if not Entropy_OK then
      debugPrint ("tls: entropy or DRBG self-test failed; exiting" & LF);
      return;
   end if;
   if not Load_Roots then
      debugPrint ("tls: no trust anchors; exiting" & LF);
      return;
   end if;
   Load_Hosts;
   if not Setup_Buffers then
      debugPrint ("tls: netstack buffers unavailable; exiting" & LF);
      return;
   end if;
   Ignore := registerDriver (Service_Role);
   if Ignore = Unsigned_64'Last then
      debugPrint ("tls: registration failed" & LF);
      return;
   end if;
   debugPrint ("tls: ready" & LF);

   loop
      Progress := False;
      while Poll_Completion (Completion'Address) = 1 loop
         declare
            Index : constant Unsigned_64 := Shift_Right (Completion.token, 4) and 15;
            Op : constant Unsigned_64 := Completion.token and 15;
            Id : constant Unsigned_64 := Shift_Right (Completion.token, 8);
         begin
            if Index < Maximum_Channels and then Op in 1 .. 4 and then
              Channels (Natural (Index)).Id = Id and then
              Channels (Natural (Index)).Phase /= Free
            then
               Complete (Natural (Index), Net_Operation'Enum_Val (Op),
                         Completion.msg);
            end if;
         end;
         Progress := True;
      end loop;

      Poll_Service_Request (From, Request, Found);
      if Found then
         Progress := True;
         if Request.authorityTag = Policy_Tag then
            case Request.tag.label is
               when Set_Scopes_Operation => Handle_Set_Scopes (From, Request);
               when Revoke_Operation => Handle_Revoke (Request);
               when others => Fail_Request (Malformed_Request);
            end case;
         else
            case Request.tag.label is
               when Open_Operation => Handle_Open (From, Request);
               when Write_Operation => Handle_Write (From, Request);
               when Read_Operation => Handle_Read (From, Request);
               when Shut_Operation => Handle_Shut (From, Request);
               when Info_Operation => Handle_Info (From, Request);
               when others => Fail_Request (Malformed_Request);
            end case;
         end if;
      end if;

      Expire (syscall (SYSCALL_GETTIME));
      if not Progress then
         if Wait_For_Activity_Until (Next_Deadline) = Unavailable then
            debugPrint ("tls: activity wait unavailable" & LF);
            return;
         end if;
      end if;
   end loop;
end Main;
