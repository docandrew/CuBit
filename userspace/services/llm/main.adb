------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  llm.svc: the Messages API client (AGENT-001 slice 1, AGENT-002 step 1).
--
--  @description
--  It holds the only network authority on the agent path: its manifest's
--  one tls-scope, the endpoint its startup entry names. The API key is a
--  named secret in secrets.svc (SEC-022 v0): llm.svc holds the use grant,
--  takes the value for each exchange into memory, and wipes it when the
--  exchange ends; it never logs or returns it. Clients ("llm" service
--  endpoint) only ask; they never see the key. Each ask is one
--  exchange: tls.svc connects and verifies the server, llm.svc renders the
--  client's provider-neutral request (typed tools and a conversation of
--  typed blocks, CuBit.Llm_Protocol) as a Messages API request
--  (Llm_Requests, with prompt-cache breakpoints when asked), decodes the
--  streamed response a byte at a time (Llm_Responses) and hands the text
--  to the client as it arrives, through the client's own window grant; a
--  captured exchange also returns the response's content blocks (text,
--  thinking, tool calls) with its summary. Every exchange is logged (Llm_Reports):
--  who asked, the declared mission, model, usage, cost, stop and duration;
--  never the key, the prompt or the answer.
--
--  Single-threaded and never blocking on the network: tls.svc's OPEN and
--  READ are submitted (capSubmit) and complete into the event loop; WRITE
--  and SHUT are answered at once. A client's Pull is set aside until there
--  is text or the end. The IPC graph stays a DAG: llm.svc calls tls.svc
--  and answers clients, and never calls a client.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Log;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Canonical_Readers;
with CuBit.TLS_Protocol;
with CuBit.Llm_Protocol;
with CuBit.Secrets_Protocol;
with Llm_Costs;
with Llm_Requests;
with Llm_Responses;
with Llm_Reports;
with Parameters;
with Startup;

procedure Main is
   package P renames CuBit.Llm_Protocol;
   package T renames CuBit.TLS_Protocol;
   package R renames Llm_Responses;
   use type CuBit.Handles.Reply;
   use type P.Ceiling_Kind;
   use type P.Stop_Reason;

   TLS_Service : constant CuBit.Handles.Endpoint := Startup.Tls;
   Secrets_Service : constant CuBit.Handles.Endpoint := Startup.Secrets;
   package S_P renames CuBit.Secrets_Protocol;

   subtype Byte is Unsigned_8;
   Page_Bytes : constant := 4_096;

   ---------------------------------------------------------------------------
   --  The endpoint (its startup entry's typed parameter) and the key.
   ---------------------------------------------------------------------------
   Endpoint_Text : constant String :=
     CuBit.Canonical_Readers.Image (Parameters.Value.endpoint);
   --  The secret holding the API key, by name (its value never in argv,
   --  settings or this program's parameters).
   Key_Secret : constant String :=
     CuBit.Canonical_Readers.Image (Parameters.Value.api_key_secret);
   HTTPS_Port : constant String := "443";

   --  "host:port" splits at its last colon; the Host header omits :443.
   function Last_Colon return Natural is
   begin
      for I in reverse Endpoint_Text'Range loop
         if Endpoint_Text (I) = ':' then
            return I;
         end if;
      end loop;
      return 0;
   end Last_Colon;
   Colon : constant Natural := Last_Colon;
   Host_Header : constant String :=
     (if Colon > Endpoint_Text'First
        and then Endpoint_Text (Colon + 1 .. Endpoint_Text'Last) = HTTPS_Port
      then Endpoint_Text (Endpoint_Text'First .. Colon - 1) else Endpoint_Text);

   --  The page secrets.svc writes the key's value into (Use), wiped as
   --  soon as the value is copied into the exchange that needs it.
   type Key_Page_Array is array (0 .. Page_Bytes - 1) of Byte with Alignment => Page_Bytes;
   Key_Page : Key_Page_Array := [others => 0];
   Key_Grant : CuBit.Memory_Grants.Grant_Reference;
   Key_Page_Lent : Boolean := False;

   ---------------------------------------------------------------------------
   --  Exchanges. Each slot owns one page-aligned transfer buffer lent to
   --  tls.svc for its channel: the "host:port" for OPEN at offset 0, a
   --  staging area at Request_Offset that each WRITE's piece of the
   --  request is copied into, and READ's area from Read_Offset. The
   --  rendered request itself (header and body) is the exchange's own,
   --  on the heap (CuAlloc), as long as it needs to be up to
   --  Maximum_Request_Bytes (LIM-001). Its text queue holds decoded text
   --  until the client pulls it; a full queue holds back the next READ
   --  (back pressure, never loss).
   ---------------------------------------------------------------------------
   Maximum_Exchanges : constant := 4;
   subtype Slot_Index is Natural range 0 .. Maximum_Exchanges - 1;

   Scheme_Area_Bytes : constant := Page_Bytes;
   --  The largest ask (CuBit.Llm_Protocol), and the ceiling on the request
   --  it renders to (header and body): JSON escaping can make a body up to
   --  six times its texts. One beyond it is refused Request_Too_Large, a
   --  typed capacity error, never cut.
   Maximum_Ask_Bytes : constant := P.Maximum_Ask_Bytes;
   Maximum_Request_Bytes : constant := 6 * Maximum_Ask_Bytes + Page_Bytes;
   --  One WRITE's piece of the request.
   Maximum_Write : constant := 16_384;
   Staging_Area_Bytes : constant := Maximum_Write;
   Read_Area_Bytes : constant := 16_384;
   Request_Offset : constant := Scheme_Area_Bytes;
   Read_Offset : constant := Request_Offset + Staging_Area_Bytes;
   Net_Buffer_Bytes : constant := Read_Offset + Read_Area_Bytes;
   Net_Buffer_Pages : constant := Net_Buffer_Bytes / Page_Bytes;
   pragma Compile_Time_Error (Net_Buffer_Bytes mod Page_Bytes /= 0, "net buffers are whole pages");
   Queue_Bytes : constant := 16_384;

   type Net_Buffer is array (0 .. Net_Buffer_Bytes - 1) of Byte
     with Alignment => Page_Bytes;
   Net_Buffers : array (Slot_Index) of Net_Buffer := [others => [others => 0]];
   Net_Grants : array (Slot_Index) of CuBit.Memory_Grants.Grant_Reference;
   Net_Lent : array (Slot_Index) of Boolean := [others => False];

   type Queue_Array is array (0 .. Queue_Bytes - 1) of Byte;

   type Phase_Kind is (Free, Opening, Writing, Reading, Ended);

   --  The heap (CuAlloc): fallible, so a request that cannot be held is
   --  refused instead of stopping the service.
   function Heap_Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure Heap_Free (Item : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";
   Heap_Alignment : constant := 16;

   --  A captured exchange's decoder holds its content area, sized from the
   --  request's max_tokens (Llm_Responses.Capacity_For).
   type Decoder_Access is access R.Decoder;

   type Exchange is record
      Phase : Phase_Kind := Free;
      Id : Unsigned_64 := 0;
      Owner : Process_ID := No_Process;
      --  The client's window: its ask, then pulled text and the summary.
      Window : CuBit.Memory_Grants.Grant_Reference;
      Window_Address : System.Address := System.Null_Address;
      Window_Bytes : Natural := 0;
      Channel : Unsigned_64 := 0;
      Channel_Open : Boolean := False;
      Decoder : Decoder_Access := null;
      --  The rendered request (header with the key, then the body).
      Request : Unsigned_64 := 0;
      --  Captured content already sent to the client (Reply_Content).
      Content_Sent : Natural := 0;
      Started_Ms, Deadline_Ms : Unsigned_64 := 0;
      Request_Length, Written : Natural := 0;
      Read_Pending, Open_Pending : Boolean := False;
      Pull_Waiting : Boolean := False;
      Pull_Max : Natural := 0;
      Queue : Queue_Array := [others => 0];
      Head, Count : Natural := 0;
      Model : String (1 .. P.Maximum_Model_Bytes) := [others => ' '];
      Model_Length : Natural range 0 .. P.Maximum_Model_Bytes := 0;
      Mission : String (1 .. P.Maximum_Mission_Bytes) := [others => ' '];
      Mission_Length : Natural range 0 .. P.Maximum_Mission_Bytes := 0;
      Ended_Ms : Unsigned_64 := 0;
      --  The key's value, for this exchange's header and to redact what
      --  the server sends back; wiped when the exchange ends.
      Key : String (1 .. P.Maximum_Key_Bytes) := [others => ' '];
      Key_Length : Natural range 0 .. P.Maximum_Key_Bytes := 0;
      Detail : String (1 .. P.Maximum_Detail_Bytes) := [others => ' '];
      Detail_Length : Natural range 0 .. P.Maximum_Detail_Bytes := 0;
   end record;

   Exchanges : array (Slot_Index) of Exchange;
   Pull_Replies : array (Slot_Index) of CuBit.Handles.Reply := [others => CuBit.Handles.No_Reply];
   Next_Id : Unsigned_64 := 1;
   Client_Now : Process_ID := No_Process;

   function Now return Unsigned_64 is (syscall (SYSCALL_GETTIME));

   ---------------------------------------------------------------------------
   --  Replies.
   ---------------------------------------------------------------------------
   procedure Send_Current
     (Label : Unsigned_32; Length : Unsigned_8; W0 : Unsigned_64 := 0; W1 : Unsigned_64 := 0)
   is
      Ignore : Unsigned_64;
   begin
      Ignore := reply
        (Client_Now,
         (tag => (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0, words => [W0, W1, 0, 0]));
   end Send_Current;

   procedure Refuse (Why : P.Refusal) is
   begin
      Send_Current (P.Reply_Error, 1, Unsigned_64 (P.Refusal'Enum_Rep (Why)));
   end Refuse;

   procedure Send_Saved
     (Saved : in out CuBit.Handles.Reply; Label : Unsigned_32; Length : Unsigned_8;
      W0 : Unsigned_64 := 0)
   is
      Ignore : Unsigned_64;
   begin
      Ignore := replyCap
        (Saved,
         (tag => (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0, words => [W0, 0, 0, 0]));
      Saved := CuBit.Handles.No_Reply;
   end Send_Saved;

   ---------------------------------------------------------------------------
   --  Little-endian fields in a mapped buffer.
   ---------------------------------------------------------------------------
   procedure Put_Bytes (Base : System.Address; Offset : Natural; Value : Unsigned_64; Width : Positive) is
      Field : array (0 .. Width - 1) of Byte with Import, Address => Base + Storage_Offset (Offset);
      V : Unsigned_64 := Value;
   begin
      for I in Field'Range loop
         Field (I) := Byte (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;
   end Put_Bytes;

   function Get_Bytes (Base : System.Address; Offset : Natural; Width : Positive) return Unsigned_64 is
      Field : array (0 .. Width - 1) of Byte with Import, Address => Base + Storage_Offset (Offset);
      V : Unsigned_64 := 0;
   begin
      for I in reverse Field'Range loop
         V := Shift_Left (V, 8) or Unsigned_64 (Field (I));
      end loop;
      return V;
   end Get_Bytes;

   procedure Put_Text (Base : System.Address; Offset : Natural; Text : String) is
      Field : String (1 .. Text'Length) with Import, Address => Base + Storage_Offset (Offset);
   begin
      Field := Text;
   end Put_Text;

   ---------------------------------------------------------------------------
   --  tls.svc.
   ---------------------------------------------------------------------------
   type Net_Operation is (Op_Open, Op_Read);
   for Net_Operation use (Op_Open => 1, Op_Read => 2);

   --  A completion's token names the slot, the exchange and the operation,
   --  so a completion for an exchange that already ended is recognised.
   function Token (Slot : Slot_Index; Op : Net_Operation) return Unsigned_64 is
     (Shift_Left (Exchanges (Slot).Id, 8) or Shift_Left (Unsigned_64 (Slot), 4) or
      Unsigned_64 (Net_Operation'Enum_Rep (Op)));

   function Message_Of (Label : Unsigned_32; Length : Unsigned_8;
                        W0, W1, W2, W3 : Unsigned_64 := 0) return Message is
     ((tag => (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0),
       authorityTag => 0, words => [W0, W1, W2, W3]));

   procedure Shut_Channel (Slot : Slot_Index) is
      E : Exchange renames Exchanges (Slot);
      Msg : Message;
      Ignore : MessageTag;
   begin
      if E.Channel_Open then
         Msg := Message_Of (T.Shut_Operation, 1, E.Channel);
         Ignore := capCall (TLS_Service, Msg, Deadline_After (1_000));
         E.Channel_Open := False;
      end if;
   end Shut_Channel;

   procedure Submit_Read (Slot : Slot_Index) is
      E : Exchange renames Exchanges (Slot);
      Room : constant Natural := Natural'Min (Read_Area_Bytes, Queue_Bytes - E.Count);
   begin
      if E.Phase /= Reading or else E.Read_Pending or else not E.Channel_Open or else Room = 0
        or else R.Finished (E.Decoder.all)
      then
         return;
      end if;
      E.Read_Pending := capSubmit
        (TLS_Service,
         Message_Of (T.Read_Operation, 4, E.Channel, Read_Offset, Unsigned_64 (Room), E.Deadline_Ms),
         Token (Slot, Op_Read));
      if not E.Read_Pending then
         R.Stop_With (E.Decoder.all, P.Disconnected);
      end if;
   end Submit_Read;

   ---------------------------------------------------------------------------
   --  Pulls and the end of an exchange.
   ---------------------------------------------------------------------------
   procedure Write_Summary (Slot : Slot_Index) is
      E : Exchange renames Exchanges (Slot);
      D : R.Decoder renames E.Decoder.all;
      U : constant Llm_Costs.Usage := R.Usage_Of (D);
      Model : constant String := R.Model_Of (D);
      Shown_Model : constant String :=
        (if Model'Length > 0 then Model else E.Model (1 .. E.Model_Length));
      Shown_Detail : constant String := E.Detail (1 .. E.Detail_Length);
      Base : constant System.Address := E.Window_Address;
   begin
      Put_Bytes (Base, P.Summary_Stop_Offset, Unsigned_64 (P.Stop_Reason'Enum_Rep (R.Stop_Of (D))), 1);
      Put_Bytes (Base, P.Summary_Error_Offset, Unsigned_64 (P.Error_Kind'Enum_Rep (R.Error_Of (D))), 1);
      Put_Bytes (Base, P.Summary_Problem_Offset, Unsigned_64 (P.Problem'Enum_Rep (R.Problem_Of (D))), 1);
      Put_Bytes (Base, P.Summary_Priced_Offset, (if R.Cost_Known (D) then 1 else 0), 1);
      Put_Bytes (Base, P.Summary_Status_Offset, Unsigned_64 (R.Status_Of (D)), 4);
      Put_Bytes (Base, P.Summary_Input_Offset, U.Input, 8);
      Put_Bytes (Base, P.Summary_Output_Offset, U.Output, 8);
      Put_Bytes (Base, P.Summary_Cache_Read_Offset, U.Cache_Read, 8);
      Put_Bytes (Base, P.Summary_Cache_Write_Offset, U.Cache_Write, 8);
      Put_Bytes (Base, P.Summary_Cost_Offset, R.Cost_Of (D), 8);
      Put_Bytes (Base, P.Summary_Duration_Offset, E.Ended_Ms - E.Started_Ms, 8);
      Put_Bytes (Base, P.Summary_Text_Bytes_Offset, R.Text_Bytes_Of (D), 8);
      Put_Bytes (Base, P.Summary_Tool_Calls_Offset, Unsigned_64 (R.Tool_Calls_Of (D)), 4);
      --  A captured exchange's content went before, in Reply_Content pieces.
      Put_Bytes (Base, P.Summary_Content_Length_Offset, Unsigned_64 (E.Content_Sent), 4);
      Put_Bytes (Base, P.Summary_Model_Length_Offset, Unsigned_64 (Shown_Model'Length), 4);
      Put_Bytes (Base, P.Summary_Detail_Length_Offset, Unsigned_64 (Shown_Detail'Length), 4);
      Put_Text (Base, P.Summary_Model_Offset, Shown_Model);
      Put_Text (Base, P.Summary_Detail_Offset, Shown_Detail);
   end Write_Summary;

   procedure Free_Decoder is new Ada.Unchecked_Deallocation (R.Decoder, Decoder_Access);

   --  The rendered request (and its key header) never outlives its writing.
   procedure Wipe_Request (E : in out Exchange) is
   begin
      if E.Request /= 0 then
         declare
            Text : String (1 .. E.Request_Length)
              with Import, Address => To_Address (Integer_Address (E.Request));
         begin
            Text := [others => ' '];
         end;
         Heap_Free (E.Request);
         E.Request := 0;
      end if;
   end Wipe_Request;

   procedure Release (Slot : Slot_Index) is
      E : Exchange renames Exchanges (Slot);
      Returned : Boolean;
   begin
      if E.Window_Address /= System.Null_Address then
         CuBit.Memory_Grants.Return_Acquisition (E.Window, Returned);
      end if;
      Wipe_Request (E);
      if E.Decoder /= null then
         Free_Decoder (E.Decoder);
      end if;
      Net_Buffers (Slot) := [others => 0];
      E := (others => <>);
   end Release;

   --  Answer a waiting Pull: text when there is some, else the end.
   procedure Serve_Pull (Slot : Slot_Index) is
      E : Exchange renames Exchanges (Slot);
   begin
      if not E.Pull_Waiting then
         return;
      end if;
      if E.Count > 0 then
         declare
            N : constant Natural := Natural'Min (E.Pull_Max, E.Count);
            Target : array (0 .. N - 1) of Byte with Import, Address => E.Window_Address;
         begin
            for I in Target'Range loop
               Target (I) := E.Queue ((E.Head + I) mod Queue_Bytes);
            end loop;
            E.Head := (E.Head + N) mod Queue_Bytes;
            E.Count := E.Count - N;
            E.Pull_Waiting := False;
            Send_Saved (Pull_Replies (Slot), P.Reply_Text, 1, Unsigned_64 (N));
         end;
         Submit_Read (Slot);
      elsif E.Phase = Ended and then E.Content_Sent < R.Content_Length (E.Decoder.all) then
         --  The captured content, piece by piece, before the summary.
         declare
            D : R.Decoder renames E.Decoder.all;
            N : constant Natural := Natural'Min (E.Pull_Max, R.Content_Length (D) - E.Content_Sent);
            Target : array (1 .. N) of Byte with Import, Address => E.Window_Address;
         begin
            for I in Target'Range loop
               Target (I) := R.Content_Byte (D, E.Content_Sent + I);
            end loop;
            E.Content_Sent := E.Content_Sent + N;
            E.Pull_Waiting := False;
            Send_Saved (Pull_Replies (Slot), P.Reply_Content, 1, Unsigned_64 (N));
         end;
      elsif E.Phase = Ended then
         Write_Summary (Slot);
         E.Pull_Waiting := False;
         Send_Saved (Pull_Replies (Slot), P.Reply_Done, 1, 0);
         Release (Slot);
      end if;
   end Serve_Pull;

   procedure Log_End (Slot : Slot_Index) is
      E : Exchange renames Exchanges (Slot);
      D : R.Decoder renames E.Decoder.all;
      Model : constant String := R.Model_Of (D);
   begin
      CuBit.Log.Info
        (Llm_Reports.Line
           ((Exchange => E.Id, Source => To_Word (E.Owner), Stop => R.Stop_Of (D),
             Error => R.Error_Of (D), Fault => R.Problem_Of (D), Status => R.Status_Of (D),
             Usage => R.Usage_Of (D), Priced => R.Cost_Known (D), Cost => R.Cost_Of (D),
             Duration_Ms => E.Ended_Ms - E.Started_Ms, Text_Bytes => R.Text_Bytes_Of (D),
             Tool_Calls => R.Tool_Calls_Of (D)),
            E.Mission (1 .. E.Mission_Length),
            (if Model'Length > 0 then Model else E.Model (1 .. E.Model_Length)),
            E.Key (1 .. E.Key_Length)));
   end Log_End;

   --  The decoder finished: close the channel, log, and tell a waiting
   --  client once its text is pulled.
   procedure End_Exchange (Slot : Slot_Index) is
      E : Exchange renames Exchanges (Slot);
   begin
      if E.Phase = Ended or else E.Phase = Free then
         return;
      end if;
      Shut_Channel (Slot);
      --  The key's header leaves memory with the request.
      Net_Buffers (Slot) (Request_Offset .. Read_Offset - 1) := [others => 0];
      Wipe_Request (E);
      E.Phase := Ended;
      E.Ended_Ms := Now;
      Log_End (Slot);
      declare
         Detail : constant String :=
           Llm_Reports.Redact (R.Detail_Of (E.Decoder.all), E.Key (1 .. E.Key_Length));
         Shown : constant Natural := Natural'Min (Detail'Length, P.Maximum_Detail_Bytes);
      begin
         E.Detail (1 .. Shown) := Detail (Detail'First .. Detail'First + Shown - 1);
         E.Detail_Length := Shown;
      end;
      E.Key := [others => ' '];
      E.Key_Length := 0;
      Serve_Pull (Slot);
   end End_Exchange;

   procedure Stop_Exchange (Slot : Slot_Index; Reason : P.Stop_Reason)
   with Pre => Reason in P.Deadline | P.Cancelled | P.Connect_Failed | P.Disconnected
   is
   begin
      R.Stop_With (Exchanges (Slot).Decoder.all, Reason);
      --  An OPEN still pending ends when it completes.
      if not Exchanges (Slot).Open_Pending then
         End_Exchange (Slot);
      end if;
   end Stop_Exchange;

   ---------------------------------------------------------------------------
   --  Writing the request and reading the response.
   ---------------------------------------------------------------------------
   procedure Write_Request (Slot : Slot_Index) is
      E : Exchange renames Exchanges (Slot);
      Msg : Message;
      Tag : MessageTag;
      Chunk : Natural;
   begin
      while E.Written < E.Request_Length and then E.Request /= 0 loop
         Chunk := Natural'Min (Maximum_Write, E.Request_Length - E.Written);
         --  The next piece into the staging area tls.svc reads it from.
         declare
            Rendered : String (1 .. E.Request_Length)
              with Import, Address => To_Address (Integer_Address (E.Request));
            Staging : String (1 .. Chunk)
              with Import, Address => Net_Buffers (Slot) (Request_Offset)'Address;
         begin
            Staging := Rendered (E.Written + 1 .. E.Written + Chunk);
         end;
         Msg := Message_Of (T.Write_Operation, 3, E.Channel,
                            Unsigned_64 (Request_Offset), Unsigned_64 (Chunk));
         Tag := capCall (TLS_Service, Msg, Deadline_After (1_000));
         if Tag.label /= T.Reply_OK then
            if Tag.label = T.Reply_Error and then Msg.words (0) = Unsigned_64 (T.Failure'Enum_Rep (T.Busy)) then
               return;   --  tls.svc's ring is full: try again on the next turn.
            end if;
            Stop_Exchange (Slot, P.Disconnected);
            return;
         end if;
         exit when Msg.words (0) = 0;
         E.Written := E.Written + Natural (Unsigned_64'Min (Msg.words (0), Unsigned_64 (Chunk)));
      end loop;
      if E.Written >= E.Request_Length then
         Net_Buffers (Slot) (Request_Offset .. Read_Offset - 1) := [others => 0];
         Wipe_Request (E);
         E.Phase := Reading;
         Submit_Read (Slot);
      end if;
   end Write_Request;

   procedure Feed (Slot : Slot_Index; Length : Natural) is
      E : Exchange renames Exchanges (Slot);
      Out_Text : R.Output;
   begin
      for I in 0 .. Length - 1 loop
         R.Push (E.Decoder.all, Net_Buffers (Slot) (Read_Offset + I), Out_Text);
         for J in 1 .. Out_Text.Count loop
            if E.Count < Queue_Bytes then
               E.Queue ((E.Head + E.Count) mod Queue_Bytes) := Out_Text.Bytes (J);
               E.Count := E.Count + 1;
            end if;
         end loop;
         exit when R.Finished (E.Decoder.all);
      end loop;
   end Feed;

   procedure Complete (Slot : Slot_Index; Op : Net_Operation; Reply_Msg : Message) is
      E : Exchange renames Exchanges (Slot);
   begin
      case Op is
         when Op_Open =>
            E.Open_Pending := False;
            if Reply_Msg.tag.label = T.Reply_OK then
               E.Channel := Reply_Msg.words (0);
               E.Channel_Open := True;
            end if;
            if R.Finished (E.Decoder.all) then
               End_Exchange (Slot);   --  stopped while connecting
            elsif not E.Channel_Open then
               declare
                  Code : constant Unsigned_64 := Reply_Msg.words (0);
               begin
                  if Reply_Msg.tag.label = T.Reply_Error and then
                    Code in 1 .. Unsigned_64 (T.Failure'Enum_Rep (T.Failure'Last))
                  then
                     CuBit.Log.Warning ("llm: connect failed: " & T.Name (T.Failure'Enum_Val (Code)));
                  end if;
               end;
               Stop_Exchange (Slot, P.Connect_Failed);
            else
               E.Phase := Writing;
               Write_Request (Slot);
            end if;
         when Op_Read =>
            E.Read_Pending := False;
            if Reply_Msg.tag.label = T.Reply_OK then
               Feed (Slot, Natural (Unsigned_64'Min (Reply_Msg.words (0), Read_Area_Bytes)));
               Serve_Pull (Slot);
            elsif Reply_Msg.tag.label = T.Reply_EOF then
               R.End_Of_Input (E.Decoder.all);
            elsif Reply_Msg.words (0) = Unsigned_64 (T.Failure'Enum_Rep (T.Timeout)) then
               R.Stop_With (E.Decoder.all, P.Deadline);
            else
               R.Stop_With (E.Decoder.all, P.Disconnected);
            end if;
            if R.Finished (E.Decoder.all) then
               End_Exchange (Slot);
            else
               Submit_Read (Slot);
            end if;
      end case;
   end Complete;

   ---------------------------------------------------------------------------
   --  Client operations.
   ---------------------------------------------------------------------------
   function Find (Owner : Process_ID; Id : Unsigned_64) return Integer is
   begin
      for S in Slot_Index loop
         if Exchanges (S).Phase /= Free and then Exchanges (S).Id = Id
           and then Exchanges (S).Owner = Owner
         then
            return S;
         end if;
      end loop;
      return -1;
   end Find;

   --  The key's value from secrets.svc, through llm.svc's use grant.
   procedure Fetch_Key
     (Into : out String; Length : out Natural; Why : out P.Refusal; Fetched : out Boolean)
   is
      Name : String (1 .. Key_Secret'Length) with Import, Address => Key_Page'Address;
      Msg : Message;
      Tag : MessageTag;
   begin
      Into := [others => ' '];
      Length := 0;
      Fetched := False;
      Why := P.Service_Failure;
      if not Key_Page_Lent then
         return;
      end if;
      Name := Key_Secret;
      Msg := Message_Of (S_P.Use_Operation, 3, Key_Grant.slot, Key_Grant.generation,
                         Unsigned_64 (Key_Secret'Length));
      Tag := capCall (Secrets_Service, Msg, Deadline_After (1_000));
      if Tag.label = S_P.Reply_OK and then Msg.words (0) in 1 .. P.Maximum_Key_Bytes then
         declare
            Value : String (1 .. Natural (Msg.words (0))) with Import, Address => Key_Page'Address;
         begin
            if Llm_Requests.Valid_Key (Value) and then Value'Length <= Into'Length then
               Into (Into'First .. Into'First + Value'Length - 1) := Value;
               Length := Value'Length;
               Fetched := True;
            else
               Why := P.Key_Rejected;
            end if;
         end;
      elsif Tag.label = S_P.Reply_Error and then Msg.words (0) = Unsigned_64 (S_P.Refusal'Enum_Rep (S_P.Not_Stored)) then
         Why := P.No_Key;
      elsif Tag.label = S_P.Reply_Error and then Msg.words (0) = Unsigned_64 (S_P.Refusal'Enum_Rep (S_P.Not_Granted)) then
         Why := P.Key_Not_Granted;
      end if;
      Key_Page := [others => 0];
   end Fetch_Key;

   procedure Handle_Ask (From : Process_ID; Request : Message) is
      Reference : constant CuBit.Memory_Grants.Grant_Reference :=
        (slot => Request.words (0), generation => Request.words (1));
      Ask_Bytes : constant Unsigned_64 := Request.words (2);
      Window_Bytes : constant Unsigned_64 := Request.words (3);
      Address : System.Address;
      Acquired, Returned : Boolean;
      Free_Slot : Integer := -1;

      procedure Give_Back (Why : P.Refusal) is
      begin
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         Refuse (Why);
      end Give_Back;
   begin
      if Ask_Bytes > Maximum_Ask_Bytes and then Ask_Bytes <= Window_Bytes
        and then Window_Bytes <= P.Maximum_Window_Bytes
      then
         --  A well-formed ask past the ceiling: a capacity error.
         Refuse (P.Request_Too_Large);
         return;
      elsif Ask_Bytes not in P.Request_Header_Bytes .. Maximum_Ask_Bytes
        or else Window_Bytes not in P.Minimum_Window_Bytes .. P.Maximum_Window_Bytes
        or else Ask_Bytes > Window_Bytes
      then
         Refuse (P.Malformed_Request);
         return;
      end if;
      for S in Slot_Index loop
         if Exchanges (S).Phase = Free and then Net_Lent (S) then
            Free_Slot := S;
            exit;
         end if;
      end loop;
      if Free_Slot < 0 then
         Refuse (P.Busy);
         return;
      end if;
      CuBit.Memory_Grants.Acquire
        (Reference, From, 0, Window_Bytes, CuBit.Memory_Grants.Write_Access, Address, Acquired);
      if not Acquired then
         Refuse (P.Malformed_Request);
         return;
      end if;
      declare
         Max_Tokens : constant Unsigned_64 := Get_Bytes (Address, P.Request_Max_Tokens_Offset, 4);
         Deadline : constant Unsigned_64 := Get_Bytes (Address, P.Request_Deadline_Offset, 4);
         Ceiling_Code : constant Unsigned_64 := Get_Bytes (Address, P.Request_Ceiling_Kind_Offset, 1);
         Ceiling : constant Unsigned_64 := Get_Bytes (Address, P.Request_Ceiling_Offset, 8);
         Model_Length : constant Unsigned_64 := Get_Bytes (Address, P.Request_Model_Length_Offset, 4);
         Mission_Length : constant Unsigned_64 := Get_Bytes (Address, P.Request_Mission_Length_Offset, 4);
         System_Length : constant Unsigned_64 := Get_Bytes (Address, P.Request_System_Length_Offset, 4);
         Flags : constant Unsigned_64 := Get_Bytes (Address, P.Request_Flags_Offset, 1);
         Tools_Length : constant Unsigned_64 := Get_Bytes (Address, P.Request_Tools_Length_Offset, 4);
         Conversation_Length : constant Unsigned_64 :=
           Get_Bytes (Address, P.Request_Conversation_Length_Offset, 4);
         Cache : constant Boolean := (Flags and P.Flag_Cache) /= 0;
         Capture : constant Boolean := (Flags and P.Flag_Capture) /= 0;
      begin
         if Max_Tokens not in 1 .. P.Maximum_Output_Tokens
           or else Deadline not in 1 .. P.Maximum_Deadline_Ms
           or else Ceiling_Code > 1
         then
            Give_Back (P.Invalid_Limits);
            return;
         elsif Model_Length > P.Maximum_Model_Bytes or else Mission_Length > P.Maximum_Mission_Bytes
           or else System_Length > P.Maximum_System_Bytes or else Tools_Length > Maximum_Ask_Bytes
           or else Conversation_Length > Maximum_Ask_Bytes
           or else (Flags and not Unsigned_64 (P.Known_Flags)) /= 0
           or else P.Request_Header_Bytes + Model_Length + Mission_Length + System_Length + Tools_Length
                   + Conversation_Length /= Ask_Bytes
           or else Window_Bytes < P.Minimum_Window_Bytes or else Window_Bytes > P.Maximum_Window_Bytes
         then
            Give_Back (P.Malformed_Request);
            return;
         end if;
         declare
            First_Text : constant Natural := P.Request_Header_Bytes;
            All_Text : String (1 .. Natural (Ask_Bytes) - First_Text)
              with Import, Address => Address + Storage_Offset (First_Text);
            M_Last : constant Natural := Natural (Model_Length);
            Mi_Last : constant Natural := M_Last + Natural (Mission_Length);
            S_Last : constant Natural := Mi_Last + Natural (System_Length);
            T_Last : constant Natural := S_Last + Natural (Tools_Length);
            Named_Model : constant String := All_Text (1 .. M_Last);
            Model : constant String := (if M_Last = 0 then Llm_Requests.Default_Model else Named_Model);
            Mission : constant String := All_Text (M_Last + 1 .. Mi_Last);
            System_Text : constant String := All_Text (Mi_Last + 1 .. S_Last);
            Tools : constant String := All_Text (S_Last + 1 .. T_Last);
            Conversation : constant String := All_Text (T_Last + 1 .. All_Text'Last);
            Kind : constant P.Ceiling_Kind := (if Ceiling_Code = 1 then P.Nano_Usd else P.Unlimited);
            Price : constant Llm_Costs.Price := Llm_Costs.Price_Of (Model);
         begin
            if not Llm_Requests.Valid_Model (Model) then
               Give_Back (P.Invalid_Model);
               return;
            elsif Kind = P.Nano_Usd and then not Price.Known then
               Give_Back (P.Unpriced_Model);
               return;
            elsif (for some C of Mission => C not in ' ' .. '~')
              or else not Llm_Requests.Valid_Tools (Tools)
              or else not Llm_Requests.Valid_Conversation (Conversation)
            then
               Give_Back (P.Malformed_Request);
               return;
            end if;
            declare
               S : constant Slot_Index := Free_Slot;
               E : Exchange renames Exchanges (S);
               Key_Value : String (1 .. P.Maximum_Key_Bytes);
               Key_Length : Natural;
               Why : P.Refusal;
               Fetched : Boolean;
            begin
               Fetch_Key (Key_Value, Key_Length, Why, Fetched);
               if not Fetched then
                  Give_Back (Why);
                  return;
               end if;
               E := (others => <>);
               E.Key := Key_Value;
               E.Key_Length := Key_Length;
               Key_Value := [others => ' '];
            end;
            declare
               Body_Length : constant Natural := Llm_Requests.Body_Length
                 (Model, P.Output_Tokens (Max_Tokens), System_Text, Tools, Conversation, Cache);
               S : constant Slot_Index := Free_Slot;
               E : Exchange renames Exchanges (S);
               Header_Length : constant Natural := Llm_Requests.Header_Length
                 (Host_Header, E.Key (1 .. E.Key_Length), Body_Length);
               Buffer_Address : constant System.Address := Net_Buffers (S)'Address;
               Rendered : Unsigned_64 := 0;
            begin
               if Header_Length + Body_Length > Maximum_Request_Bytes then
                  E := (others => <>);
                  Give_Back (P.Request_Too_Large);
                  return;
               end if;
               Rendered := Heap_Allocate (Unsigned_64 (Header_Length + Body_Length), Heap_Alignment);
               if Rendered = 0 then
                  E := (others => <>);
                  CuBit.Log.Warning ("llm: no memory for a request of" &
                                     Natural'Image (Header_Length + Body_Length) & " bytes");
                  Give_Back (P.Request_Too_Large);
                  return;
               end if;
               E.Request := Rendered;
               declare
                  Header : String (1 .. Header_Length)
                    with Import, Address => To_Address (Integer_Address (Rendered));
                  Body_Text : String (1 .. Body_Length)
                    with Import, Address => To_Address (Integer_Address (Rendered)) + Storage_Offset (Header_Length);
               begin
                  Llm_Requests.Write_Header (Host_Header, E.Key (1 .. E.Key_Length), Body_Length, Header);
                  Llm_Requests.Write_Body
                    (Model, P.Output_Tokens (Max_Tokens), System_Text, Tools, Conversation, Cache, Body_Text);
               end;
               Put_Text (Buffer_Address, 0, Endpoint_Text);
               E.Phase := Opening;
               E.Id := Next_Id;
               Next_Id := Next_Id + 1;
               E.Owner := From;
               E.Window := Reference;
               E.Window_Address := Address;
               E.Window_Bytes := Natural (Window_Bytes);
               E.Started_Ms := Now;
               E.Deadline_Ms := E.Started_Ms + Deadline;
               E.Request_Length := Header_Length + Body_Length;
               E.Model (1 .. Model'Length) := Model;
               E.Model_Length := Model'Length;
               E.Mission (1 .. Mission'Length) := Mission;
               E.Mission_Length := Mission'Length;
               declare
                  Limit : constant R.Limits :=
                    (Max_Tokens => P.Output_Tokens (Max_Tokens), Ceiling => Kind,
                     Ceiling_Nano_Usd => Ceiling, Price => Price, Capture => Capture);
               begin
                  E.Decoder := new R.Decoder (R.Capacity_For (Limit));
                  R.Begin_Response (E.Decoder.all, Limit);
               end;
               --  The ask's own bytes (the conversation) are not kept in the window.
               All_Text := [others => ' '];
               E.Open_Pending := capSubmit
                 (TLS_Service,
                  Message_Of (T.Open_Operation, Unsigned_8 (Endpoint_Text'Length),
                              Net_Grants (S).slot, Net_Buffer_Bytes, 0, Net_Grants (S).generation),
                  Token (S, Op_Open));
               Send_Current (P.Reply_OK, 1, E.Id);
               if not E.Open_Pending then
                  Stop_Exchange (S, P.Connect_Failed);
               end if;
            end;
         end;
      end;
   end Handle_Ask;

   procedure Handle_Pull (From : Process_ID; Request : Message) is
      Index : constant Integer := Find (From, Request.words (0));
      Max : constant Unsigned_64 := Request.words (1);
   begin
      if Index < 0 then
         Refuse (P.Unknown_Exchange);
         return;
      end if;
      declare
         E : Exchange renames Exchanges (Index);
      begin
         if E.Pull_Waiting or else Max > Unsigned_64 (E.Window_Bytes) or else Max < P.Summary_Bytes then
            Refuse (P.Malformed_Request);
            return;
         end if;
         declare
            Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
         begin
            if not Saved.Saved then
               Refuse (P.Service_Failure);
               return;
            end if;
            Pull_Replies (Index) := Saved.Handle;
         end;
         E.Pull_Waiting := True;
         E.Pull_Max := Natural (Max);
         Serve_Pull (Index);
      end;
   end Handle_Pull;

   procedure Handle_Cancel (From : Process_ID; Request : Message) is
      Index : constant Integer := Find (From, Request.words (0));
   begin
      if Index < 0 then
         Refuse (P.Unknown_Exchange);
         return;
      end if;
      if not R.Finished (Exchanges (Index).Decoder.all) then
         Stop_Exchange (Index, P.Cancelled);
      end if;
      Send_Current (P.Reply_OK, 0);
   end Handle_Cancel;

   function In_Flight return Natural is
      Total : Natural := 0;
   begin
      for E of Exchanges loop
         if E.Phase /= Free then
            Total := Total + 1;
         end if;
      end loop;
      return Total;
   end In_Flight;

   ---------------------------------------------------------------------------
   --  Deadlines.
   ---------------------------------------------------------------------------
   --  A request tls.svc could not take whole is offered again this soon.
   Write_Retry_Ms : constant := 10;
   --  An ended exchange whose client never pulls its end is let go.
   Abandoned_After_Ms : constant := 60_000;

   function Next_Deadline return Unsigned_64 is
      Due : Unsigned_64 := Unsigned_64'Last;
      Time : constant Unsigned_64 := Now;
   begin
      for E of Exchanges loop
         if E.Phase in Opening | Writing | Reading then
            Due := Unsigned_64'Min (Due, E.Deadline_Ms);
         end if;
         if E.Phase = Writing then
            Due := Unsigned_64'Min (Due, Time + Write_Retry_Ms);
         elsif E.Phase = Ended then
            Due := Unsigned_64'Min (Due, E.Ended_Ms + Abandoned_After_Ms);
         end if;
      end loop;
      return Due;
   end Next_Deadline;

   procedure Expire (Time : Unsigned_64) is
   begin
      for S in Slot_Index loop
         if Exchanges (S).Phase in Opening | Writing | Reading and then Time >= Exchanges (S).Deadline_Ms
           and then not R.Finished (Exchanges (S).Decoder.all)
         then
            Stop_Exchange (S, P.Deadline);
         end if;
         if Exchanges (S).Phase = Writing then
            Write_Request (S);
         elsif Exchanges (S).Phase = Ended and then not Exchanges (S).Pull_Waiting
           and then Time >= Exchanges (S).Ended_Ms + Abandoned_After_Ms
         then
            CuBit.Log.Warning ("llm: exchange" & Exchanges (S).Id'Image & " ended unread; released");
            Release (S);
         end if;
      end loop;
   end Expire;

   From : Process_ID;
   Request : Message;
   Found, Progress : Boolean;
   Completion : CompletionEntry;
   Ignore : Unsigned_64;
begin
   CuBit.Log.Info ("llm: starting");
   if Endpoint_Text'Length = 0 or else Colon = 0 or else not Llm_Requests.Valid_Host (Host_Header)
     or else Endpoint_Text'Length > Scheme_Area_Bytes or else Endpoint_Text'Length > Natural (Unsigned_8'Last)
   then
      CuBit.Log.Warning ("llm: the endpoint parameter is not host:port; exiting");
      return;
   end if;
   for S in Slot_Index loop
      CuBit.Memory_Grants.Create_Via_Capability
        (TLS_Service, Net_Buffers (S)'Address, Net_Buffer_Pages, True, Net_Grants (S), Net_Lent (S));
   end loop;
   CuBit.Memory_Grants.Create_Via_Capability
     (Secrets_Service, Key_Page'Address, 1, True, Key_Grant, Key_Page_Lent);
   if not S_P.Valid_Name (Key_Secret) then
      CuBit.Log.Warning ("llm: the api_key_secret parameter is not a secret name; exiting");
      return;
   elsif not Key_Page_Lent then
      CuBit.Log.Warning ("llm: secrets.svc unavailable; asks will be refused");
   end if;
   if not (for some L of Net_Lent => L) then
      CuBit.Log.Warning ("llm: tls.svc buffers unavailable; exiting");
      return;
   end if;
   Ignore := registerDriver (P.Service_Role, Startup.Registration);
   if Ignore = Unsigned_64'Last then
      CuBit.Log.Warning ("llm: registration failed");
      return;
   end if;
   CuBit.Log.Info ("llm: ready for " & Endpoint_Text);

   loop
      Progress := False;
      while Poll_Completion (Completion'Address) = 1 loop
         Progress := True;
         declare
            Slot_Word : constant Unsigned_64 := Shift_Right (Completion.token, 4) and 15;
            Op_Word : constant Unsigned_64 := Completion.token and 15;
            Id : constant Unsigned_64 := Shift_Right (Completion.token, 8);
         begin
            if Slot_Word < Maximum_Exchanges
              and then Op_Word in Unsigned_64 (Net_Operation'Enum_Rep (Op_Open)) ..
                                  Unsigned_64 (Net_Operation'Enum_Rep (Op_Read))
              and then Exchanges (Natural (Slot_Word)).Phase /= Free
              and then Exchanges (Natural (Slot_Word)).Id = Id
            then
               Complete (Natural (Slot_Word), Net_Operation'Enum_Val (Op_Word), Completion.msg);
            end if;
         end;
      end loop;

      Poll_Service_Request (From, Request, Found);
      Client_Now := From;
      if Found then
         Progress := True;
         case Request.tag.label is
            when P.Status_Operation =>
               Send_Current (P.Reply_OK, 1, Unsigned_64 (In_Flight));
            when P.Ask_Operation => Handle_Ask (From, Request);
            when P.Pull_Operation => Handle_Pull (From, Request);
            when P.Cancel_Operation => Handle_Cancel (From, Request);
            when others => Refuse (P.Malformed_Request);
         end case;
      end if;

      Expire (Now);
      if not Progress then
         if Wait_For_Activity_Until (Next_Deadline) = Unavailable then
            CuBit.Log.Warning ("llm: activity wait unavailable");
            return;
         end if;
      end if;
   end loop;
end Main;
