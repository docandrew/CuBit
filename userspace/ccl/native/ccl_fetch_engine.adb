------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.fetch on CuBit (see the spec).
------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Handles;
with CuBit.Memory_Grants;
with CuBit.TLS_Protocol;
with SPARKNaCl;
with SPARKTLSCrypto.Hashing.SHA256;
with Fetch_Urls;
with Http_Responses;
with CCL_Native_Bindings;
with CCL_Place_Files;

package body CCL_Fetch_Engine is
   use CuBit.Messages;
   use type CCL_Fetch.Outcome;
   use type CCL_Fetch.Existing_Policy;
   use type CCL_Place_Files.Result_Kind;
   use type Fetch_Urls.Refusal;
   use type Http_Responses.Refusal;
   use type Http_Responses.Framing;
   use type Http_Responses.Event_Kind;
   use type SPARKNaCl.Byte_Seq;
   package P renames CuBit.TLS_Protocol;
   package SHA renames SPARKTLSCrypto.Hashing.SHA256;
   package Files renames CCL_Place_Files;
   package R renames Http_Responses;
   subtype Outcome is CCL_Fetch.Outcome;

   PAGE_BYTES : constant := 4_096;
   --  The window lent to tls.svc: the authority, the request, then each
   --  read's plaintext (TLS records are at most 16 KiB).
   WINDOW_BYTES : constant := 32_768;
   WINDOW_PAGES : constant := WINDOW_BYTES / PAGE_BYTES;
   pragma Compile_Time_Error
     (WINDOW_BYTES < Fetch_Urls.MAXIMUM_URL_BYTES + 256, "the request must fit the window");
   --  RFC 9110 15.3.1 and 15.4: success and the redirects that name a
   --  Location (300 and 305 do not).
   OK_STATUS : constant := 200;
   TEMPORARY_SUFFIX : constant String := ".partial";

   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate_zeroed";
   procedure Heap_Free (Item : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";

   type Text_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, Text_Access);
   type Reader_Access is access R.Reader;
   procedure Free is new Ada.Unchecked_Deallocation (R.Reader, Reader_Access);

   --  What the outstanding submitted call is.
   type Call_Kind is (No_Call, Open_Call, Write_Call, Read_Call, Shut_Call);

   type Slot_Index is range 0 .. CCL_Fetch.MAXIMUM_FETCHES - 1;

   type Slot is record
      Used : Boolean := False;
      Id : Unsigned_64 := 0;
      Ended_As : Outcome := CCL_Fetch.Running;
      Pending : Call_Kind := No_Call;
      Sequence : Unsigned_64 := 0;
      Url : Fetch_Urls.Url;
      Redirects, Max_Redirects : Natural := 0;
      Max_Bytes : Unsigned_64 := 0;
      Deadline : Unsigned_64 := 0;
      Pinned : Boolean := False;
      Pin : SPARKNaCl.Bytes_32 := [others => 0];
      Existing : CCL_Fetch.Existing_Policy := CCL_Fetch.Refuse;
      Final_Path, Temporary_Path : Text_Access;
      File : Files.File;
      Reader : Reader_Access;
      Hash : SHA.Context;
      Window : Unsigned_64 := 0;
      Loan : CuBit.Memory_Grants.Grant_Reference;
      Lent : Boolean := False;
      Channel : Unsigned_64 := 0;
      Channel_Open : Boolean := False;
      --  The request's bytes not yet accepted by WRITE.
      Request_Sent, Request_Length : Natural := 0;
      --  A redirect waits for the old channel's SHUT.
      Redirecting : Boolean := False;
      --  The binding let it go: released once nothing is out.
      Forgotten : Boolean := False;
      Report : CCL_Fetch.Report;
   end record;
   Slots : array (Slot_Index) of Slot;
   Next_Id : Unsigned_64 := 0;

   --  "NFE" in the top three bytes; the slot takes the next byte.
   TOKEN_MARKER : constant Unsigned_64 := 16#4E46_4500_0000_0000#;
   MARKER_MASK : constant Unsigned_64 := 16#FFFF_FF00_0000_0000#;
   SLOT_SHIFT : constant := 32;
   SLOT_MASK : constant Unsigned_64 := 16#FF#;
   SEQUENCE_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;

   function Token_Of (K : Slot_Index) return Unsigned_64 is
     (TOKEN_MARKER or Shift_Left (Unsigned_64 (K), SLOT_SHIFT) or (Slots (K).Sequence and SEQUENCE_MASK));

   function Window_Address (K : Slot_Index; Offset : Natural := 0) return System.Address is
     (To_Address (Integer_Address (Slots (K).Window + Unsigned_64 (Offset))));

   function Hex_Digit (Value : Unsigned_8) return Character is
     (if Value < 10 then Character'Val (Character'Pos ('0') + Natural (Value))
      else Character'Val (Character'Pos ('a') + Natural (Value) - 10));

   procedure Put_Window (K : Slot_Index; Text : String) is
      Area : String (1 .. Text'Length) with Import, Address => Window_Address (K);
   begin
      Area := Text;
   end Put_Window;

   procedure Say (K : Slot_Index; Text : String) is
   begin
      Slots (K).Report.Detail := CCL_Fetch.Short (Text);
   end Say;

   procedure Submit (K : Slot_Index; Kind : Call_Kind; M : Message) is
      S : Slot renames Slots (K);
   begin
      S.Sequence := S.Sequence + 1;
      if capSubmit (CCL_Native_Bindings.Tls, M, Token_Of (K)) then
         S.Pending := Kind;
      else
         S.Pending := No_Call;
      end if;
   end Submit;

   procedure Submit_Shut (K : Slot_Index) is
      M : Message := NULL_MESSAGE;
   begin
      M.tag := (label => P.Shut_Operation, length => 1, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := Slots (K).Channel;
      Slots (K).Channel_Open := False;
      Submit (K, Shut_Call, M);
   end Submit_Shut;

   --  The end of a fetch: the channel shut, the file closed, the
   --  temporary file removed unless it became the target.
   procedure Finish (K : Slot_Index; As : Outcome; Detail : String := "") is
      S : Slot renames Slots (K);
      Ignored : Files.Result_Kind;
   begin
      if S.Ended_As /= CCL_Fetch.Running then
         return;
      end if;
      S.Ended_As := As;
      S.Report.Ended_As := As;
      if Detail'Length > 0 then
         Say (K, Detail);
      end if;
      Files.Close (S.File);
      if As /= CCL_Fetch.Stored and then S.Temporary_Path /= null then
         Files.Remove_File (S.Temporary_Path.all, Ignored);
      end if;
      if S.Channel_Open and then S.Pending = No_Call then
         Submit_Shut (K);
      end if;
   end Finish;

   function Failure_Outcome (Code : Unsigned_64) return Outcome is
     (if Code not in 1 .. Unsigned_64 (P.Failure'Enum_Rep (P.Failure'Last)) then CCL_Fetch.Connect_Failed
      else (case P.Failure'Enum_Val (Code) is
               when P.Scope_Denied => CCL_Fetch.Not_Granted,
               when P.Clock_Untrusted | P.Certificate_Expired | P.Certificate_Untrusted |
                    P.Certificate_Rejected | P.Protocol_Alert => CCL_Fetch.Tls_Failed,
               when P.Peer_Closed => CCL_Fetch.Truncated,
               when P.Timeout => CCL_Fetch.Deadline,
               when others => CCL_Fetch.Connect_Failed));

   function Failure_Name (Code : Unsigned_64) return String is
     (if Code not in 1 .. Unsigned_64 (P.Failure'Enum_Rep (P.Failure'Last)) then "the TLS service failed"
      else P.Name (P.Failure'Enum_Val (Code)));

   procedure Open_Channel (K : Slot_Index) is
      S : Slot renames Slots (K);
      Authority : constant String := Fetch_Urls.Authority (S.Url);
      M : Message := NULL_MESSAGE;
   begin
      Put_Window (K, Authority);
      M.tag := (label => P.Open_Operation, length => Unsigned_8 (Authority'Length), protocol => 0, ipc => <>,
                reserved => 0);
      M.words (0) := S.Loan.slot;
      M.words (1) := WINDOW_BYTES;
      M.words (3) := S.Loan.generation;
      R.Start (S.Reader.all, R.Body_Count (Unsigned_64'Min (S.Max_Bytes, Unsigned_64 (R.Body_Count'Last))));
      Submit (K, Open_Call, M);
      if S.Pending = No_Call then
         Finish (K, CCL_Fetch.Connect_Failed, "tls.svc refused the request queue");
      end if;
   end Open_Channel;

   procedure Write_Request (K : Slot_Index) is
      S : Slot renames Slots (K);
      M : Message := NULL_MESSAGE;
   begin
      M.tag := (label => P.Write_Operation, length => 3, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := S.Channel;
      M.words (1) := Unsigned_64 (S.Request_Sent);
      M.words (2) := Unsigned_64 (S.Request_Length - S.Request_Sent);
      Submit (K, Write_Call, M);
   end Write_Request;

   procedure Read_More (K : Slot_Index) is
      S : Slot renames Slots (K);
      M : Message := NULL_MESSAGE;
   begin
      M.tag := (label => P.Read_Operation, length => 4, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := S.Channel;
      M.words (1) := 0;
      M.words (2) := WINDOW_BYTES;
      M.words (3) := S.Deadline;
      Submit (K, Read_Call, M);
   end Read_More;

   --  A body run of the window: hashed and written.
   procedure Take_Body (K : Slot_Index; First, Last : Positive) is
      S : Slot renames Slots (K);
      Count : constant Natural := Last - First + 1;
      Bytes : constant SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (Count - 1))
        with Import, Address => Window_Address (K, First - 1);
      Written : Files.Result_Kind;
   begin
      SHA.Update (S.Hash, Bytes);
      Files.Write (S.File, S.Report.Bytes, Window_Address (K, First - 1), Count, Written);
      if Written /= Files.Done then
         Finish (K, CCL_Fetch.Write_Failed, "the filesystem refused a write to " & S.Temporary_Path.all);
         return;
      end if;
      S.Report.Bytes := S.Report.Bytes + Unsigned_64 (Count);
   end Take_Body;

   --  The body is whole: digest, pin, then the rename into place.
   procedure Complete (K : Slot_Index) is
      S : Slot renames Slots (K);
      Digest : SHA.Digest;
      Renamed : Files.Result_Kind;
   begin
      SHA.Final (S.Hash, Digest);
      for I in Digest'Range loop
         S.Report.Sha256 (Natural (I) * 2 + 1) := Hex_Digit (Shift_Right (Digest (I), 4));
         S.Report.Sha256 (Natural (I) * 2 + 2) := Hex_Digit (Digest (I) and 16#0F#);
      end loop;
      S.Report.Sha256_Known := True;
      Files.Close (S.File);
      if S.Pinned and then Digest /= S.Pin then
         Finish (K, CCL_Fetch.Digest_Mismatch, "the body's SHA-256 is not the pinned one; nothing was kept");
         return;
      elsif S.Existing = CCL_Fetch.Refuse and then Files.Exists (S.Final_Path.all) then
         Finish (K, CCL_Fetch.Exists, S.Final_Path.all & " appeared while fetching; nothing was replaced");
         return;
      end if;
      Files.Rename (S.Temporary_Path.all, S.Final_Path.all, Renamed);
      if Renamed /= Files.Done then
         Finish (K, CCL_Fetch.Write_Failed, "could not rename the download to " & S.Final_Path.all);
         return;
      end if;
      Finish (K, CCL_Fetch.Stored, S.Final_Path.all);
   end Complete;

   --  The head of a response: success, a redirect, or another status.
   procedure Take_Head (K : Slot_Index) is
      S : Slot renames Slots (K);
      Status : constant Natural := R.Status (S.Reader.all);
   begin
      S.Report.Status := Status;
      if Status in 301 | 302 | 303 | 307 | 308 and then R.Location (S.Reader.all)'Length > 0 then
         declare
            Next : Fetch_Urls.Url;
            Why : Fetch_Urls.Refusal;
         begin
            Fetch_Urls.Resolve (S.Url, R.Location (S.Reader.all), Next, Why);
            if Why /= Fetch_Urls.No_Refusal then
               Finish (K, CCL_Fetch.Bad_Url, "a redirect to a URL that cannot be fetched (" &
                       Fetch_Urls.Name (Why) & ")");
            elsif S.Redirects >= S.Max_Redirects then
               Finish (K, CCL_Fetch.Too_Many_Redirects, "more redirects than max_redirects allows");
            else
               S.Redirects := S.Redirects + 1;
               S.Report.Redirects := S.Redirects;
               S.Url := Next;
               declare
                  Image : constant String := Fetch_Urls.Image (S.Url);
               begin
                  S.Report.Url_Length := Natural'Min (Image'Length, CCL_Fetch.MAXIMUM_URL);
                  S.Report.Url (1 .. S.Report.Url_Length) :=
                    Image (Image'First .. Image'First + S.Report.Url_Length - 1);
               end;
               S.Redirecting := True;
            end if;
         end;
      elsif Status /= OK_STATUS then
         Finish (K, CCL_Fetch.Http_Status, "the server answered" & Natural'Image (Status));
      elsif R.Body_Framing (S.Reader.all) = R.Length_Delimited then
         S.Report.Expected_Known := True;
         S.Report.Expected := Unsigned_64 (R.Content_Length (S.Reader.all));
         if S.Report.Expected > S.Max_Bytes then
            Finish (K, CCL_Fetch.Too_Large, "the body is" & Unsigned_64'Image (S.Report.Expected) &
                    " bytes, more than max_bytes");
         end if;
      end if;
   end Take_Head;

   function Response_Outcome (Why : R.Refusal) return Outcome is
     (case Why is
         when R.Body_Too_Large => CCL_Fetch.Too_Large,
         when R.Truncated => CCL_Fetch.Truncated,
         when others => CCL_Fetch.Bad_Response);

   --  Count bytes of plaintext at the window's start.
   procedure Take_Bytes (K : Slot_Index; Count : Natural) is
      S : Slot renames Slots (K);
      Input : constant String (1 .. Count) with Import, Address => Window_Address (K);
      Next : Positive := 1;
      E : R.Event;
   begin
      while S.Ended_As = CCL_Fetch.Running and then not S.Redirecting loop
         R.Step (S.Reader.all, Input, Next, E);
         case E.Kind is
            when R.Head_Ready => Take_Head (K);
            when R.Body_Run =>
               if R.Status (S.Reader.all) = OK_STATUS then
                  Take_Body (K, E.First, E.Last);
               end if;
            when R.Complete =>
               if Next <= Input'Last then
                  Finish (K, CCL_Fetch.Bad_Response, "the response has bytes after its body");
               elsif R.Status (S.Reader.all) = OK_STATUS then
                  Complete (K);
               end if;
               exit;
            when R.Refused =>
               Finish (K, Response_Outcome (E.Why), "the response was refused: " & R.Name (E.Why));
               exit;
            when R.Need_More => null;
         end case;
         exit when Next > Input'Last and then E.Kind /= R.Head_Ready;
      end loop;
   end Take_Bytes;

   procedure Received (K : Slot_Index; Receipt : CompletionEntry) is
      S : Slot renames Slots (K);
      Kind : constant Call_Kind := S.Pending;
      Label : constant Unsigned_32 := (if Receipt.valid then Receipt.msg.tag.label else P.Reply_Error);
      Word : constant Unsigned_64 := Receipt.msg.words (0);
   begin
      S.Pending := No_Call;
      case Kind is
         when No_Call =>
            null;
         when Open_Call =>
            if Label = P.Reply_OK then
               S.Channel := Word;
               S.Channel_Open := True;
            end if;
            if S.Ended_As /= CCL_Fetch.Running then
               --  Ended (cancelled, deadline) while connecting.
               if S.Channel_Open then
                  Submit_Shut (K);
               end if;
            elsif Label /= P.Reply_OK then
               Finish (K, (if Label = P.Reply_Error then Failure_Outcome (Word) else CCL_Fetch.Connect_Failed),
                       "connecting to " & Fetch_Urls.Authority (S.Url) & ": " &
                       (if Label = P.Reply_Error then Failure_Name (Word) else "no answer"));
            else
               declare
                  Request : constant String := Fetch_Urls.Request (S.Url);
               begin
                  Put_Window (K, Request);
                  S.Request_Sent := 0;
                  S.Request_Length := Request'Length;
                  Write_Request (K);
               end;
            end if;
         when Write_Call =>
            if S.Ended_As /= CCL_Fetch.Running then
               Submit_Shut (K);
            elsif Label /= P.Reply_OK or else Word = 0
              or else Word > Unsigned_64 (S.Request_Length - S.Request_Sent)
            then
               Finish (K, CCL_Fetch.Connect_Failed, "sending the request failed");
            else
               S.Request_Sent := S.Request_Sent + Natural (Word);
               if S.Request_Sent < S.Request_Length then
                  Write_Request (K);
               else
                  Read_More (K);
               end if;
            end if;
         when Read_Call =>
            if S.Ended_As /= CCL_Fetch.Running then
               Submit_Shut (K);
            elsif Label = P.Reply_OK and then Word in 1 .. WINDOW_BYTES then
               Take_Bytes (K, Natural (Word));
               if S.Ended_As = CCL_Fetch.Running then
                  if S.Redirecting then
                     Submit_Shut (K);
                  else
                     Read_More (K);
                  end if;
               end if;
            elsif Label = P.Reply_OK and then Word = 0 then
               Read_More (K);
            elsif Label = P.Reply_EOF then
               --  close_notify: the end the server meant.
               declare
                  E : R.Event;
               begin
                  R.Input_Ended (S.Reader.all, E);
                  S.Channel_Open := False;
                  if E.Kind = R.Complete and then R.Status (S.Reader.all) = OK_STATUS then
                     Complete (K);
                  elsif E.Kind = R.Refused then
                     Finish (K, Response_Outcome (E.Why), "the response ended early: " & R.Name (E.Why));
                  else
                     Finish (K, CCL_Fetch.Bad_Response, "the connection closed after a redirect's head");
                  end if;
                  Submit_Shut (K);
               end;
            else
               Finish (K, (if Label = P.Reply_Error then Failure_Outcome (Word) else CCL_Fetch.Bad_Response),
                       "reading: " & (if Label = P.Reply_Error then Failure_Name (Word)
                                      elsif Label = P.Reply_OK then "tls.svc reported more than the window"
                                      else "no answer"));
            end if;
         when Shut_Call =>
            if S.Redirecting and then S.Ended_As = CCL_Fetch.Running then
               S.Redirecting := False;
               Open_Channel (K);
            end if;
      end case;
   end Received;

   procedure Deliver (Receipt : CompletionEntry; Consumed : out Boolean) is
      Slot_Word : constant Unsigned_64 := Shift_Right (Receipt.token, SLOT_SHIFT) and SLOT_MASK;
   begin
      Consumed := False;
      if (Receipt.token and MARKER_MASK) /= TOKEN_MARKER or else Slot_Word > Unsigned_64 (Slot_Index'Last) then
         return;
      end if;
      Consumed := True;
      declare
         K : constant Slot_Index := Slot_Index (Slot_Word);
      begin
         if Slots (K).Used and then Slots (K).Pending /= No_Call and then Receipt.token = Token_Of (K) then
            Received (K, Receipt);
         end if;
      end;
   end Deliver;

   function Find (Id : Unsigned_64) return Integer is
   begin
      for K in Slot_Index loop
         if Slots (K).Used and then Slots (K).Id = Id then
            return Integer (K);
         end if;
      end loop;
      return -1;
   end Find;

   --  Let go of slot K's memory: the loan revoked, the window wiped.
   procedure Release (K : Slot_Index) is
      S : Slot renames Slots (K);
      Revoked : Boolean;
   begin
      if S.Lent then
         CuBit.Memory_Grants.Revoke (S.Loan, Revoked);
      end if;
      if S.Window /= 0 then
         declare
            Area : String (1 .. WINDOW_BYTES) with Import, Address => Window_Address (K);
         begin
            Area := [others => ASCII.NUL];
         end;
         Heap_Free (S.Window);
      end if;
      Free (S.Final_Path);
      Free (S.Temporary_Path);
      Free (S.Reader);
      S := (others => <>);
   end Release;

   function Hex_Value (C : Character) return Unsigned_8 is
     (case C is
         when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
         when 'a' .. 'f' => Character'Pos (C) - Character'Pos ('a') + 10,
         when 'A' .. 'F' => Character'Pos (C) - Character'Pos ('A') + 10,
         when others => 0);

   procedure Start
     (Url, Root, Path, Name : String; Pinned : Boolean; Pin_Hex : String;
      Existing : CCL_Fetch.Existing_Policy; Max_Bytes : Unsigned_64; Max_Redirects : Natural;
      Deadline_Ms : Positive; Now_Ms : Unsigned_64;
      Id : out Unsigned_64; Result : out CCL_Fetch.Start_Result; Why : out CCL_Fetch.Short_Text)
   is
      Parsed : Fetch_Urls.Url;
      Refused_As : Fetch_Urls.Refusal;
      K : Integer := -1;
      Directory : constant String := (if Path'Length = 0 then Root else Root & "/" & Path);
      Final : constant String := Directory & "/" & Name;
      Temporary : constant String := Directory & "/." & Name & TEMPORARY_SUFFIX;
      Created : Files.Result_Kind;
   begin
      Id := 0;
      Why := CCL_Fetch.Short ("");
      if not CuBit.Handles.Valid (CCL_Native_Bindings.Tls) then
         Result := CCL_Fetch.Not_Granted;
         Why := CCL_Fetch.Short ("the program's manifest must request the TLS service (service ""tls"" ""tls"")");
         return;
      elsif Pinned and then (Pin_Hex'Length /= CCL_Fetch.SHA256_HEX_DIGITS
                              or else (for some C of Pin_Hex => C not in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'))
      then
         Result := CCL_Fetch.Refused;
         Why := CCL_Fetch.Short ("a pinned SHA-256 is 64 hex digits");
         return;
      end if;
      Fetch_Urls.Parse (Url, Parsed, Refused_As);
      if Refused_As /= Fetch_Urls.No_Refusal then
         Result := CCL_Fetch.Refused;
         Why := CCL_Fetch.Short ("the URL cannot be fetched: " & Fetch_Urls.Name (Refused_As) &
                                 " (https://NAME[:PORT]/PATH only)");
         return;
      end if;
      for S in Slot_Index loop
         if not Slots (S).Used then
            K := Integer (S);
            exit;
         end if;
      end loop;
      if K < 0 then
         Result := CCL_Fetch.Refused;
         Why := CCL_Fetch.Short ("this program already follows" & Natural'Image (CCL_Fetch.MAXIMUM_FETCHES) &
                                 " fetches");
         return;
      end if;
      if Existing = CCL_Fetch.Refuse and then Files.Exists (Final) then
         Result := CCL_Fetch.Refused;
         Why := CCL_Fetch.Short (Final & " exists (existing => Fetch_Existing.Replace replaces it)");
         return;
      end if;
      declare
         S : Slot renames Slots (Slot_Index (K));
      begin
         --  The place first: a fetch into a place it may not write never
         --  reaches the network.
         Files.Create (Temporary, Exclusive => False, Item => S.File, Result => Created);
         if Created /= Files.Done then
            Result := (if Created in Files.Not_Granted | Files.Not_Found then CCL_Fetch.Not_Granted
                       else CCL_Fetch.Unavailable);
            Why := CCL_Fetch.Short
              ((if Created = Files.Not_Granted then Directory & " is outside this program's writable places"
                elsif Created = Files.Not_Found then "nothing is at " & Directory
                else "the filesystem could not create " & Temporary));
            Files.Close (S.File);
            return;
         end if;
         S.Window := Allocate_Zeroed (WINDOW_BYTES, PAGE_BYTES);
         if S.Window /= 0 then
            CuBit.Memory_Grants.Create_Via_Capability
              (CCL_Native_Bindings.Tls, Window_Address (Slot_Index (K)), WINDOW_PAGES, True, S.Loan, S.Lent);
         end if;
         if S.Window = 0 or else not S.Lent then
            declare
               Ignored : Files.Result_Kind;
            begin
               Files.Close (S.File);
               Files.Remove_File (Temporary, Ignored);
            end;
            Release (Slot_Index (K));
            Result := CCL_Fetch.Unavailable;
            Why := CCL_Fetch.Short ("no memory, or no loan, for the fetch's window");
            return;
         end if;
         Next_Id := Next_Id + 1;
         S.Used := True;
         S.Id := Next_Id;
         S.Url := Parsed;
         S.Max_Bytes := Max_Bytes;
         S.Max_Redirects := Max_Redirects;
         S.Deadline := Now_Ms + Unsigned_64 (Deadline_Ms);
         S.Pinned := Pinned;
         if Pinned then
            for I in S.Pin'Range loop
               S.Pin (I) := Hex_Value (Pin_Hex (Pin_Hex'First + Natural (I) * 2)) * 16 +
                 Hex_Value (Pin_Hex (Pin_Hex'First + Natural (I) * 2 + 1));
            end loop;
         end if;
         S.Existing := Existing;
         S.Final_Path := new String'(Final);
         S.Temporary_Path := new String'(Temporary);
         S.Reader := new R.Reader;
         SHA.Init (S.Hash);
         declare
            Image : constant String := Fetch_Urls.Image (Parsed);
         begin
            S.Report.Url_Length := Natural'Min (Image'Length, CCL_Fetch.MAXIMUM_URL);
            S.Report.Url (1 .. S.Report.Url_Length) := Image (Image'First .. Image'First + S.Report.Url_Length - 1);
         end;
         Id := S.Id;
         Result := CCL_Fetch.Started;
         Open_Channel (Slot_Index (K));
      end;
   end Start;

   procedure Poll (Id : Unsigned_64; Item : out CCL_Fetch.Report; Found : out Boolean) is
      K : constant Integer := Find (Id);
   begin
      Found := K >= 0;
      Item := (if Found then Slots (Slot_Index (K)).Report else (others => <>));
   end Poll;

   procedure Cancel (Id : Unsigned_64) is
      K : constant Integer := Find (Id);
   begin
      if K >= 0 then
         Finish (Slot_Index (K), CCL_Fetch.Cancelled, "cancelled");
      end if;
   end Cancel;

   procedure Forget (Id : Unsigned_64) is
      K : constant Integer := Find (Id);
   begin
      --  A slot whose call is still out stays until its completion (the
      --  window is lent until then).
      if K >= 0 and then Slots (Slot_Index (K)).Ended_As /= CCL_Fetch.Running then
         Slots (Slot_Index (K)).Forgotten := True;
         if Slots (Slot_Index (K)).Pending = No_Call and then not Slots (Slot_Index (K)).Channel_Open then
            Release (Slot_Index (K));
         end if;
      end if;
   end Forget;

   function Running return Boolean is
     (for some S of Slots => S.Used and then S.Ended_As = CCL_Fetch.Running);

   procedure Pump (Now_Ms : Unsigned_64; Next_Due : out Unsigned_64) is
   begin
      Next_Due := Unsigned_64'Last;
      for K in Slot_Index loop
         if Slots (K).Used and then Slots (K).Ended_As = CCL_Fetch.Running then
            if Now_Ms >= Slots (K).Deadline then
               Finish (K, CCL_Fetch.Deadline, "the deadline passed");
            else
               Next_Due := Unsigned_64'Min (Next_Due, Slots (K).Deadline - Now_Ms);
            end if;
         elsif Slots (K).Used and then Slots (K).Pending = No_Call and then Slots (K).Channel_Open then
            Submit_Shut (K);
         elsif Slots (K).Used and then Slots (K).Forgotten and then Slots (K).Pending = No_Call then
            Release (K);
         end if;
      end loop;
   end Pump;
end CCL_Fetch_Engine;
