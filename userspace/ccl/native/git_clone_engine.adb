------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git clone over HTTPS on CuBit (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Handles;
with CuBit.Memory_Grants;
with CuBit.TLS_Protocol;
with Inflate_Streams;
with Fetch_Urls;
with Http_Responses;
with Git_Protocol;
with Git_Clone_Sessions;
with CCL_Native_Bindings;
with CCL_Place_Files;

package body Git_Clone_Engine is
   use CuBit.Messages;
   use type CCL_Place_Files.Result_Kind;
   use type Fetch_Urls.Refusal;
   use type Http_Responses.Event_Kind;
   use type Git_Protocol.Object_Format;
   package P renames CuBit.TLS_Protocol;
   package Files renames CCL_Place_Files;
   package R renames Http_Responses;

   PAGE_BYTES : constant := 4_096;
   --  The window lent to tls.svc: the authority, then the request a piece
   --  at a time, then each read's plaintext (TLS records are at most 16 KiB).
   WINDOW_BYTES : constant := 32_768;
   WINDOW_PAGES : constant := WINDOW_BYTES / PAGE_BYTES;
   --  RFC 9110 15.3.1 and 15.4.
   OK_STATUS : constant := 200;

   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate_zeroed";
   procedure Heap_Free (Item : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";

   ---------------------------------------------------------------------------
   --  The store: the program's places, through the filesystem service.

   type Place_Store is limited record
      File : Files.File;
   end record;

   procedure Make_Directory (S : in out Place_Store; Path : String; Ok : out Boolean) is
      pragma Unreferenced (S);
      Made : Files.Result_Kind;
   begin
      Files.Make_Directory (Path, Made);
      Ok := Made = Files.Done;
   end Make_Directory;

   procedure Create_File (S : in out Place_Store; Path : String; Ok : out Boolean) is
      Made : Files.Result_Kind;
   begin
      Files.Create (Path, Exclusive => True, Item => S.File, Result => Made);
      Ok := Made = Files.Done;
   end Create_File;

   procedure Write
     (S : in out Place_Store; Offset : Inflate_Streams.Byte_Count; Data : Inflate_Streams.Byte_Array;
      Ok : out Boolean)
   is
      Written : Files.Result_Kind := Files.Done;
   begin
      if Data'Length > 0 then
         Files.Write (S.File, Unsigned_64 (Offset), Data'Address, Data'Length, Written);
      end if;
      Ok := Written = Files.Done;
   end Write;

   procedure Close_File (S : in out Place_Store) is
   begin
      Files.Close (S.File);
   end Close_File;

   procedure Remove_File (S : in out Place_Store; Path : String) is
      pragma Unreferenced (S);
      Ignored : Files.Result_Kind;
   begin
      Files.Remove_File (Path, Ignored);
   end Remove_File;

   procedure Remove_Directory (S : in out Place_Store; Path : String) is
      pragma Unreferenced (S);
      Ignored : Files.Result_Kind;
   begin
      Files.Remove_Directory (Path, Ignored);
   end Remove_Directory;

   procedure Rename (S : in out Place_Store; From, To : String; Ok : out Boolean) is
      pragma Unreferenced (S);
      Renamed : Files.Result_Kind;
   begin
      Files.Rename (From, To, Renamed);
      Ok := Renamed = Files.Done;
   end Rename;

   function Exists (S : in out Place_Store; Path : String) return Boolean is
      pragma Unreferenced (S);
   begin
      return Files.Exists (Path);
   end Exists;

   package Sessions is new Git_Clone_Sessions
     (Place_Store, Make_Directory, Create_File, Write, Close_File, Remove_File, Remove_Directory, Rename, Exists);
   use type Sessions.Exchange;
   use type Sessions.Outcome;

   type Session_Access is access Sessions.Session;
   type Store_Access is access Place_Store;
   type Reader_Access is access R.Reader;
   type Text_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (Sessions.Session, Session_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Place_Store, Store_Access);
   procedure Free is new Ada.Unchecked_Deallocation (R.Reader, Reader_Access);
   procedure Free is new Ada.Unchecked_Deallocation (String, Text_Access);

   function From_Session (O : Sessions.Outcome) return Outcome is
     (case O is
         when Sessions.In_Progress => Running,
         when Sessions.Cloned => Cloned,
         when Sessions.Target_Exists => Target_Exists,
         when Sessions.Bad_Directory => Bad_Directory,
         when Sessions.Bad_Url => Bad_Url,
         when Sessions.Write_Failed => Write_Failed,
         when Sessions.Not_Smart_Http => Not_Smart_Http,
         when Sessions.Bad_Refs => Bad_Refs,
         when Sessions.Bad_Fetch => Bad_Fetch,
         when Sessions.Remote_Error => Remote_Error,
         when Sessions.Bad_Pack => Bad_Pack,
         when Sessions.Bad_Delta => Bad_Delta,
         when Sessions.Too_Many_Objects => Too_Many_Objects,
         when Sessions.Too_Large => Too_Large,
         when Sessions.Missing_Object => Missing_Object,
         when Sessions.Bad_Object => Bad_Object,
         when Sessions.Path_Too_Long => Path_Too_Long,
         when Sessions.No_Refs => No_Refs,
         when Sessions.Abandoned => Cancelled);

   ---------------------------------------------------------------------------
   --  The clones followed.

   type Call_Kind is (No_Call, Open_Call, Write_Call, Read_Call, Shut_Call);

   type Slot_Index is range 0 .. MAXIMUM_CLONES - 1;

   type Slot is record
      Used : Boolean := False;
      Id : Unsigned_64 := 0;
      Ended_As : Outcome := Running;
      Pending : Call_Kind := No_Call;
      Sequence : Unsigned_64 := 0;
      --  The repository's URL (redirects of the advertisement move it).
      Url : Fetch_Urls.Url;
      Ceiling : Limits;
      Redirects : Natural := 0;
      Deadline : Unsigned_64 := 0;
      Session : Session_Access;
      Store : Store_Access;
      Reader : Reader_Access;
      Window : Unsigned_64 := 0;
      Loan : CuBit.Memory_Grants.Grant_Reference;
      Lent : Boolean := False;
      Channel : Unsigned_64 := 0;
      Channel_Open : Boolean := False;
      --  The exchange's request, head and body, and how much was accepted.
      Request : Text_Access;
      Request_Sent : Natural := 0;
      Request_Piece : Natural := 0;
      --  After the old channel's SHUT: the advertisement again at a
      --  redirect's target (Redirecting), or the next exchange (Advancing).
      Redirecting : Boolean := False;
      Advancing : Boolean := False;
      Forgotten : Boolean := False;
      Report : Git_Clone_Reports.Report;
   end record;
   Slots : array (Slot_Index) of Slot;
   Next_Id : Unsigned_64 := 0;

   --  "GCE" in the top three bytes; the slot takes the next byte.
   TOKEN_MARKER : constant Unsigned_64 := 16#4743_4500_0000_0000#;
   MARKER_MASK : constant Unsigned_64 := 16#FFFF_FF00_0000_0000#;
   SLOT_SHIFT : constant := 32;
   SLOT_MASK : constant Unsigned_64 := 16#FF#;
   SEQUENCE_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;

   function Token_Of (K : Slot_Index) return Unsigned_64 is
     (TOKEN_MARKER or Shift_Left (Unsigned_64 (K), SLOT_SHIFT) or (Slots (K).Sequence and SEQUENCE_MASK));

   function Window_Address (K : Slot_Index; Offset : Natural := 0) return System.Address is
     (To_Address (Integer_Address (Slots (K).Window + Unsigned_64 (Offset))));

   procedure Put_Window (K : Slot_Index; Text : String) is
      Area : String (1 .. Text'Length) with Import, Address => Window_Address (K);
   begin
      Area := Text;
   end Put_Window;

   procedure Say (K : Slot_Index; Text : String) is
   begin
      Slots (K).Report.Detail := Short (Text);
   end Say;

   procedure Note_Url (K : Slot_Index) is
      Image : constant String := Fetch_Urls.Image (Slots (K).Url);
   begin
      Slots (K).Report.Url_Length := Natural'Min (Image'Length, MAXIMUM_URL);
      Slots (K).Report.Url (1 .. Slots (K).Report.Url_Length) :=
        Image (Image'First .. Image'First + Slots (K).Report.Url_Length - 1);
   end Note_Url;

   procedure Note_Session (K : Slot_Index) is
      S : Slot renames Slots (K);
   begin
      S.Report.Refs := Sessions.Refs (S.Session.all);
      S.Report.Objects := Sessions.Objects (S.Session.all);
      S.Report.Files := Sessions.Files (S.Session.all);
      S.Report.Format :=
        (if Sessions.Format (S.Session.all) = Git_Protocol.Sha256_Format then Sha256 else Sha1);
   end Note_Session;

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

   --  The end of a clone: what was made removed unless it was cloned, the
   --  channel shut.
   procedure Finish (K : Slot_Index; As : Outcome; Detail : String := "") is
      S : Slot renames Slots (K);
   begin
      if S.Ended_As /= Running then
         return;
      end if;
      if S.Session /= null and then Sessions.Next (S.Session.all) /= Sessions.Finished then
         Sessions.Abandon (S.Session.all, S.Store.all, Detail);
      end if;
      S.Ended_As := As;
      S.Report.Ended_As := As;
      if Detail'Length > 0 then
         Say (K, Detail);
      end if;
      if S.Session /= null then
         Note_Session (K);
      end if;
      if S.Channel_Open and then S.Pending = No_Call then
         Submit_Shut (K);
      end if;
   end Finish;

   --  The session ended (or refused): the clone ends as it did.
   procedure Session_Ended (K : Slot_Index) is
      S : Slot renames Slots (K);
   begin
      Finish (K, From_Session (Sessions.Result (S.Session.all)), Sessions.Detail (S.Session.all));
   end Session_Ended;

   function Failure_Outcome (Code : Unsigned_64) return Outcome is
     (if Code not in 1 .. Unsigned_64 (P.Failure'Enum_Rep (P.Failure'Last)) then Connect_Failed
      else (case P.Failure'Enum_Val (Code) is
               when P.Scope_Denied => Not_Granted,
               when P.Clock_Untrusted | P.Certificate_Expired | P.Certificate_Untrusted |
                    P.Certificate_Rejected | P.Protocol_Alert => Tls_Failed,
               when P.Peer_Closed => Truncated,
               when P.Timeout => Deadline,
               when others => Connect_Failed));

   function Failure_Name (Code : Unsigned_64) return String is
     (if Code not in 1 .. Unsigned_64 (P.Failure'Enum_Rep (P.Failure'Last)) then "the TLS service failed"
      else P.Name (P.Failure'Enum_Val (Code)));

   --  A connection for the session's next exchange.
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
      R.Start (S.Reader.all,
               R.Body_Count (Unsigned_64'Min (S.Ceiling.Maximum_Bytes, Unsigned_64 (R.Body_Count'Last))));
      Submit (K, Open_Call, M);
      if S.Pending = No_Call then
         Finish (K, Connect_Failed, "tls.svc refused the request queue");
      end if;
   end Open_Channel;

   --  The next piece of the request: as much as the window holds.
   procedure Write_Request (K : Slot_Index) is
      S : Slot renames Slots (K);
      Piece : constant Natural := Natural'Min (S.Request'Length - S.Request_Sent, WINDOW_BYTES);
      M : Message := NULL_MESSAGE;
   begin
      Put_Window (K, S.Request (S.Request'First + S.Request_Sent .. S.Request'First + S.Request_Sent + Piece - 1));
      S.Request_Piece := Piece;
      M.tag := (label => P.Write_Operation, length => 3, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := S.Channel;
      M.words (1) := 0;
      M.words (2) := Unsigned_64 (Piece);
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

   --  The channel is open: the exchange's request, head then body.
   procedure Send_Request (K : Slot_Index) is
      S : Slot renames Slots (K);
      Next : constant Sessions.Exchange := Sessions.Next (S.Session.all);
   begin
      Free (S.Request);
      if Next = Sessions.Advertisement then
         S.Request := new String'(Fetch_Urls.Git_Request (S.Url, Fetch_Urls.Info_Refs, 0));
      else
         declare
            Request_Body : constant String := Sessions.Request_Body (S.Session.all);
         begin
            S.Request := new String'(Fetch_Urls.Git_Request (S.Url, Fetch_Urls.Upload_Pack, Request_Body'Length)
                                     & Request_Body);
         end;
      end if;
      S.Request_Sent := 0;
      Write_Request (K);
   end Send_Request;

   --  The advertisement's request URL redirected: the repository moves to
   --  the target, which must name the same service path.
   procedure Redirect (K : Slot_Index; Location : String) is
      S : Slot renames Slots (K);
      Base_Image : constant String := Fetch_Urls.Image (S.Url);
      Base_Last : constant Natural :=
        (if Base_Image (Base_Image'Last) = '/' then Base_Image'Last - 1 else Base_Image'Last);
      Asked, Target, Moved : Fetch_Urls.Url;
      Why : Fetch_Urls.Refusal;
      Suffix : constant String := Sessions.ADVERTISEMENT_PATH;
   begin
      Fetch_Urls.Parse (Base_Image (Base_Image'First .. Base_Last) & Suffix, Asked, Why);
      if Why = Fetch_Urls.No_Refusal then
         Fetch_Urls.Resolve (Asked, Location, Target, Why);
      end if;
      if Why /= Fetch_Urls.No_Refusal then
         Finish (K, Bad_Url, "a redirect to a URL that cannot be fetched (" & Fetch_Urls.Name (Why) & ")");
         return;
      end if;
      declare
         Image : constant String := Fetch_Urls.Image (Target);
      begin
         if Image'Length <= Suffix'Length or else Image (Image'Last - Suffix'Length + 1 .. Image'Last) /= Suffix
         then
            Finish (K, Bad_Url, "a redirect away from git's info/refs");
            return;
         end if;
         Fetch_Urls.Parse (Image (Image'First .. Image'Last - Suffix'Length), Moved, Why);
      end;
      if Why /= Fetch_Urls.No_Refusal or else not Fetch_Urls.Git_Repository (Moved) then
         Finish (K, Bad_Url, "a redirect to a URL that is not a repository");
      elsif S.Redirects >= S.Ceiling.Maximum_Redirects then
         Finish (K, Too_Many_Redirects, "more redirects than max_redirects allows");
      else
         S.Redirects := S.Redirects + 1;
         S.Report.Redirects := S.Redirects;
         S.Url := Moved;
         Note_Url (K);
         S.Redirecting := True;
      end if;
   end Redirect;

   --  The response's head.
   procedure Take_Head (K : Slot_Index) is
      S : Slot renames Slots (K);
      Status : constant Natural := R.Status (S.Reader.all);
   begin
      S.Report.Status := Status;
      if Status in 301 | 302 | 303 | 307 | 308 and then R.Location (S.Reader.all)'Length > 0
        and then Sessions.Next (S.Session.all) = Sessions.Advertisement
      then
         Redirect (K, R.Location (S.Reader.all));
      elsif Status /= OK_STATUS then
         Finish (K, Http_Status, "the server answered" & Natural'Image (Status));
      end if;
   end Take_Head;

   --  The response is whole: the session takes the next step; the next
   --  exchange gets a new connection.
   procedure Exchange_Done (K : Slot_Index) is
      S : Slot renames Slots (K);
   begin
      Sessions.Response_Ended (S.Session.all, S.Store.all);
      Note_Session (K);
      if Sessions.Next (S.Session.all) = Sessions.Finished then
         Session_Ended (K);
      else
         S.Advancing := True;
      end if;
   end Exchange_Done;

   function Response_Outcome (Why : R.Refusal) return Outcome is
     (case Why is
         when R.Body_Too_Large => Too_Large,
         when R.Truncated => Truncated,
         when others => Bad_Response);

   --  Count bytes of plaintext at the window's start.
   procedure Take_Bytes (K : Slot_Index; Count : Natural) is
      S : Slot renames Slots (K);
      Input : constant String (1 .. Count) with Import, Address => Window_Address (K);
      Next : Positive := 1;
      E : R.Event;
   begin
      while S.Ended_As = Running and then not S.Redirecting and then not S.Advancing loop
         R.Step (S.Reader.all, Input, Next, E);
         case E.Kind is
            when R.Head_Ready => Take_Head (K);
            when R.Body_Run =>
               if R.Status (S.Reader.all) = OK_STATUS then
                  declare
                     Run : constant Inflate_Streams.Byte_Array (1 .. E.Last - E.First + 1)
                       with Import, Address => Window_Address (K, E.First - 1);
                  begin
                     S.Report.Bytes := S.Report.Bytes + Unsigned_64 (Run'Length);
                     Sessions.Take (S.Session.all, S.Store.all, Run);
                     if Sessions.Next (S.Session.all) = Sessions.Finished then
                        Session_Ended (K);
                     end if;
                  end;
               end if;
            when R.Complete =>
               if Next <= Input'Last then
                  Finish (K, Bad_Response, "the response has bytes after its body");
               elsif R.Status (S.Reader.all) = OK_STATUS then
                  Exchange_Done (K);
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
            if S.Ended_As /= Running then
               if S.Channel_Open then
                  Submit_Shut (K);
               end if;
            elsif Label /= P.Reply_OK then
               Finish (K, (if Label = P.Reply_Error then Failure_Outcome (Word) else Connect_Failed),
                       "connecting to " & Fetch_Urls.Authority (S.Url) & ": " &
                       (if Label = P.Reply_Error then Failure_Name (Word) else "no answer"));
            else
               Send_Request (K);
            end if;
         when Write_Call =>
            if S.Ended_As /= Running then
               Submit_Shut (K);
            elsif Label /= P.Reply_OK or else Word = 0 or else Word > Unsigned_64 (S.Request_Piece) then
               Finish (K, Connect_Failed, "sending the request failed");
            else
               S.Request_Sent := S.Request_Sent + Natural (Word);
               if S.Request_Sent < S.Request'Length then
                  Write_Request (K);
               else
                  Read_More (K);
               end if;
            end if;
         when Read_Call =>
            if S.Ended_As /= Running then
               Submit_Shut (K);
            elsif Label = P.Reply_OK and then Word in 1 .. WINDOW_BYTES then
               Take_Bytes (K, Natural (Word));
               if S.Ended_As = Running then
                  if S.Redirecting or else S.Advancing then
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
                     Exchange_Done (K);
                  elsif E.Kind = R.Refused then
                     Finish (K, Response_Outcome (E.Why), "the response ended early: " & R.Name (E.Why));
                  else
                     Finish (K, Bad_Response, "the connection closed after a redirect's head");
                  end if;
                  Submit_Shut (K);
               end;
            else
               Finish (K, (if Label = P.Reply_Error then Failure_Outcome (Word) else Bad_Response),
                       "reading: " & (if Label = P.Reply_Error then Failure_Name (Word)
                                      elsif Label = P.Reply_OK then "tls.svc reported more than the window"
                                      else "no answer"));
            end if;
         when Shut_Call =>
            if S.Ended_As = Running and then (S.Redirecting or else S.Advancing) then
               S.Redirecting := False;
               S.Advancing := False;
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
      Free (S.Session);
      Free (S.Store);
      Free (S.Reader);
      Free (S.Request);
      S := (others => <>);
   end Release;

   procedure Start
     (Url, Root, Path, Directory : String; Ceiling : Limits; Now_Ms : Unsigned_64;
      Id : out Unsigned_64; Result : out Start_Result; Why : out Short_Text)
   is
      Parsed : Fetch_Urls.Url;
      Refused_As : Fetch_Urls.Refusal;
      K : Integer := -1;
      Parent : constant String := (if Path'Length = 0 then Root else Root & "/" & Path);
   begin
      Id := 0;
      Why := Short ("");
      if not CuBit.Handles.Valid (CCL_Native_Bindings.Tls) then
         Result := Not_Granted;
         Why := Short ("the program's manifest must request the TLS service (service ""tls"" ""tls"")");
         return;
      end if;
      Fetch_Urls.Parse (Url, Parsed, Refused_As);
      if Refused_As /= Fetch_Urls.No_Refusal or else not Fetch_Urls.Git_Repository (Parsed) then
         Result := Refused;
         Why := Short ("the URL cannot be cloned: " &
                       (if Refused_As /= Fetch_Urls.No_Refusal then Fetch_Urls.Name (Refused_As) else "a query") &
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
         Result := Refused;
         Why := Short ("this program already follows" & Natural'Image (MAXIMUM_CLONES) & " clones");
         return;
      end if;
      declare
         S : Slot renames Slots (Slot_Index (K));
      begin
         Next_Id := Next_Id + 1;
         S.Used := True;
         S.Id := Next_Id;
         S.Url := Parsed;
         S.Ceiling := Ceiling;
         S.Deadline := Now_Ms + Unsigned_64 (Ceiling.Deadline_Ms);
         S.Session := new Sessions.Session;
         S.Store := new Place_Store;
         S.Reader := new R.Reader;
         Note_Url (Slot_Index (K));
         Id := S.Id;
         Result := Started;
         --  The place first: a clone into a place it may not write never
         --  reaches the network.
         Sessions.Start
           (S.Session.all, S.Store.all, Parent, Directory, Fetch_Urls.Image (Parsed),
            (Maximum_Refs => Ceiling.Maximum_Refs, Maximum_Objects => Ceiling.Maximum_Objects,
             Maximum_Bytes => Inflate_Streams.Byte_Count
               (Unsigned_64'Min (Ceiling.Maximum_Bytes, Unsigned_64 (Inflate_Streams.Byte_Count'Last)))));
         if Sessions.Next (S.Session.all) = Sessions.Finished then
            Session_Ended (Slot_Index (K));
            return;
         end if;
         S.Window := Allocate_Zeroed (WINDOW_BYTES, PAGE_BYTES);
         if S.Window /= 0 then
            CuBit.Memory_Grants.Create_Via_Capability
              (CCL_Native_Bindings.Tls, Window_Address (Slot_Index (K)), WINDOW_PAGES, True, S.Loan, S.Lent);
         end if;
         if S.Window = 0 or else not S.Lent then
            Finish (Slot_Index (K), Connect_Failed, "no memory, or no loan, for the clone's window");
            return;
         end if;
         Open_Channel (Slot_Index (K));
      end;
   end Start;

   procedure Poll (Id : Unsigned_64; Item : out Report; Found : out Boolean) is
      K : constant Integer := Find (Id);
   begin
      Found := K >= 0;
      Item := (if Found then Slots (Slot_Index (K)).Report else (others => <>));
   end Poll;

   procedure Cancel (Id : Unsigned_64) is
      K : constant Integer := Find (Id);
   begin
      if K >= 0 then
         Finish (Slot_Index (K), Cancelled, "cancelled");
      end if;
   end Cancel;

   procedure Forget (Id : Unsigned_64) is
      K : constant Integer := Find (Id);
   begin
      --  A slot whose call is still out stays until its completion (the
      --  window is lent until then).
      if K >= 0 and then Slots (Slot_Index (K)).Ended_As /= Running then
         Slots (Slot_Index (K)).Forgotten := True;
         if Slots (Slot_Index (K)).Pending = No_Call and then not Slots (Slot_Index (K)).Channel_Open then
            Release (Slot_Index (K));
         end if;
      end if;
   end Forget;

   function Running return Boolean is
     (for some S of Slots => S.Used and then S.Ended_As = Running);

   procedure Pump (Now_Ms : Unsigned_64; Next_Due : out Unsigned_64) is
   begin
      Next_Due := Unsigned_64'Last;
      for K in Slot_Index loop
         if Slots (K).Used and then Slots (K).Ended_As = Running then
            if Now_Ms >= Slots (K).Deadline then
               Finish (K, Deadline, "the deadline passed");
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
end Git_Clone_Engine;
