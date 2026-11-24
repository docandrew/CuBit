------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hosted tests of net.fetch's codecs (SELF-003 step 1): Fetch_Urls and
--  Http_Responses, with the responses given whole, a byte at a time and
--  split at every offset, well-formed and adversarial. Every fixture is a
--  literal below.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Fetch_Urls;
with Http_Responses; use Http_Responses;

procedure Main is
   Failures : Natural := 0;
   Checks : Natural := 0;
   CRLF : constant String := ASCII.CR & ASCII.LF;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   ---------------------------------------------------------------------------
   --  URLs
   ---------------------------------------------------------------------------
   use type Fetch_Urls.Refusal;

   procedure Url_OK (Text, Host : String; Port : Natural; Target : String) is
      U : Fetch_Urls.Url;
      Why : Fetch_Urls.Refusal;
   begin
      Fetch_Urls.Parse (Text, U, Why);
      Check (Why = Fetch_Urls.No_Refusal and then U.Host (1 .. U.Host_Last) = Host and then U.Port = Port
             and then U.Target (1 .. U.Target_Last) = Target,
             "the URL " & Text & " (" & Why'Image & ")");
   end Url_OK;

   procedure Url_Refused (Text : String; Expected : Fetch_Urls.Refusal) is
      U : Fetch_Urls.Url;
      Why : Fetch_Urls.Refusal;
   begin
      Fetch_Urls.Parse (Text, U, Why);
      Check (Why = Expected, "the URL " & Text & " is refused as " & Expected'Image & " (got " & Why'Image & ")");
   end Url_Refused;

   procedure Resolves (Location, Expected : String) is
      Base, U : Fetch_Urls.Url;
      Why : Fetch_Urls.Refusal;
   begin
      Fetch_Urls.Parse ("https://github.com:8443/owner/repo/archive/v1.tar.gz", Base, Why);
      Fetch_Urls.Resolve (Base, Location, U, Why);
      Check (Why = Fetch_Urls.No_Refusal and then Fetch_Urls.Image (U) = Expected,
             "the Location " & Location & " (" & Why'Image & ")");
   end Resolves;

   procedure Resolve_Refused (Location : String; Expected : Fetch_Urls.Refusal) is
      Base, U : Fetch_Urls.Url;
      Why : Fetch_Urls.Refusal;
   begin
      Fetch_Urls.Parse ("https://github.com/x", Base, Why);
      Fetch_Urls.Resolve (Base, Location, U, Why);
      Check (Why = Expected, "the Location " & Location & " is refused as " & Expected'Image & " (got " &
             Why'Image & ")");
   end Resolve_Refused;

   ---------------------------------------------------------------------------
   --  Responses
   ---------------------------------------------------------------------------
   type Outcome is record
      Last : Event_Kind := Need_More;
      Why : Refusal := No_Refusal;
      Heads : Natural := 0;
      Status : Natural := 0;
      Body_Text : String (1 .. 4096) := [others => ' '];
      Body_Last : Natural := 0;
      Location_Text : String (1 .. 200) := [others => ' '];
      Location_Last : Natural := 0;
   end record;

   --  Feed Text in pieces of Piece bytes (0: whole), then the end of input
   --  when Ended; the events folded into an outcome.
   function Run (Text : String; Piece : Natural := 0; Ended : Boolean := True;
                 Ceiling : Body_Count := 1_000_000) return Outcome
   is
      R : Reader;
      O : Outcome;
      First : Positive := Text'First;
      E : Event;
   begin
      Start (R, Ceiling);
      while First <= Text'Last loop
         declare
            Last : constant Natural :=
              (if Piece = 0 then Text'Last else Natural'Min (Text'Last, First + Piece - 1));
            Input : constant String (1 .. Last - First + 1) := Text (First .. Last);
            Next : Positive := 1;
            Steps : Natural := 0;
         begin
            loop
               --  Every step takes a byte or ends: more steps than bytes is a hang.
               Steps := Steps + 1;
               if Steps > Input'Length + 2 then
                  O.Last := Refused;
                  O.Why := No_Refusal;
                  return O;
               end if;
               Step (R, Input, Next, E);
               case E.Kind is
                  when Head_Ready =>
                     O.Heads := O.Heads + 1;
                     O.Status := Status (R);
                     O.Location_Last := Location (R)'Length;
                     O.Location_Text (1 .. O.Location_Last) := Location (R);
                  when Body_Run =>
                     for C of Input (E.First .. E.Last) loop
                        O.Body_Last := O.Body_Last + 1;
                        O.Body_Text (O.Body_Last) := C;
                     end loop;
                  when Complete | Refused =>
                     O.Last := E.Kind;
                     O.Why := E.Why;
                  when Need_More =>
                     null;
               end case;
               exit when E.Kind in Refused | Need_More or else Next > Input'Last;
            end loop;
            if E.Kind = Refused then
               return O;
            end if;
            First := Last + 1;
         end;
      end loop;
      if Ended then
         Input_Ended (R, E);
         if O.Last /= Complete or else E.Kind = Refused then
            O.Last := E.Kind;
            O.Why := E.Why;
         end if;
      end if;
      return O;
   end Run;

   type Piece_List is array (1 .. 3) of Natural;
   --  The same response whole, a byte at a time, and in 7-byte pieces.
   procedure Response_OK (Text : String; Status, Expected_Body : String; What : String) is
   begin
      for Piece of Piece_List'[0, 1, 7] loop
         declare
            O : constant Outcome := Run (Text, Piece);
         begin
            Check (O.Last = Complete and then O.Heads = 1 and then Natural'Image (O.Status) = " " & Status
                   and then O.Body_Text (1 .. O.Body_Last) = Expected_Body,
                   What & " in pieces of" & Natural'Image (Piece) & " (" & O.Last'Image & " " & O.Why'Image & ")");
         end;
      end loop;
   end Response_OK;

   procedure Response_Refused (Text : String; Expected : Refusal; What : String; Ceiling : Body_Count := 1_000_000) is
   begin
      for Piece of Piece_List'[0, 1, 5] loop
         declare
            O : constant Outcome := Run (Text, Piece, Ceiling => Ceiling);
         begin
            Check (O.Last = Refused and then O.Why = Expected,
                   What & " is refused as " & Expected'Image & " in pieces of" & Natural'Image (Piece) &
                   " (got " & O.Last'Image & " " & O.Why'Image & ")");
         end;
      end loop;
   end Response_Refused;

   function Head (Status_Line : String; Fields : String := "") return String is
     (Status_Line & CRLF & Fields & CRLF);
   function Field (Name, Value : String) return String is (Name & ": " & Value & CRLF);
begin
   --  URLs that are accepted.
   Url_OK ("https://example.com/", "example.com", 443, "/");
   Url_OK ("https://example.com", "example.com", 443, "/");
   Url_OK ("HTTPS://Example.COM/A/b?c=D", "example.com", 443, "/A/b?c=D");
   Url_OK ("https://git.example.org:8443/x.tar?ref=v1#readme", "git.example.org", 8443, "/x.tar?ref=v1");
   Url_OK ("https://h.example?q", "h.example", 443, "/?q");
   Url_OK ("https://a.example/%7Euser/f%20g", "a.example", 443, "/%7Euser/f%20g");
   Url_OK ("https://a.example:1/", "a.example", 1, "/");
   Url_OK ("https://a.example:65535/", "a.example", 65535, "/");

   --  URLs that are refused.
   Url_Refused ("http://example.com/", Fetch_Urls.Not_Https);
   Url_Refused ("ftp://example.com/", Fetch_Urls.Not_Https);
   Url_Refused ("example.com/x", Fetch_Urls.Not_Https);
   Url_Refused ("https:/example.com", Fetch_Urls.Not_Https);
   Url_Refused ("https://user:pw@example.com/", Fetch_Urls.Has_Userinfo);
   Url_Refused ("https://example.com@evil.example/", Fetch_Urls.Has_Userinfo);
   Url_Refused ("https://[::1]/", Fetch_Urls.Bad_Host);
   Url_Refused ("https://10.0.2.2/", Fetch_Urls.Bad_Host);
   Url_Refused ("https:///x", Fetch_Urls.Bad_Host);
   Url_Refused ("https://-bad.example/", Fetch_Urls.Bad_Host);
   Url_Refused ("https://under_score.example/", Fetch_Urls.Bad_Host);
   Url_Refused ("https://" & [1 .. 65 => 'a'] & "/", Fetch_Urls.Bad_Host);
   Url_Refused ("https://a.example:0/", Fetch_Urls.Bad_Port);
   Url_Refused ("https://a.example:65536/", Fetch_Urls.Bad_Port);
   Url_Refused ("https://a.example:443443/", Fetch_Urls.Bad_Port);
   Url_Refused ("https://a.example:/", Fetch_Urls.Bad_Port);
   Url_Refused ("https://a.example:8x/", Fetch_Urls.Bad_Port);
   Url_Refused ("https://a.example/a b", Fetch_Urls.Bad_Target);
   Url_Refused ("https://a.example/a" & ASCII.CR & ASCII.LF & "Host: evil", Fetch_Urls.Bad_Target);
   Url_Refused ("https://a.example/""q""", Fetch_Urls.Bad_Target);
   Url_Refused ("https://a.example/a\b", Fetch_Urls.Bad_Target);
   Url_Refused ("https://a.example/%zz", Fetch_Urls.Bad_Target);
   Url_Refused ("https://a.example/%4", Fetch_Urls.Bad_Target);
   Url_Refused ("https://a.example/" & [1 .. 8_200 => 'x'], Fetch_Urls.Too_Long);

   --  Redirects.
   Resolves ("https://objects.example.net/blob?sig=1", "https://objects.example.net/blob?sig=1");
   Resolves ("//cdn.example.net/x", "https://cdn.example.net/x");
   Resolves ("/owner/repo/tar/v1", "https://github.com:8443/owner/repo/tar/v1");
   Resolve_Refused ("http://github.com/x", Fetch_Urls.Not_Https);
   Resolve_Refused ("javascript:alert(1)", Fetch_Urls.Not_Https);
   Resolve_Refused ("x/y", Fetch_Urls.Unsupported_Reference);
   Resolve_Refused ("../y", Fetch_Urls.Unsupported_Reference);
   Resolve_Refused ("", Fetch_Urls.Unsupported_Reference);

   --  The request and the authority.
   declare
      U : Fetch_Urls.Url;
      Why : Fetch_Urls.Refusal;
   begin
      Fetch_Urls.Parse ("https://Fetch-Test.cubit.internal:18495/files/a.tar", U, Why);
      Check (Fetch_Urls.Authority (U) = "fetch-test.cubit.internal:18495", "the authority for tls.svc");
      Check (Fetch_Urls.Request (U) =
               "GET /files/a.tar HTTP/1.1" & CRLF & "Host: fetch-test.cubit.internal:18495" & CRLF &
               "User-Agent: CuBit-net.fetch/1" & CRLF & "Accept: */*" & CRLF & "Accept-Encoding: identity" &
               CRLF & "Connection: close" & CRLF & CRLF, "the GET request");
      Fetch_Urls.Parse ("https://example.com/", U, Why);
      Check (Fetch_Urls.Authority (U) = "example.com:443", "the default port in the authority");
      Check (Fetch_Urls.Image (U) = "https://example.com/", "no :443 in the image");
      --  git's smart HTTP requests (gitprotocol-http), a trailing '/' dropped.
      Fetch_Urls.Parse ("https://git.example.com:8443/team/repo.git/", U, Why);
      Check (Fetch_Urls.Git_Repository (U), "a repository URL");
      Check (Fetch_Urls.Git_Request (U, Fetch_Urls.Info_Refs, 0) =
               "GET /team/repo.git/info/refs?service=git-upload-pack HTTP/1.1" & CRLF &
               "Host: git.example.com:8443" & CRLF & "User-Agent: CuBit-git/1" & CRLF &
               "Accept-Encoding: identity" & CRLF & "Git-Protocol: version=2" & CRLF & "Connection: close" & CRLF &
               "Accept: */*" & CRLF & CRLF, "git's advertisement request");
      Check (Fetch_Urls.Git_Request (U, Fetch_Urls.Upload_Pack, 1234) =
               "POST /team/repo.git/git-upload-pack HTTP/1.1" & CRLF &
               "Host: git.example.com:8443" & CRLF & "User-Agent: CuBit-git/1" & CRLF &
               "Accept-Encoding: identity" & CRLF & "Git-Protocol: version=2" & CRLF & "Connection: close" & CRLF &
               "Accept: application/x-git-upload-pack-result" & CRLF &
               "Content-Type: application/x-git-upload-pack-request" & CRLF & "Content-Length: 1234" & CRLF & CRLF,
             "git's upload-pack request");
      Fetch_Urls.Parse ("https://example.com/", U, Why);
      Check (Fetch_Urls.Git_Request (U, Fetch_Urls.Upload_Pack, 0)
               (1 .. 30) = "POST /git-upload-pack HTTP/1.1", "a repository at the root");
      Fetch_Urls.Parse ("https://example.com/r.git?x=1", U, Why);
      Check (not Fetch_Urls.Git_Repository (U), "a URL with a query is not a repository URL");
   end;

   --  Responses that complete.
   Response_OK (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "5") & Field ("X-Other", "a b")) & "hello",
                "200", "hello", "a length-delimited body");
   Response_OK (Head ("HTTP/1.1 200 OK", Field ("content-length", "  0  ")), "200", "", "an empty body");
   Response_OK (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "chunked")) &
                  "5;name=value" & CRLF & "hello" & CRLF & "A" & CRLF & "0123456789" & CRLF & "0" & CRLF &
                  "Trailer-Field: x" & CRLF & CRLF,
                "200", "hello0123456789", "a chunked body with an extension and a trailer");
   Response_OK (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "Chunked")) & "0" & CRLF & CRLF,
                "200", "", "an empty chunked body");
   Response_OK (Head ("HTTP/1.0 200 OK") & "until the close", "200", "until the close",
                "a body delimited by the close");
   Response_OK (Head ("HTTP/1.1 100 Continue") & Head ("HTTP/1.1 103 Early Hints", Field ("Link", "</a>")) &
                  Head ("HTTP/1.1 200 OK", Field ("Content-Length", "2")) & "ok",
                "200", "ok", "interim responses passed over");
   Response_OK (Head ("HTTP/1.1 204 No Content"), "204", "", "204 has no body");
   Response_OK (Head ("HTTP/1.1 200"), "200", "", "a status line without a reason");
   Response_OK (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "3") & Field ("Content-Length", "3")) & "abc",
                "200", "abc", "two equal lengths");
   declare
      O : constant Outcome :=
        Run (Head ("HTTP/1.1 302 Found", Field ("Location", "https://cdn.example.net/x") &
                   Field ("Content-Length", "4")) & "move");
   begin
      Check (O.Status = 302 and then O.Location_Text (1 .. O.Location_Last) = "https://cdn.example.net/x",
             "a redirect's Location");
   end;

   --  Responses that are refused.
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "2")) & "abc", Extra_Data,
                     "a byte past Content-Length");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "1") & Field ("Content-Length", "2")) & "a",
                     Conflicting_Lengths, "two different lengths");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "1,1")), Bad_Content_Length,
                     "a length list");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "-1")), Bad_Content_Length,
                     "a negative length");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "99999999999999999999")),
                     Bad_Content_Length, "a length beyond 2**62");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "3") & Field ("Transfer-Encoding", "chunked")),
                     Length_And_Chunked, "Content-Length with chunked");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "gzip, chunked")),
                     Unsupported_Transfer_Coding, "a gzip transfer coding");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "chunked") &
                           Field ("Transfer-Encoding", "chunked")),
                     Unsupported_Transfer_Coding, "chunked twice");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Content-Encoding", "gzip")), Unsupported_Content_Coding,
                     "a gzip content coding");
   Response_Refused (Head ("HTTP/1.1 101 Switching Protocols"), Switching_Protocols, "101");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("X-A", "b") & " folded" & CRLF), Folded_Field,
                     "obsolete line folding");
   Response_Refused (Head ("HTTP/1.1 200 OK", "Bad Name: x" & CRLF), Bad_Field, "a space in a field name");
   Response_Refused (Head ("HTTP/1.1 200 OK", ": x" & CRLF), Bad_Field, "an empty field name");
   Response_Refused (Head ("HTTP/1.1 200 OK", "NoColon" & CRLF), Bad_Field, "a field without a colon");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("X", "a" & ASCII.NUL & "b")), Bad_Field,
                     "a NUL in a value");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("X", "a" & ASCII.LF & "Injected: b")), Bad_Field,
                     "a bare LF in a value");
   Response_Refused (Head ("HTTP/2 200 OK"), Bad_Status_Line, "HTTP/2");
   Response_Refused (Head ("HTTP/1.1 20 OK"), Bad_Status_Line, "a two-digit status");
   Response_Refused (Head ("HTTP/1.1 099 Low"), Bad_Status_Line, "status 099");
   Response_Refused (Head ("HTTP/1.1 600 High"), Bad_Status_Line, "status 600");
   Response_Refused (Head ("HTTP/1.1 200OK"), Bad_Status_Line, "no space after the status");
   Response_Refused (Head ("ICY 200 OK"), Bad_Status_Line, "not HTTP");
   Response_Refused (Head ("HTTP/1.1 302 Found", Field ("Location", [1 .. 8_193 => 'a'])), Location_Too_Long,
                     "a Location beyond the ceiling");
   Response_Refused ("HTTP/1.1 200 OK" & CRLF & Field ("X", [1 .. 70_000 => 'a']), Head_Too_Large,
                     "a head beyond the ceiling");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "10")) & "short", Truncated,
                     "a length-delimited body cut short");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "chunked")) & "5" & CRLF & "hel",
                     Truncated, "a chunk cut short");
   Response_Refused ("HTTP/1.1 200 OK" & ASCII.LF & "Content-Length: 1" & ASCII.LF & ASCII.LF & "x",
                     Truncated, "bare LF line endings (the head never ends)");
   Response_Refused ("", Truncated, "no response at all");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "chunked")) & "x" & CRLF, Bad_Chunk,
                     "a chunk size that is not hex");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "chunked")) & "1000000000000000" & CRLF,
                     Bad_Chunk, "a chunk size beyond 2**60");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "chunked")) & "2" & CRLF & "abX" & CRLF,
                     Bad_Chunk, "chunk data longer than its size");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "chunked")) & "2" & ASCII.LF & "ab",
                     Bad_Chunk, "a chunk size line without CR");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "chunked")) & "0" & CRLF & CRLF & "x",
                     Extra_Data, "a byte after the last chunk");
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Content-Length", "10")) & "0123456789", Body_Too_Large,
                     "a body beyond the ceiling", Ceiling => 9);
   Response_Refused (Head ("HTTP/1.1 200 OK") & "0123456789", Body_Too_Large,
                     "a close-delimited body beyond the ceiling", Ceiling => 4);
   Response_Refused (Head ("HTTP/1.1 200 OK", Field ("Transfer-Encoding", "chunked")) & "a" & CRLF & "0123456789",
                     Body_Too_Large, "a chunked body beyond the ceiling", Ceiling => 9);

   --  A refusal is final.
   declare
      R : Reader;
      E : Event;
      Bad : constant String := "nonsense" & CRLF & CRLF;
      Good : constant String := Head ("HTTP/1.1 200 OK", Field ("Content-Length", "0"));
      Next : Positive := 1;
   begin
      Start (R, 100);
      Step (R, Bad, Next, E);
      Check (E.Kind = Refused and then E.Why = Bad_Status_Line, "the first refusal");
      Next := 1;
      Step (R, Good, Next, E);
      Check (E.Kind = Refused and then E.Why = Bad_Status_Line, "a later good head is refused too");
   end;

   if Failures = 0 then
      Put_Line ("fetch-codecs: PASS (" & Natural'Image (Checks) & " checks)");
   else
      Put_Line ("fetch-codecs: FAIL," & Natural'Image (Failures) & " of" & Natural'Image (Checks));
      raise Program_Error;
   end if;
end Main;
