--  Hosted tests of the Messages API client core (AGENT-001 slice 1):
--  Llm_Requests, Llm_Responses, Llm_Costs and Llm_Reports. Linux-hosted:
--  they exercise the same units llm.svc links, not llm.svc itself.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.Llm_Protocol;
with Llm_Costs;
with Llm_Requests;
with Llm_Responses;
with Llm_Reports;
with Llm_Json;
with Agent_Blocks;

procedure Llm_Main is
   package P renames CuBit.Llm_Protocol;
   package R renames Llm_Responses;
   use type P.Stop_Reason;
   use type P.Error_Kind;
   use type P.Problem;
   use ASCII;

   Failures : Natural := 0;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Count (Text, Pattern : String) return Natural is
     (Ada.Strings.Fixed.Count (Text, Pattern));

   Fake_Key : constant String := "sk-ant-test-0000";

   ---------------------------------------------------------------------------
   --  Responses.
   ---------------------------------------------------------------------------
   type Outcome is record
      Finished : Boolean := False;
      Started : Boolean := False;
      Stop : P.Stop_Reason := P.Disconnected;
      Error : P.Error_Kind := P.No_Error;
      Fault : P.Problem := P.No_Problem;
      Status : Natural := 0;
      Usage : Llm_Costs.Usage;
      Priced : Boolean := False;
      Cost : Unsigned_64 := 0;
      Model, Detail, Text : Unbounded_String;
      Text_Bytes : Unsigned_64 := 0;
      Text_After_Finish : Boolean := False;
   end record;

   Opus : constant Llm_Costs.Price := Llm_Costs.Price_Of ("claude-opus-5-5");

   function Limits_Of
     (Max_Tokens : P.Output_Tokens := 1_000; Ceiling : Unsigned_64 := 0;
      Bounded : Boolean := False; Price : Llm_Costs.Price := Opus) return R.Limits is
     ((Max_Tokens => Max_Tokens,
       Ceiling => (if Bounded then P.Nano_Usd else P.Unlimited),
       Ceiling_Nano_Usd => Ceiling, Price => Price, Capture => False));

   --  Feed Input; End_Of_Input when Close.
   function Run (Input : String; Limit : R.Limits := Limits_Of; Close : Boolean := True)
     return Outcome
   is
      D : R.Decoder := R.Start (Limit);
      O : Outcome;
      Out_Text : R.Output;
   begin
      for C of Input loop
         declare
            Was_Done : constant Boolean := R.Finished (D);
         begin
            R.Push (D, Character'Pos (C), Out_Text);
            for I in 1 .. Out_Text.Count loop
               Append (O.Text, Character'Val (Out_Text.Bytes (I)));
            end loop;
            if Was_Done and then Out_Text.Count > 0 then
               O.Text_After_Finish := True;
            end if;
         end;
      end loop;
      if Close then
         R.End_Of_Input (D);
      end if;
      O.Finished := R.Finished (D);
      O.Started := R.Started (D);
      O.Stop := R.Stop_Of (D);
      O.Error := R.Error_Of (D);
      O.Fault := R.Problem_Of (D);
      O.Status := R.Status_Of (D);
      O.Usage := R.Usage_Of (D);
      O.Priced := R.Cost_Known (D);
      O.Cost := R.Cost_Of (D);
      O.Model := To_Unbounded_String (R.Model_Of (D));
      O.Detail := To_Unbounded_String (R.Detail_Of (D));
      O.Text_Bytes := R.Text_Bytes_Of (D);
      return O;
   end Run;

   function Event (Name, Data : String) return String is
     ("event: " & Name & LF & "data: " & Data & LF & LF);

   Message_Start : constant String := Event
     ("message_start",
      "{""type"":""message_start"",""message"":{""id"":""msg_01"",""type"":""message""," &
      """role"":""assistant"",""model"":""claude-opus-5-5"",""content"":[]," &
      """stop_reason"":null,""stop_sequence"":null,""usage"":{""input_tokens"":25," &
      """cache_creation_input_tokens"":10,""cache_read_input_tokens"":5,""output_tokens"":1}}}");
   Ping : constant String := Event ("ping", "{""type"": ""ping""}");
   Thinking_Block : constant String :=
     Event ("content_block_start",
            "{""type"":""content_block_start"",""index"":0,""content_block"":" &
            "{""type"":""thinking"",""thinking"":"""",""signature"":""""}}") &
     Event ("content_block_delta",
            "{""type"":""content_block_delta"",""index"":0,""delta"":" &
            "{""type"":""thinking_delta"",""thinking"":""private""}}") &
     Event ("content_block_delta",
            "{""type"":""content_block_delta"",""index"":0,""delta"":" &
            "{""type"":""signature_delta"",""signature"":""EqQBCgIYAhIM""}}") &
     Event ("content_block_stop", "{""type"":""content_block_stop"",""index"":0}");
   Text_Block : constant String :=
     Event ("content_block_start",
            "{""type"":""content_block_start"",""index"":1,""content_block"":" &
            "{""type"":""text"",""text"":""""}}") &
     Event ("content_block_delta",
            "{""type"":""content_block_delta"",""index"":1,""delta"":" &
            "{""type"":""text_delta"",""text"":""Hello""}}") &
     Event ("content_block_delta",
            "{""type"":""content_block_delta"",""index"":1,""delta"":" &
            "{""type"":""text_delta"",""text"":"", \""world\""\n café 😀 naïve/\/ok""}}") &
     Event ("content_block_stop", "{""type"":""content_block_stop"",""index"":1}");
   Message_End : constant String :=
     Event ("message_delta",
            "{""type"":""message_delta"",""delta"":{""stop_reason"":""end_turn""," &
            """stop_sequence"":null},""usage"":{""output_tokens"":12}}") &
     Event ("message_stop", "{""type"":""message_stop""}");
   Sample_Body : constant String := Message_Start & Ping & Thinking_Block & Text_Block & Message_End;
   Expected_Text : constant String :=
     "Hello, ""world""" & LF & " caf" & Character'Val (16#C3#) & Character'Val (16#A9#) & " " &
     Character'Val (16#F0#) & Character'Val (16#9F#) & Character'Val (16#98#) & Character'Val (16#80#) &
     " na" & Character'Val (16#C3#) & Character'Val (16#AF#) & "ve//ok";
   --  25 input at 4000, 12 output at 20000, 10 cache writes at 5000, 5
   --  cache reads at 200 (nano-USD per token).
   Expected_Cost : constant := 25 * 4_000 + 12 * 20_000 + 10 * 5_000 + 5 * 200;

   CRLF : constant String := CR & LF;
   Event_Headers : constant String :=
     "content-type: text/event-stream; charset=utf-8" & CRLF &
     "request-id: req_011CTest" & CRLF;

   function Hex (Value : Natural) return String is
      Digits_Text : constant String := "0123456789abcdef";
      V : Natural := Value;
      Result : Unbounded_String;
   begin
      loop
         Result := Digits_Text (V mod 16 + 1) & Result;
         V := V / 16;
         exit when V = 0;
      end loop;
      return To_String (Result);
   end Hex;

   function Chunked (Payload : String; Split : Natural; Status_Line : String := "HTTP/1.1 200 OK";
                     Headers : String := Event_Headers) return String
   is
      First : constant String := Payload (Payload'First .. Payload'First + Split - 1);
      Second : constant String := Payload (Payload'First + Split .. Payload'Last);
      function Chunk (Part : String) return String is
        (if Part'Length = 0 then "" else Hex (Part'Length) & CRLF & Part & CRLF);
   begin
      return Status_Line & CRLF & Headers & "transfer-encoding: chunked" & CRLF & CRLF &
        Chunk (First) & Chunk (Second) & "0" & CRLF & CRLF;
   end Chunked;

   function Identity (Payload : String; Status_Line : String := "HTTP/1.1 200 OK";
                      Headers : String := Event_Headers) return String is
     (Status_Line & CRLF & Headers & "content-length:" & Natural'Image (Payload'Length) & CRLF & CRLF &
      Payload);

   function Close_Delimited (Payload : String) return String is
     ("HTTP/1.1 200 OK" & CRLF & Event_Headers & CRLF & Payload);

   function To_CRLF (Text : String) return String is
      Result : Unbounded_String;
   begin
      for C of Text loop
         if C = LF then Append (Result, CRLF); else Append (Result, C); end if;
      end loop;
      return To_String (Result);
   end To_CRLF;

   procedure Check_Sample (O : Outcome; What : String) is
   begin
      Check (O.Finished and then O.Stop = P.End_Turn,
             What & ": ends with end_turn (" & O.Stop'Image & " " & O.Fault'Image & ")");
      Check (To_String (O.Text) = Expected_Text, What & ": text deltas, unescaped, in order");
      Check (To_String (O.Model) = "claude-opus-5-5", What & ": model from message_start");
      Check (O.Usage.Input = 25 and then O.Usage.Output = 12 and then O.Usage.Cache_Write = 10
             and then O.Usage.Cache_Read = 5, What & ": usage");
      Check (O.Priced and then O.Cost = Expected_Cost, What & ": cost from usage");
      Check (O.Text_Bytes = Unsigned_64 (Expected_Text'Length), What & ": text byte count");
      Check (O.Status = 200, What & ": status");
   end Check_Sample;

   procedure Test_Event_Stream is
      Whole : constant Outcome := Run (Chunked (Sample_Body, Sample_Body'Length));
   begin
      Check_Sample (Whole, "whole stream");
      Check (Count (To_String (Whole.Text), "private") = 0, "thinking text is not delivered");
      --  Every byte boundary of the body as a chunk boundary.
      declare
         Same : Boolean := True;
      begin
         for Split in 0 .. Sample_Body'Length loop
            declare
               O : constant Outcome := Run (Chunked (Sample_Body, Split));
            begin
               if not (O.Stop = P.End_Turn and then To_String (O.Text) = Expected_Text
                       and then O.Cost = Expected_Cost)
               then
                  Same := False;
                  Put_Line ("  split at" & Split'Image & " differs");
               end if;
            end;
         end loop;
         Check (Same, "chunk split at every byte boundary decodes the same");
      end;
      Check_Sample (Run (Identity (Sample_Body)), "content-length framing");
      Check_Sample (Run (Close_Delimited (Sample_Body)), "close-delimited framing");
      Check_Sample (Run (Chunked (To_CRLF (Sample_Body), 17)), "CRLF event lines");
      --  Data split over two data: lines (joined with a line feed).
      declare
         Split_Data : constant String :=
           Message_Start &
           "event: content_block_delta" & LF & "data: {""type"":""content_block_delta""," & LF &
           "data: ""index"":1,""delta"":{""type"":""text_delta"",""text"":""Hi""}}" & LF & LF &
           "id: 7" & LF & ": a comment" & LF & Message_End;
         O : constant Outcome := Run (Chunked (Split_Data, 40));
      begin
         Check (O.Stop = P.End_Turn and then To_String (O.Text) = "Hi",
                "multi-line data, comments and other fields");
      end;
      --  Unknown events are parsed and ignored.
      declare
         O : constant Outcome := Run (Chunked
           (Message_Start & Event ("future_event", "{""type"":""future_event"",""x"":[1,2,{""y"":true}]}") &
            Text_Block & Message_End, 3));
      begin
         Check (O.Stop = P.End_Turn, "an unknown event is ignored");
      end;
   end Test_Event_Stream;

   procedure Test_Stop_Reasons is
      function Ending (Reason : String) return String is
        (Event ("message_delta",
                "{""type"":""message_delta"",""delta"":{""stop_reason"":""" & Reason & """}," &
                """usage"":{""output_tokens"":3}}") &
         Event ("message_stop", "{""type"":""message_stop""}"));
      procedure Expect (Reason : String; Stop : P.Stop_Reason) is
         O : constant Outcome := Run (Chunked (Message_Start & Ending (Reason), 10));
      begin
         Check (O.Stop = Stop, "stop_reason " & Reason);
      end Expect;
   begin
      Expect ("end_turn", P.End_Turn);
      Expect ("max_tokens", P.Max_Tokens);
      Expect ("stop_sequence", P.Stop_Sequence);
      Expect ("tool_use", P.Tool_Use);
      Expect ("pause_turn", P.Pause_Turn);
      Expect ("refusal", P.Refusal_Stop);
      Expect ("model_context_window_exceeded", P.Context_Window_Exceeded);
      Expect ("something_new", P.Unknown_Stop_Reason);
   end Test_Stop_Reasons;

   procedure Test_Errors is
      Overloaded_Event : constant String := Event
        ("error", "{""type"":""error"",""error"":{""type"":""overloaded_error"",""message"":""Overloaded""}}");
   begin
      declare
         O : constant Outcome := Run (Chunked (Message_Start & Text_Block & Overloaded_Event, 50));
      begin
         Check (O.Stop = P.Api_Error and then O.Error = P.Overloaded, "error event mid-stream");
         Check (To_String (O.Detail) = "Overloaded", "error message kept as detail");
         Check (To_String (O.Text) = Expected_Text (1 .. Expected_Text'Length),
                "text before the error was delivered");
      end;
      declare
         O : constant Outcome := Run (Chunked (Overloaded_Event, 5));
      begin
         Check (O.Stop = P.Api_Error and then O.Error = P.Overloaded, "error event before message_start");
      end;
      declare
         Doc : constant String :=
           "{""type"":""error"",""error"":{""type"":""authentication_error"",""message"":""invalid x-api-key""}}";
         O : constant Outcome := Run (Identity (Doc, "HTTP/1.1 401 Unauthorized",
                                                "content-type: application/json" & CRLF));
      begin
         Check (O.Stop = P.Api_Error and then O.Error = P.Authentication and then O.Status = 401,
                "401 with an error document");
         Check (To_String (O.Detail) = "invalid x-api-key", "401 detail");
      end;
      declare
         O : constant Outcome := Run (Identity ("<html>busy</html>", "HTTP/1.1 529 Site Overloaded",
                                                "content-type: text/html" & CRLF));
      begin
         Check (O.Stop = P.Api_Error and then O.Error = P.Overloaded, "529 with a non-JSON body");
      end;
      declare
         Doc : constant String :=
           "{""type"":""error"",""error"":{""type"":""rate_limit_error"",""message"":""slow down""}}";
         O : constant Outcome := Run (Chunked (Doc, 9, "HTTP/1.1 429 Too Many Requests",
                                               "content-type: application/json" & CRLF));
      begin
         Check (O.Stop = P.Api_Error and then O.Error = P.Rate_Limited, "429 chunked error document");
      end;
   end Test_Errors;

   procedure Expect_Problem (Input : String; Why : P.Problem; What : String; Limit : R.Limits := Limits_Of) is
      O : constant Outcome := Run (Input, Limit);
   begin
      Check (O.Finished and then O.Stop = P.Malformed_Response and then O.Fault = Why,
             What & " -> " & Why'Image & " (got " & O.Stop'Image & " " & O.Fault'Image & ")");
      Check (not O.Text_After_Finish, What & ": no text after the stop");
   end Expect_Problem;

   procedure Test_Malformed is
      Long : constant String (1 .. 100) := [others => 'm'];
      Deep : constant String (1 .. 40) := [others => '['];
   begin
      Expect_Problem (Chunked (Event ("message_start",
        "{""type"":""message_start"",""message"":{""model"":""" & Long & """}}"), 3),
        P.Field_Too_Long, "oversized model");
      Expect_Problem (Chunked (Message_Start & Event ("message_delta",
        "{""type"":""message_delta"",""delta"":{""stop_reason"":""" & Long & """}}"), 3),
        P.Field_Too_Long, "oversized stop_reason");
      Expect_Problem (Chunked (Event ("message_start", "{""type"":""" & Long & """}"), 3),
        P.Field_Too_Long, "oversized type");
      Expect_Problem (Chunked (Event ("message_start", "{""type"":""message_start"""), 3),
        P.Truncated_Event, "unterminated object");
      Expect_Problem (Chunked (Event ("ping", "{""type"":""ping""}}"), 3),
        P.Json_Syntax, "trailing garbage");
      Expect_Problem (Chunked (Event ("ping", "{""type"":""pi\qng""}"), 3),
        P.Json_Syntax, "bad escape");
      Expect_Problem (Chunked (Event ("ping", "{""type"":""pi" & HT & "ng""}"), 3),
        P.Json_Syntax, "raw control character in a string");
      Expect_Problem (Chunked (Event ("ping", "{""type"":""ping"",""x"":01}"), 3),
        P.Json_Syntax, "leading zero");
      Expect_Problem (Chunked (Event ("ping", "{""type"":""ping"",""x"":tru}"), 3),
        P.Json_Syntax, "bad literal");
      Expect_Problem (Chunked (Event ("ping", "{""type"":""ping"",""x"":" & Deep & "}"), 3),
        P.Json_Too_Deep, "nesting deeper than the ceiling");
      Expect_Problem (Chunked (Event ("message_start",
        "{""type"":""message_start"",""message"":{""usage"":{""input_tokens"":99999999999999999999}}}"), 3),
        P.Number_Overflow, "usage overflow");
      Expect_Problem (Chunked (Event ("message_start",
        "{""type"":""message_start"",""message"":{""usage"":{""input_tokens"":""25""}}}"), 3),
        P.Type_Mismatch, "usage as a string");
      Expect_Problem (Chunked (Event ("message_start",
        "{""type"":""message_start"",""message"":{""usage"":{""input_tokens"":2.5}}}"), 3),
        P.Type_Mismatch, "usage as a fraction");
      Expect_Problem (Chunked (Event ("message_start", "{""type"":""message_stop""}"), 3),
        P.Type_Mismatch, "event name and type differ");
      Expect_Problem (Chunked ("data: {""type"":""ping""}" & LF & LF, 3),
        P.Data_Without_Event, "data without an event name");
      Expect_Problem (Chunked (Text_Block, 3), P.Out_Of_Order, "content before message_start");
      Expect_Problem (Chunked (Message_Start & Message_Start, 3), P.Out_Of_Order, "two message_starts");
      Expect_Problem (Chunked (Message_Start & Event ("message_stop", "{""type"":""message_stop""}"), 3),
        P.Missing_Stop_Reason, "message_stop without a stop_reason");
      Expect_Problem ("HTTX/1.1 200 OK" & CRLF, P.Bad_Status_Line, "bad status line");
      Expect_Problem ("HTTP/1.1 20 OK" & CRLF, P.Bad_Status_Line, "short status code");
      Expect_Problem ("HTTP/1.1 200 OK" & CRLF & "content-type: application/json" & CRLF & CRLF & "{}",
        P.Bad_Content_Type, "200 that is not an event stream");
      Expect_Problem ("HTTP/1.1 200 OK" & CRLF & Event_Headers & "transfer-encoding: chunked" & CRLF & CRLF &
        "zz" & CRLF, P.Bad_Chunk, "bad chunk size");
      Expect_Problem ("HTTP/1.1 200 OK" & CRLF & Event_Headers & "transfer-encoding: gzip" & CRLF & CRLF,
        P.Bad_Header, "unsupported transfer coding");
      Expect_Problem ("HTTP/1.1 200 OK" & CRLF & "bad header line" & CRLF & CRLF,
        P.Bad_Header, "header without a colon");
      declare
         Huge_Header : constant String (1 .. R.Maximum_Header_Section) := [others => 'a'];
      begin
         Expect_Problem ("HTTP/1.1 200 OK" & CRLF & "x: " & Huge_Header & CRLF & CRLF,
           P.Headers_Too_Large, "header section over its ceiling");
      end;
      --  A long string the client does not keep is fine.
      declare
         Long_Signature : constant String (1 .. 100_000) := [others => 'S'];
         O : constant Outcome := Run (Chunked
           (Message_Start & Event ("content_block_delta",
              "{""type"":""content_block_delta"",""index"":0,""delta"":{""type"":""signature_delta""," &
              """signature"":""" & Long_Signature & """}}") & Message_End, 11));
      begin
         Check (O.Stop = P.End_Turn, "a long field the client does not keep is not an error");
      end;
   end Test_Malformed;

   procedure Test_Disconnect is
      Full : constant String := Chunked (Sample_Body, 20);
   begin
      declare
         O : constant Outcome := Run (Full (Full'First .. Full'First + Full'Length / 2));
      begin
         Check (O.Stop = P.Disconnected, "connection closed mid-stream");
         Check (O.Started, "it had started");
      end;
      Check (Run ("").Stop = P.Disconnected, "connection closed before any byte");
      Check (Run ("HTTP/1.1 200 OK" & CRLF).Stop = P.Disconnected, "closed during the headers");
      declare
         O : constant Outcome := Run (Close_Delimited (Message_Start & Text_Block));
      begin
         Check (O.Stop = P.Disconnected and then To_String (O.Text) = Expected_Text,
                "close-delimited stream cut before message_stop");
      end;
      declare
         O : constant Outcome := Run (Full, Close => False);
      begin
         Check (O.Finished, "a complete stream is finished before the close");
      end;
   end Test_Disconnect;

   procedure Test_Budgets is
   begin
      --  Worst case at message_start: 25*4000 + 1000*20000 + 10*5000 + 5*200.
      declare
         Worst : constant := 25 * 4_000 + 1_000 * 20_000 + 10 * 5_000 + 5 * 200;
         Over : constant Outcome := Run (Chunked (Sample_Body, 30),
                                         Limits_Of (Ceiling => Worst - 1, Bounded => True));
         Under : constant Outcome := Run (Chunked (Sample_Body, 30),
                                          Limits_Of (Ceiling => Worst, Bounded => True));
      begin
         Check (Over.Stop = P.Cost_Budget, "cost ceiling below the worst case stops at message_start");
         Check (Length (Over.Text) = 0, "no text after a cost stop");
         Check (Over.Cost <= Worst - 1, "what it cost stays under the ceiling");
         Check (Under.Stop = P.End_Turn, "a ceiling that covers the worst case completes");
      end;
      declare
         Unpriced : constant Outcome := Run (Chunked (Sample_Body, 30),
           Limits_Of (Ceiling => Unsigned_64'Last, Bounded => True, Price => Llm_Costs.Price_Of ("x")));
      begin
         Check (Unpriced.Stop = P.Cost_Budget, "a ceiling without a price stops");
         Check (not Llm_Costs.Price_Of ("claude-opus-5").Known, "no prefix matching of prices");
      end;
      declare
         Unlimited : constant Outcome := Run (Chunked (Sample_Body, 30),
           Limits_Of (Price => Llm_Costs.Price_Of ("claude-unknown")));
      begin
         Check (Unlimited.Stop = P.End_Turn and then not Unlimited.Priced, "unpriced, unlimited completes");
      end;
      --  The response ceiling: overhead plus Response_Bytes_Per_Token per token.
      declare
         Padding : constant String (1 .. R.Response_Overhead_Bytes + 2 * R.Response_Bytes_Per_Token) :=
           [others => ' '];
         O : constant Outcome := Run (Chunked (": " & Padding & LF & Sample_Body, 7),
                                      Limits_Of (Max_Tokens => 1));
      begin
         Check (O.Stop = P.Malformed_Response and then O.Fault = P.Response_Too_Large,
                "response over its ceiling");
      end;
      --  The caller's stops.
      declare
         D : R.Decoder := R.Start (Limits_Of);
         Out_Text : R.Output;
      begin
         for C of Close_Delimited (Message_Start) loop
            R.Push (D, Character'Pos (C), Out_Text);
         end loop;
         R.Stop_With (D, P.Deadline);
         Check (R.Finished (D) and then R.Stop_Of (D) = P.Deadline, "deadline stop");
         R.Push (D, Character'Pos ('x'), Out_Text);
         R.End_Of_Input (D);
         Check (R.Stop_Of (D) = P.Deadline and then Out_Text.Count = 0, "nothing after a stop");
      end;
   end Test_Budgets;

   ---------------------------------------------------------------------------
   --  Requests.
   ---------------------------------------------------------------------------
   procedure Test_Requests is
      package Q renames Llm_Requests;
      package B renames Agent_Blocks;
      function Body_Of
        (Model : String; Max : P.Output_Tokens; System, Tools, Conversation : String; Cache : Boolean := False)
        return String
      is
         Into : String (1 .. Q.Body_Length (Model, Max, System, Tools, Conversation, Cache));
      begin
         Q.Write_Body (Model, Max, System, Tools, Conversation, Cache, Into);
         return Into;
      end Body_Of;
      function Header_Of (Host, Key : String; Length : Natural) return String is
         Into : String (1 .. Q.Header_Length (Host, Key, Length));
      begin
         Q.Write_Header (Host, Key, Length, Into);
         return Into;
      end Header_Of;
      function Tool_Entry (Name, Description, Schema : String) return String is
         function Byte (Value : Natural) return Character is (Character'Val (Value mod 256));
      begin
         return Byte (Name'Length) & Byte (Name'Length / 256) & Byte (Description'Length) &
           Byte (Description'Length / 256) & Byte (Schema'Length) & Byte (Schema'Length / 256) &
           Byte (Schema'Length / 65_536) & Byte (0) & Name & Description & Schema;
      end Tool_Entry;
      function User_Text (Text : String) return String is (B.Encode (P.User, P.Text_Block, Text));
      Hello : constant String := User_Text ("hello");
      Plain : constant String := Body_Of ("claude-opus-5-5", 1024, "", "", Hello);
      Escaped : constant String := Body_Of
        ("claude-opus-5-5", 64000, "Be brief.", "",
         User_Text ("say ""hi""\ now" & LF & HT & Character'Val (1) & Character'Val (16#7F#) &
                    Character'Val (16#C3#) & Character'Val (16#A9#)));
      Schema : constant String :=
        "{""type"":""object"",""properties"":{""service"":{""type"":""string""}},""required"":[""service""]}";
      Tools : constant String := Tool_Entry ("logs_recent", "Recent log records", Schema) &
        Tool_Entry ("files_list", "A directory", "{""type"":""object"",""properties"":{}}");
      --  A conversation that grows turn by turn, as the agent's does.
      Turn_1 : constant String := User_Text ("diagnose");
      Turn_2 : constant String := Turn_1 &
        B.Encode (P.Assistant, P.Thinking_Block, "", "c2ln") &
        B.Encode (P.Assistant, P.Text_Block, "Checking.") &
        B.Encode (P.Assistant, P.Tool_Call_Block, "toolu_1", "logs_recent", "{""service"":""acpi""}") &
        B.Encode (P.User, P.Tool_Result_Block, "toolu_1", "[{""message"":""ok""}]") &
        B.Encode (P.User, P.Tool_Result_Block, "toolu_2", "refused", Failed => True);
      Turn_3 : constant String := Turn_2 &
        B.Encode (P.Assistant, P.Text_Block, "Done?") & User_Text ("continue");
      System : constant String := "You are a diagnostic agent.";
      Body_1 : constant String := Body_Of ("claude-sonnet-5-5", 4096, System, Tools, Turn_1, Cache => True);
      Body_2 : constant String := Body_Of ("claude-sonnet-5-5", 2048, System, Tools, Turn_2, Cache => True);
      Body_3 : constant String := Body_Of ("claude-sonnet-5-5", 1024, System, Tools, Turn_3, Cache => True);
      Mark : constant String := ",""cache_control"":{""type"":""ephemeral""}";
      function Unmarked (Text : String) return String is
         At_Mark : constant Natural := Ada.Strings.Fixed.Index (Text, Mark);
      begin
         return (if At_Mark = 0 then Text
                 else Text (Text'First .. At_Mark - 1) & Unmarked (Text (At_Mark + Mark'Length .. Text'Last)));
      end Unmarked;
      --  A body without its breakpoints, up to the end of its last block.
      function Prefix (Text : String) return String is
         Plain_Text : constant String := Unmarked (Text);
         Cut : constant Natural := Ada.Strings.Fixed.Index (Plain_Text, "]}],""max_tokens""");
      begin
         return Plain_Text (Plain_Text'First .. Cut - 1);
      end Prefix;
      Header : constant String := Header_Of ("api.anthropic.com", Fake_Key, Plain'Length);
   begin
      Check (Plain = "{""model"":""claude-opus-5-5"",""stream"":true,""messages"":[{""role"":""user""," &
                     """content"":[{""type"":""text"",""text"":""hello""}]}],""max_tokens"":1024}", "plain body: " & Plain);
      Check (Escaped = "{""model"":""claude-opus-5-5"",""stream"":true,""system"":""Be brief.""," &
               """messages"":[{""role"":""user"",""content"":[{""type"":""text"",""text"":" &
               """say \""hi\""\\ now\n\t\u0001\u007f" & Character'Val (16#C3#) & Character'Val (16#A9#) &
               """}]}],""max_tokens"":64000}", "escaped body: " & Escaped);
      Check (Count (Body_2, "{""role"":""assistant"",""content"":[{""type"":""thinking"",""thinking"":""""," &
                    """signature"":""c2ln""},{""type"":""text"",""text"":""Checking.""},{""type"":""tool_use""," &
                    """id"":""toolu_1"",""name"":""logs_recent"",""input"":{""service"":""acpi""}}]}") = 1,
             "an assistant turn: thinking, text and a tool call in one message: " & Body_2);
      Check (Count (Body_2, "{""role"":""user"",""content"":[{""type"":""tool_result"",""tool_use_id"":""toolu_1""," &
                    """content"":""[{\""message\"":\""ok\""}]""},{""type"":""tool_result"",""tool_use_id"":" &
                    """toolu_2"",""content"":""refused"",""is_error"":true") = 1,
             "tool results in one user message, errors flagged");
      Check (Count (Body_1, """tools"":[{""name"":""logs_recent"",""description"":""Recent log records""," &
                    """input_schema"":" & Schema & "},{""name"":""files_list""") = 1, "tool definitions");
      Check (Count (Body_1, Mark) = 3 and then Count (Body_2, Mark) = 3 and then Count (Body_3, Mark) = 3,
             "three breakpoints: the last tool, the system text, the last block");
      Check (Count (Body_1, """input_schema"":{""type"":""object"",""properties"":{}}" & Mark & "}]") = 1,
             "a breakpoint on the last tool");
      Check (Count (Body_1, """system"":[{""type"":""text"",""text"":""" & System & """" & Mark & "}]") = 1,
             "a breakpoint on the system text");
      Check (Count (Body_3, """text"":""continue""" & Mark & "}]}],""max_tokens"":1024}") = 1,
             "a breakpoint on the last block, max_tokens after it");
      Check (Prefix (Body_2) (1 .. Prefix (Body_1)'Length) = Prefix (Body_1)
             and then Prefix (Body_3) (1 .. Prefix (Body_2)'Length) = Prefix (Body_2),
             "each turn's request begins with the last one's bytes (the cache hits)");
      Check (Q.Valid_Conversation (Turn_3) and then Q.Valid_Tools (Tools) and then Q.Valid_Tools (""),
             "valid sections");
      Check (not Q.Valid_Conversation ("") and then not Q.Valid_Conversation (Turn_2 (1 .. Turn_2'Length - 1))
             and then not Q.Valid_Conversation (Turn_1 & B.Encode (P.Assistant, P.Text_Block, "x"))
             and then not Q.Valid_Conversation (B.Encode (P.Assistant, P.Text_Block, "x") & Turn_1)
             and then not Q.Valid_Conversation (User_Text (""))
             and then not Q.Valid_Conversation (B.Encode (P.User, P.Tool_Call_Block, "t", "n", "{}"))
             and then not Q.Valid_Conversation
               (Turn_1 & B.Encode (P.Assistant, P.Tool_Call_Block, "t", "n", "{}],""max_tokens"":99999,""x"":{") &
                User_Text ("y"))
             and then not Q.Valid_Conversation
               (Turn_1 & B.Encode (P.Assistant, P.Tool_Call_Block, "t""", "n", "{}") & User_Text ("y"))
             and then not Q.Valid_Conversation
               (Turn_1 & B.Encode (P.Assistant, P.Tool_Call_Block, "t", "bad name", "{}") & User_Text ("y")),
             "malformed conversations: truncated, ending with the assistant, empty text, misplaced or " &
             "hostile tool calls");
      Check (not Q.Valid_Tools (Tool_Entry ("x", "", "{}],""max_tokens"":1"))
             and then not Q.Valid_Tools (Tool_Entry ("a.b", "", "{}"))
             and then not Q.Valid_Tools (Tool_Entry ("x", "", "[]")) and then not Q.Valid_Tools ("abc"),
             "malformed tools: a schema that is not one object, a bad name, a short entry");
      Check (Llm_Json.Valid_Object ("{""a"":[1,2.5e3,true,null,""é""],""b"":{}}") and then
             not Llm_Json.Valid_Object ("{""a"":1}}") and then not Llm_Json.Valid_Object ("{""a"" 1}") and then
             not Llm_Json.Valid_Object ("{""a"":01}") and then not Llm_Json.Valid_Object ("[1]") and then
             not Llm_Json.Valid_Object ("{""a"":""" & LF & """}") and then
             not Llm_Json.Valid_Object ((1 .. 70 => '{')) and then Llm_Json.Valid_Value (" 42 "),
             "JSON checks");
      Check (Q.Escaped_Length ("a""b") = 4 and then Q.Escape ("a""b" & LF) = "a\""b\n", "escaping");
      Check (Header (Header'First .. Header'First + 29) = "POST /v1/messages HTTP/1.1" & CRLF & "ho",
             "request line");
      Check (Count (Header, "x-api-key: " & Fake_Key & CRLF) = 1, "the key is in one x-api-key line");
      Check (Count (Header, Fake_Key) = 1, "the key appears once in the header");
      Check (Count (Header, "anthropic-version: 2026-10-21" & CRLF) = 1, "anthropic-version");
      Check (Count (Header, "content-type: application/json" & CRLF) = 1, "content-type");
      Check (Count (Header, "content-length:" & Natural'Image (Plain'Length) & CRLF) = 1, "content-length");
      Check (Count (Header, "host: api.anthropic.com" & CRLF) = 1, "host");
      Check (Header (Header'Last - 3 .. Header'Last) = CRLF & CRLF, "header ends with a blank line");
      Check (Count (Plain & Escaped & Body_3, Fake_Key) = 0, "the key is never in a body");
      Check (not Q.Valid_Key ("sk-ant" & CR & LF & "x-evil: 1"), "a key cannot carry a header line");
      Check (not Q.Valid_Key ("sk ant"), "a key cannot carry a space");
      Check (not Q.Valid_Key (""), "an empty key is refused");
      Check (Q.Valid_Key (Fake_Key), "a key token");
      Check (Q.Valid_Model ("claude-opus-5-5") and then not Q.Valid_Model ("Claude Opus")
             and then not Q.Valid_Model ("a""b"), "model identifiers");
      Check (Q.Default_Model'Length = 15 and then Q.Default_Model (1 .. 6) = "claude", "the default model");
   end Test_Requests;

   ---------------------------------------------------------------------------
   --  Captured content: a turn's blocks, for the agent's conversation.
   ---------------------------------------------------------------------------
   function Capture (Input : String; Limit : R.Limits) return String is
      D : R.Decoder := R.Start (Limit);
      Out_Text : R.Output;
      Result : Unbounded_String;
   begin
      for C of Input loop
         R.Push (D, Character'Pos (C), Out_Text);
      end loop;
      R.End_Of_Input (D);
      for I in 1 .. R.Content_Length (D) loop
         Append (Result, Character'Val (R.Content_Byte (D, I)));
      end loop;
      return To_String (Result);
   end Capture;

   procedure Test_Capture is
      package B renames Agent_Blocks;
      Capturing : constant R.Limits := (Limits_Of with delta Capture => True);
      function Block_Start (Index : Natural; Block : String) return String is
        (Event ("content_block_start", "{""type"":""content_block_start"",""index"":" & Index'Image &
                ",""content_block"":" & Block & "}"));
      function Block_Delta (Index : Natural; Item : String) return String is
        (Event ("content_block_delta", "{""type"":""content_block_delta"",""index"":" & Index'Image &
                ",""delta"":" & Item & "}"));
      function Block_Stop (Index : Natural) return String is
        (Event ("content_block_stop", "{""type"":""content_block_stop"",""index"":" & Index'Image & "}"));
      Start_Event : constant String :=
        Event ("message_start", "{""type"":""message_start"",""message"":{""model"":""claude-sonnet-5-5""," &
               """usage"":{""input_tokens"":10,""output_tokens"":1}}}");
      function Ending (Reason : String) return String is
        (Event ("message_delta", "{""type"":""message_delta"",""delta"":{""stop_reason"":""" & Reason &
                """},""usage"":{""output_tokens"":9}}") & Event ("message_stop", "{""type"":""message_stop""}"));
      Turn : constant String := Start_Event &
        Block_Start (0, "{""type"":""thinking"",""thinking"":"""",""signature"":""""}") &
        Block_Delta (0, "{""type"":""signature_delta"",""signature"":""c2ln""}") & Block_Stop (0) &
        Block_Start (1, "{""type"":""text"",""text"":""""}") &
        Block_Delta (1, "{""type"":""text_delta"",""text"":""Look""}") &
        Block_Delta (1, "{""type"":""text_delta"",""text"":""ing.""}") & Block_Stop (1) &
        Block_Start (2, "{""type"":""server_tool_use"",""id"":""srv_1"",""name"":""web_search"",""input"":{}}") &
        Block_Stop (2) &
        Block_Start (3, "{""type"":""tool_use"",""id"":""toolu_9"",""name"":""logs_recent"",""input"":{}}") &
        Block_Delta (3, "{""type"":""input_json_delta"",""partial_json"":""{\""serv""}") &
        Block_Delta (3, "{""type"":""input_json_delta"",""partial_json"":""ice\"":\""acpi\""}""}") &
        Block_Stop (3) &
        Block_Start (4, "{""type"":""tool_use"",""id"":""toolu_10"",""name"":""files_list"",""input"":{}}") &
        Block_Stop (4) &
        Block_Start (5, "{""type"":""text"",""text"":""""}") & Block_Stop (5) &
        Ending ("tool_use");
      Expected : constant String :=
        B.Encode (P.Assistant, P.Thinking_Block, "", "c2ln") & B.Encode (P.Assistant, P.Text_Block, "Looking.") &
        B.Encode (P.Assistant, P.Tool_Call_Block, "toolu_9", "logs_recent", "{""service"":""acpi""}") &
        B.Encode (P.Assistant, P.Tool_Call_Block, "toolu_10", "files_list", "{}");
      Captured : constant String := Capture (Chunked (Turn, 37), Capturing);
      Plain_Run : constant Outcome := Run (Chunked (Turn, 37), Capturing);
      Uncaptured : constant String := Capture (Chunked (Turn, 37), Limits_Of);
   begin
      Check (Captured = Expected, "a turn's blocks: thinking with its signature, text, tool calls with " &
               "their input; an unknown block and empty text dropped");
      Check (Plain_Run.Stop = P.Tool_Use and then To_String (Plain_Run.Text) = "Looking.",
             "the text still streams while it is captured");
      Check (Uncaptured = "", "nothing is kept unless capture is asked for");
      Expect_Problem
        (Chunked (Start_Event & Block_Start (0, "{""type"":""tool_use"",""name"":""x"",""input"":{}}") & Block_Stop (0) &
                  Ending ("tool_use"), 64),
         P.Bad_Content_Block, "a tool call without an id", Capturing);
      Expect_Problem
        (Chunked (Start_Event & Block_Start (0, "{""type"":""tool_use"",""id"":""a b"",""name"":""x"",""input"":{}}") &
                  Block_Stop (0) & Ending ("tool_use"), 64),
         P.Bad_Content_Block, "a tool call id that is not an identifier", Capturing);
      Expect_Problem
        (Chunked (Start_Event & Block_Start (0, "{""type"":""thinking"",""thinking"":"""",""signature"":""""}") &
                  Block_Delta (0, "{""type"":""signature_delta"",""signature"":""c2ln""}") &
                  Block_Delta (0, "{""type"":""thinking_delta"",""thinking"":""late""}") & Block_Stop (0) &
                  Ending ("end_turn"), 64),
         P.Bad_Content_Block, "reasoning after its signature", Capturing);
      declare
         Huge : constant String := (1 .. 70_000 => 'x');
      begin
         Expect_Problem
           (Chunked (Start_Event & Block_Start (0, "{""type"":""text"",""text"":""""}") &
                     Block_Delta (0, "{""type"":""text_delta"",""text"":""" & Huge & """}") &
                     Block_Delta (0, "{""type"":""text_delta"",""text"":""" & Huge & """}") & Block_Stop (0) &
                     Ending ("end_turn"), 4096),
            P.Content_Too_Large, "content beyond its capacity (sized from a small max_tokens)",
            (Capturing with delta Max_Tokens => 1));
         --  LIM-001: the capacity comes from the request (max_tokens), not a
         --  fixed 128 KiB; the same 140 KB turn is captured whole.
         declare
            Big_Turn : constant String :=
              Start_Event & Block_Start (0, "{""type"":""text"",""text"":""""}") &
              Block_Delta (0, "{""type"":""text_delta"",""text"":""" & Huge & """}") &
              Block_Delta (0, "{""type"":""text_delta"",""text"":""" & Huge & """}") & Block_Stop (0) &
              Ending ("end_turn");
            Large : constant R.Limits := (Capturing with delta Max_Tokens => 128_000);
            Whole : constant String := Capture (Chunked (Big_Turn, 4096), Large);
         begin
            Check (R.Capacity_For (Large) > 131_072 and then R.Capacity_For (Large) <= P.Maximum_Content_Bytes,
                   "the capture's capacity is sized from max_tokens, under the named ceiling");
            Check (Whole = B.Encode (P.Assistant, P.Text_Block, Huge & Huge),
                   "a 140 KB turn (past the old fixed 128 KiB) is captured whole");
            Check (R.Capacity_For (Limits_Of) = 0, "no capture, no content area");
         end;
      end;
   end Test_Capture;

   procedure Test_Prices is
      Haiku : constant Llm_Costs.Price := Llm_Costs.Price_Of ("claude-haiku-5-5");
      Sonnet : constant Llm_Costs.Price := Llm_Costs.Price_Of ("claude-sonnet-5-5");
      Short : constant Llm_Costs.Usage := (Input => 1_000, Output => 100, Cache_Write => 0, Cache_Read => 0);
      Long : constant Llm_Costs.Usage := (Input => 90_000, Output => 100, Cache_Write => 0, Cache_Read => 20_000);
   begin
      Check (Haiku.Known and then Sonnet.Known and then Opus.Known, "three priced models");
      Check (Llm_Costs.Cost (Sonnet, Short) = 1_000 * 2_000 + 100 * 10_000, "Sonnet 5.5: USD 2 / 10");
      Check (Llm_Costs.Cost (Sonnet, (Short with delta Cache_Read => 1_000)) =
               1_000 * 2_000 + 100 * 10_000 + 1_000 * 200, "Sonnet 5.5 cache reads: USD 0.20");
      Check (Llm_Costs.Cost (Haiku, Short) = 1_000 * 100 + 100 * 500, "Haiku 5.5 up to 100K: USD 0.10 / 0.50");
      Check (Llm_Costs.Cost (Haiku, Long) = 90_000 * 500 + 100 * 2_500 + 20_000 * 50,
             "Haiku 5.5 beyond 100K prompt tokens (cache reads count): USD 0.50 / 2.50");
      Check (Llm_Costs.Worst_Case (Haiku, Long, 1_000) = 90_000 * 500 + 1_000 * 2_500 + 20_000 * 50,
             "the worst case uses the long tier");
   end Test_Prices;

   ---------------------------------------------------------------------------
   --  Logging.
   ---------------------------------------------------------------------------
   procedure Test_Reports is
      Facts : constant Llm_Reports.Exchange_Facts :=
        (Exchange => 3, Source => 12, Stop => P.End_Turn, Error => P.No_Error,
         Fault => P.No_Problem, Status => 200,
         Usage => (Input => 25, Output => 12, Cache_Write => 10, Cache_Read => 5),
         Priced => True, Cost => Expected_Cost, Duration_Ms => 812, Text_Bytes => 40, Tool_Calls => 0);
      Line : constant String := Llm_Reports.Line (Facts, "console", "claude-opus-5-5", Fake_Key);
      Hostile : constant String := Llm_Reports.Line
        (Facts, "steal " & Fake_Key & LF & "x", "m" & Fake_Key, Fake_Key);
   begin
      Check (Line = "llm: exchange 3 source 12 mission ""console"" model claude-opus-5-5 stop end_turn" &
               " status 200 input 25 output 12 cache_read 5 cache_write 10 cost_nano_usd 391000" &
               " duration_ms 812 text_bytes 40", "log line: " & Line);
      Check (Count (Hostile, Fake_Key) = 0, "the key never appears in a log line");
      Check (Count (Hostile, Llm_Reports.Redacted) = 2, "an echoed key is redacted");
      Check (Count (Hostile, [1 => LF]) = 0, "log text is printable");
      Check (Llm_Reports.Redact ("invalid key " & Fake_Key, Fake_Key) = "invalid key [redacted]",
             "redact a server message");
      Check (Llm_Reports.Redact ("abc", "") = "abc", "nothing to redact without a key");
   end Test_Reports;

   --  llm_main --decode FILE: decode a captured response (for example
   --  from tests/llm/mock_server.py) and print what the decoder made of it.
   procedure Decode_File (Path : String) is
      use Ada.Streams.Stream_IO;
      File : Ada.Streams.Stream_IO.File_Type;
      Item : Ada.Streams.Stream_Element_Array (1 .. 1);
      Last : Ada.Streams.Stream_Element_Offset;
      Input : Unbounded_String;
   begin
      Open (File, Ada.Streams.Stream_IO.In_File, Path);
      while not Ada.Streams.Stream_IO.End_Of_File (File) loop
         Read (File, Item, Last);
         Append (Input, Character'Val (Item (1)));
      end loop;
      Ada.Streams.Stream_IO.Close (File);
      declare
         O : constant Outcome := Run (To_String (Input));
      begin
         Put_Line ("stop " & Llm_Reports.Stop_Name (O.Stop) & " error " & Llm_Reports.Error_Name (O.Error) &
                   " problem " & Llm_Reports.Problem_Name (O.Fault) & " input" & O.Usage.Input'Image &
                   " output" & O.Usage.Output'Image & " cost" & O.Cost'Image);
         Put_Line ("text [" & To_String (O.Text) & "]");
      end;
   end Decode_File;

begin
   if Ada.Command_Line.Argument_Count = 2 and then Ada.Command_Line.Argument (1) = "--decode" then
      Decode_File (Ada.Command_Line.Argument (2));
      return;
   end if;
   Test_Requests;
   Test_Event_Stream;
   Test_Stop_Reasons;
   Test_Errors;
   Test_Malformed;
   Test_Disconnect;
   Test_Budgets;
   Test_Reports;
   Test_Capture;
   Test_Prices;
   if Failures = 0 then
      Put_Line ("PASS: llm core" & Checks'Image & " checks");
   else
      Put_Line ("FAIL: llm core" & Failures'Image & " of" & Checks'Image & " checks");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Llm_Main;
