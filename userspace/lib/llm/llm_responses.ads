------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The response to a streaming Messages API request, decoded one byte at
--  a time (AGENT-001 slice 1): HTTP/1.1 status and headers, chunked or
--  identity framing, Server-Sent Events, and the JSON of each event, into
--  typed results and the text deltas as they arrive.
--
--  @description
--  The server is untrusted. Every byte is checked against the grammar it
--  belongs to, and nothing content-sized is buffered: text deltas leave as
--  soon as they are decoded, other strings are kept only where the client
--  needs them (an event's type, the model, the stop reason, the error type)
--  and a longer one is the typed Field_Too_Long. Malformed input ends the
--  exchange with Malformed_Response and the Problem; it never raises.
--
--  Events: message_start (model, usage), content_block_start (a block's
--  type and initial text; a tool call's id and name), content_block_delta
--  (text, thinking, signature and tool input deltas), content_block_stop,
--  message_delta (stop_reason, cumulative usage),
--  message_stop, ping, error (the API's error type and message). An event
--  this client does not know is parsed and ignored (the API may add some).
--  Each event's "type" must equal its event name; content events come only
--  between message_start and message_stop.
--
--  Budgets: the response is at most Response_Overhead_Bytes plus
--  Response_Bytes_Per_Token for every output token allowed. With a cost
--  ceiling, the exchange stops as Cost_Budget at message_start if its
--  input plus every output token it may still produce would cost more
--  than the ceiling, so what it costs never exceeds the ceiling (the
--  caller then closes the connection). The deadline is the caller's
--  (Stop_With Deadline).
--
--  Capture (Limits.Capture): every content block of the response is also
--  kept, in CuBit.Llm_Protocol's block layout (assistant text, thinking
--  with its signature, redacted thinking, tool calls with id, name and
--  input), so the client can append the turn to its conversation and run
--  the tool calls. Blocks of a kind this client does not know are dropped,
--  as are empty text blocks. Content beyond the decoder's Capacity is the
--  typed Content_Too_Large; a block whose fields arrive out of order (a
--  signature before the reasoning is over, say), or a tool call without an
--  id or name, is Bad_Content_Block. Only whole blocks count.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Llm_Protocol;
with Llm_Costs;

package Llm_Responses with SPARK_Mode is
   package P renames CuBit.Llm_Protocol;

   subtype Byte is Unsigned_8;

   --  Named ceilings on what the decoder keeps (never on the text, which
   --  it does not keep).
   Maximum_Header_Section   : constant := 65_536;
   Maximum_Json_Depth       : constant := 32;
   Maximum_Capture          : constant := 64;
   Response_Bytes_Per_Token : constant := 1_024;
   Response_Overhead_Bytes  : constant := 1_048_576;

   type Limits is record
      Max_Tokens : P.Output_Tokens := 1;
      Ceiling : P.Ceiling_Kind := P.Unlimited;
      Ceiling_Nano_Usd : Unsigned_64 := 0;
      Price : Llm_Costs.Price;
      Capture : Boolean := False;
   end record;

   --  What one byte of input releases: up to four bytes of UTF-8 text.
   Maximum_Output : constant := 4;
   type Output_Bytes is array (1 .. Maximum_Output) of Byte;
   type Output is record
      Count : Natural range 0 .. Maximum_Output := 0;
      Bytes : Output_Bytes := [others => 0];
   end record;

   --  The captured content's capacity: sized from the request's max_tokens
   --  (LIM-001), up to the named ceiling P.Maximum_Content_Bytes; none
   --  without Capture.
   subtype Content_Count is Natural range 0 .. P.Maximum_Content_Bytes;
   function Capacity_For (Limit : Limits) return Content_Count is
     (if not Limit.Capture then 0
      else Natural'Min (P.Maximum_Content_Bytes,
                        P.Content_Overhead_Bytes + Natural (Limit.Max_Tokens) * P.Content_Bytes_Per_Token));

   type Decoder (Capacity : Content_Count) is private;

   --  A decoder for one response (its content area allocated by the
   --  caller, e.g. on the heap with Capacity_For (Limit)).
   procedure Begin_Response (D : in out Decoder; Limit : Limits)
   with Pre => Capacity_For (Limit) <= D.Capacity;
   function Start (Limit : Limits) return Decoder
   with Post => Start'Result.Capacity = Capacity_For (Limit);

   procedure Push (D : in out Decoder; B : Byte; Text : out Output);
   --  The connection ended (the peer closed it).
   procedure End_Of_Input (D : in out Decoder);
   --  A stop decided outside the response: the deadline passed, the
   --  client cancelled, the connection failed or broke.
   procedure Stop_With (D : in out Decoder; Reason : P.Stop_Reason)
   with Pre => Reason in P.Deadline | P.Cancelled | P.Connect_Failed | P.Disconnected;

   function Finished (D : Decoder) return Boolean;
   --  message_start arrived: the model and input usage are known.
   function Started (D : Decoder) return Boolean;
   function Stop_Of (D : Decoder) return P.Stop_Reason;
   function Error_Of (D : Decoder) return P.Error_Kind;
   function Problem_Of (D : Decoder) return P.Problem;
   --  The response's HTTP status (0: none arrived).
   function Status_Of (D : Decoder) return Natural;
   function Usage_Of (D : Decoder) return Llm_Costs.Usage;
   function Cost_Known (D : Decoder) return Boolean;
   function Cost_Of (D : Decoder) return Llm_Costs.Nano_Usd;
   --  The model the server named in message_start.
   function Model_Of (D : Decoder) return String
   with Post => Model_Of'Result'Length <= Maximum_Capture;
   --  The server's error message, cut to P.Maximum_Detail_Bytes.
   function Detail_Of (D : Decoder) return String
   with Post => Detail_Of'Result'Length <= P.Maximum_Detail_Bytes;
   --  Text bytes released so far.
   function Text_Bytes_Of (D : Decoder) return Unsigned_64;

   --  The captured content: whole blocks only.
   function Content_Length (D : Decoder) return Content_Count;
   function Content_Byte (D : Decoder; Index : Positive) return Byte
   with Pre => Index <= Content_Length (D) and then Index <= D.Capacity;
   --  Whole tool calls captured.
   function Tool_Calls_Of (D : Decoder) return Natural;

private
   use type P.Stop_Reason;

   Maximum_Header_Name  : constant := 32;
   Maximum_Header_Value : constant := 64;
   Maximum_Field_Name   : constant := 8;
   Maximum_Event_Name   : constant := 32;
   Maximum_Key          : constant := 32;
   Tracked_Depth        : constant := 4;
   Maximum_Status_Version : constant := 16;
   Maximum_Chunk_Digits : constant := 12;
   Maximum_Length_Digits : constant := 15;

   type Http_Phase is
     (Status_Line, Header_Line, Body_Identity, Body_Until_Close, Chunk_Size,
      Chunk_Extension, Chunk_Data, Chunk_Data_End, Trailer_Line, Body_Done);
   type Status_Step is (Version, Code, Reason);

   type Sse_Step is (Line_Start, Field_Name, Field_Space, Field_Value, Ignore_Line);
   type Sse_Field is (No_Field, Event_Field, Data_Field, Other_Field);
   type Event_Kind is
     (No_Event, Message_Start, Content_Block_Start, Content_Block_Delta,
      Content_Block_Stop, Message_Delta, Message_Stop, Ping, Error_Event,
      Unknown_Event);

   type Container is (In_Object, In_Array);
   type Expectation is
     (Expect_Value, Expect_Value_Or_Close, Expect_Key_Or_Close, Expect_Key,
      Expect_Colon, Expect_Comma_Or_Close, Expect_Nothing);
   type Lexeme is (No_Lexeme, In_String, In_Escape, In_Unicode, In_Number, In_Literal);
   type Number_Step is
     (After_Minus, After_Zero, Int_Digits, After_Point, Frac_Digits,
      After_Exponent, After_Exponent_Sign, Exp_Digits);
   type Literal_Kind is (Literal_True, Literal_False, Literal_Null);

   type Key_Id is
     (K_Other, K_Element, K_Type, K_Message, K_Model, K_Usage, K_Input_Tokens,
      K_Output_Tokens, K_Cache_Creation, K_Cache_Read, K_Content_Block,
      K_Delta, K_Text, K_Stop_Reason, K_Error, K_Id, K_Name, K_Partial_Json,
      K_Thinking, K_Signature, K_Data);
   type Target is
     (T_None, T_Text, T_Thinking, T_Signature, T_Redacted, T_Tool_Input,
      T_Type, T_Model, T_Stop, T_Error_Type, T_Block_Type, T_Tool_Id, T_Tool_Name,
      T_Error_Message, T_Input, T_Output, T_Cache_Write, T_Cache_Read);
   subtype Number_Target is Target range T_Input .. T_Cache_Read;
   subtype Content_Target is Target range T_Text .. T_Tool_Input;
   subtype Capture_Target is Target range T_Type .. T_Tool_Name;

   --  A captured block's fields, in layout order.
   type Content_Field is (Field_A, Field_B, Field_C);
   type Block_Type is (Unknown_Block, Text_Type, Thinking_Type, Redacted_Type, Tool_Use_Type);
   type Content_Area is array (Positive range <>) of Byte;
   type Field_Lengths is array (Content_Field) of Content_Count;

   type Message_Phase is (Awaiting_Start, In_Message);

   subtype Depth_Count is Natural range 0 .. Maximum_Json_Depth;
   type Container_Stack is array (1 .. Maximum_Json_Depth) of Container;
   type Key_Path is array (1 .. Tracked_Depth) of Key_Id;
   subtype Code_Unit is Natural range 0 .. 16#FFFF#;

   type Decoder_State is record
      Limit : Limits;
      Done : Boolean := False;
      Result : P.Stop_Reason := P.Disconnected;
      Error : P.Error_Kind := P.No_Error;
      Fault : P.Problem := P.No_Problem;
      Body_Limit : Unsigned_64 := Response_Overhead_Bytes;

      --  HTTP.
      Phase : Http_Phase := Status_Line;
      Status_At : Status_Step := Version;
      Version_Length : Natural range 0 .. Maximum_Status_Version := 0;
      Status_Digits : Natural range 0 .. 3 := 0;
      Status : Natural range 0 .. 999 := 0;
      Line_CR : Boolean := False;
      Line_Empty : Boolean := True;
      Header_Bytes : Natural range 0 .. Maximum_Header_Section := 0;
      Name : String (1 .. Maximum_Header_Name) := [others => ' '];
      Name_Length : Natural range 0 .. Maximum_Header_Name := 0;
      Name_Over : Boolean := False;
      In_Value : Boolean := False;
      Value_Started : Boolean := False;
      Value : String (1 .. Maximum_Header_Value) := [others => ' '];
      Value_Length : Natural range 0 .. Maximum_Header_Value := 0;
      Value_Over : Boolean := False;
      Chunked : Boolean := False;
      Have_Length : Boolean := False;
      Event_Stream : Boolean := False;
      Remaining : Unsigned_64 := 0;
      Chunk_Digits : Natural range 0 .. Maximum_Chunk_Digits := 0;
      Body_Bytes : Unsigned_64 := 0;
      Error_Document : Boolean := False;
      Document_Broken : Boolean := False;

      --  Server-Sent Events.
      Sse_At : Sse_Step := Line_Start;
      Sse_CR : Boolean := False;
      Field : Sse_Field := No_Field;
      Field_Text : String (1 .. Maximum_Field_Name) := [others => ' '];
      Field_Length : Natural range 0 .. Maximum_Field_Name := 0;
      Field_Over : Boolean := False;
      Event_Name : String (1 .. Maximum_Event_Name) := [others => ' '];
      Event_Name_Length : Natural range 0 .. Maximum_Event_Name := 0;
      Event_Name_Over : Boolean := False;
      Event_Named : Boolean := False;
      Event : Event_Kind := No_Event;
      Data_Seen : Boolean := False;

      --  JSON of the current event (or of an error document).
      Depth : Depth_Count := 0;
      Stack : Container_Stack := [others => In_Object];
      Keys : Key_Path := [others => K_Other];
      Expect : Expectation := Expect_Value;
      Lex : Lexeme := No_Lexeme;
      Is_Key : Boolean := False;
      Key : String (1 .. Maximum_Key) := [others => ' '];
      Key_Length : Natural range 0 .. Maximum_Key := 0;
      Key_Over : Boolean := False;
      Hex_Count : Natural range 0 .. 4 := 0;
      Code : Code_Unit := 0;
      High_Surrogate : Code_Unit := 0;
      Number_At : Number_Step := Int_Digits;
      Number_Plain : Boolean := True;
      Number_Value : Llm_Costs.Token_Count := 0;
      Literal : Literal_Kind := Literal_Null;
      Literal_Position : Natural range 0 .. 5 := 0;
      Goal : Target := T_None;
      Capture : String (1 .. Maximum_Capture) := [others => ' '];
      Capture_Length : Natural range 0 .. Maximum_Capture := 0;
      Capture_Over : Boolean := False;
      Type_Seen : Boolean := False;
      Type_Matches : Boolean := False;

      --  The message.
      Message_At : Message_Phase := Awaiting_Start;
      Has_Started : Boolean := False;
      Model : String (1 .. Maximum_Capture) := [others => ' '];
      Model_Length : Natural range 0 .. Maximum_Capture := 0;
      Usage : Llm_Costs.Usage;
      Server_Stop : P.Stop_Reason := P.Unknown_Stop_Reason;
      Server_Stop_Seen : Boolean := False;
      Error_Seen : P.Error_Kind := P.No_Error;
      Detail : String (1 .. P.Maximum_Detail_Bytes) := [others => ' '];
      Detail_Length : Natural range 0 .. P.Maximum_Detail_Bytes := 0;
      Text_Bytes : Unsigned_64 := 0;

      --  Capture: whole blocks end at Content_Done; the open block's
      --  header is at Block_Head, its bytes after it.
      Content_Done : Content_Count := 0;
      Content_End : Content_Count := 0;
      Block_Open : Boolean := False;
      Block_Head : Content_Count := 0;
      Block_Kind : Block_Type := Unknown_Block;
      Field_At : Content_Field := Field_A;
      Lengths : Field_Lengths := [others => 0];
      Tool_Id : String (1 .. Maximum_Capture) := [others => ' '];
      Tool_Id_Length : Natural range 0 .. Maximum_Capture := 0;
      Tool_Name : String (1 .. Maximum_Capture) := [others => ' '];
      Tool_Name_Length : Natural range 0 .. Maximum_Capture := 0;
      Tool_Calls : Natural := 0;
   end record;

   type Decoder (Capacity : Content_Count) is record
      S : Decoder_State;
      Content : Content_Area (1 .. Capacity) := [others => 0];
   end record;
end Llm_Responses;
