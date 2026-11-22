------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The typed calls a holder of the "llm" service endpoint makes to llm.svc
--  (AGENT-001 slice 1, docs/agent-harness.md "Order" step 1): the Messages
--  API client that holds the API key and the only network authority on the
--  agent path (its manifest's tls-scope names exactly one endpoint). The
--  API key is a named secret in secrets.svc (CuBit.Secrets_Protocol):
--  llm.svc holds its use grant and takes the value only to build each
--  request's header, which it wipes once written. Clients never see it.
--
--  @description
--  Every call but Pull is answered at once; Pull is submitted (capSubmit)
--  and answered when the exchange has news, so a client never blocks on
--  the model. The IPC graph stays a DAG: llm.svc never calls a client.
--  - Status (call) -> OK [exchanges in flight].
--  - Ask (call: grant slot, grant generation, request bytes, window
--    bytes): an encoded request (Request layout below) at offset 0. OK [exchange], or ERR
--    with a Refusal. The grant stays the exchange's window: Pull answers
--    into it, so the client keeps it until the exchange is done. Windows
--    are sized per ask (LIM-001): at least Minimum_Window_Bytes and the
--    request, at most Maximum_Window_Bytes; nothing else is fixed.
--  - Pull (submit: exchange, max bytes): Reply_Text [count] when count
--    > 0 bytes of streamed text were copied to offset 0; once the exchange
--    ended and every text byte was pulled, Reply_Content [count] for each
--    next piece of a captured exchange's content blocks (in order, at
--    offset 0), and then Reply_Done [0] with the Summary layout at offset
--    0 (its content length is the sum of the pieces). One Pull per
--    exchange at a time.
--  - Cancel (call: exchange) -> OK: the exchange stops (Cancelled) and its
--    next Pull answers Done.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Llm_Protocol with Pure, SPARK_Mode is

   --  Registered service identity (the kernel service registry) and catalog
   --  service number (userspace/ccl/catalogs/native-runtime-services.ccl).
   Service_Role : constant Unsigned_64 := 37;

   Status_Operation     : constant Unsigned_32 := 16#0D03#;
   Ask_Operation        : constant Unsigned_32 := 16#0D04#;
   Pull_Operation       : constant Unsigned_32 := 16#0D05#;
   Cancel_Operation     : constant Unsigned_32 := 16#0D06#;

   Reply_OK    : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;
   Reply_Text  : constant Unsigned_32 := 16#F010#;
   Reply_Done  : constant Unsigned_32 := 16#F011#;
   Reply_Content : constant Unsigned_32 := 16#F012#;

   --  Why a call was refused (word 0 of Reply_Error).
   type Refusal is
     (Malformed_Request, No_Key, Key_Not_Granted, Key_Rejected, Busy,
      Unknown_Exchange, Invalid_Model, Unpriced_Model, Invalid_Limits,
      Service_Failure, Request_Too_Large);
   for Refusal use
     (Malformed_Request => 1, No_Key => 2, Key_Not_Granted => 3,
      Key_Rejected => 4, Busy => 5, Unknown_Exchange => 6, Invalid_Model => 7,
      Unpriced_Model => 8, Invalid_Limits => 9, Service_Failure => 10,
      Request_Too_Large => 11);

   function Name (Value : Refusal) return String is
     (case Value is
         when Malformed_Request => "malformed request",
         when No_Key => "no API key stored: (secrets.set (Secret ""anthropic-api-key""))",
         when Key_Not_Granted => "llm.svc holds no use grant for its key secret",
         when Key_Rejected => "the stored key is not one printable ASCII token",
         when Busy => "too many exchanges in flight",
         when Unknown_Exchange => "unknown exchange",
         when Invalid_Model => "not a model identifier",
         when Unpriced_Model => "a cost ceiling needs a model with a known price",
         when Invalid_Limits => "a limit is out of range",
         when Service_Failure => "service failure",
         when Request_Too_Large => "capacity: the request is larger than llm.svc's ceiling");

   ---------------------------------------------------------------------------
   --  How an exchange ended. The first eight are the server's stop_reason;
   --  the rest are this side's typed stops.
   ---------------------------------------------------------------------------
   type Stop_Reason is
     (End_Turn, Max_Tokens, Stop_Sequence, Tool_Use, Pause_Turn, Refusal_Stop,
      Context_Window_Exceeded, Unknown_Stop_Reason,
      Cost_Budget, Deadline, Cancelled, Api_Error, Malformed_Response,
      Disconnected, Connect_Failed);
   for Stop_Reason use
     (End_Turn => 1, Max_Tokens => 2, Stop_Sequence => 3, Tool_Use => 4,
      Pause_Turn => 5, Refusal_Stop => 6, Context_Window_Exceeded => 7,
      Unknown_Stop_Reason => 8, Cost_Budget => 9, Deadline => 10,
      Cancelled => 11, Api_Error => 12, Malformed_Response => 13,
      Disconnected => 14, Connect_Failed => 15);

   --  The API's error types (an error event, or a non-200 response).
   type Error_Kind is
     (No_Error, Invalid_Request, Authentication, Billing, Permission,
      Not_Found, Request_Too_Large, Rate_Limited, Api_Internal, Overloaded,
      Gateway_Timeout, Other_Error);
   for Error_Kind use
     (No_Error => 0, Invalid_Request => 1, Authentication => 2, Billing => 3,
      Permission => 4, Not_Found => 5, Request_Too_Large => 6,
      Rate_Limited => 7, Api_Internal => 8, Overloaded => 9,
      Gateway_Timeout => 10, Other_Error => 11);

   --  What was wrong with a malformed response (Malformed_Response).
   type Problem is
     (No_Problem, Bad_Status_Line, Headers_Too_Large, Bad_Header,
      Bad_Content_Type, Bad_Chunk, Bad_Event_Stream, Data_Without_Event,
      Json_Syntax, Json_Too_Deep, Field_Too_Long, Number_Overflow,
      Type_Mismatch, Out_Of_Order, Missing_Stop_Reason, Response_Too_Large,
      Truncated_Event, Content_Too_Large, Bad_Content_Block);
   for Problem use
     (No_Problem => 0, Bad_Status_Line => 1, Headers_Too_Large => 2,
      Bad_Header => 3, Bad_Content_Type => 4, Bad_Chunk => 5,
      Bad_Event_Stream => 6, Data_Without_Event => 7, Json_Syntax => 8,
      Json_Too_Deep => 9, Field_Too_Long => 10, Number_Overflow => 11,
      Type_Mismatch => 12, Out_Of_Order => 13, Missing_Stop_Reason => 14,
      Response_Too_Large => 15, Truncated_Event => 16, Content_Too_Large => 17,
      Bad_Content_Block => 18);

   --  Each value's wire name, as the API spells it where it has one (logs,
   --  CCL replies). The runtime discards enumeration names.
   function Image (Value : Stop_Reason) return String is
     (case Value is
         when End_Turn => "end_turn",
         when Max_Tokens => "max_tokens",
         when Stop_Sequence => "stop_sequence",
         when Tool_Use => "tool_use",
         when Pause_Turn => "pause_turn",
         when Refusal_Stop => "refusal",
         when Context_Window_Exceeded => "context_window_exceeded",
         when Unknown_Stop_Reason => "unknown_stop_reason",
         when Cost_Budget => "cost_budget",
         when Deadline => "deadline",
         when Cancelled => "cancelled",
         when Api_Error => "api_error",
         when Malformed_Response => "malformed_response",
         when Disconnected => "disconnected",
         when Connect_Failed => "connect_failed");
   function Image (Value : Error_Kind) return String is
     (case Value is
         when No_Error => "none",
         when Invalid_Request => "invalid_request",
         when Authentication => "authentication",
         when Billing => "billing",
         when Permission => "permission",
         when Not_Found => "not_found",
         when Request_Too_Large => "request_too_large",
         when Rate_Limited => "rate_limited",
         when Api_Internal => "api_internal",
         when Overloaded => "overloaded",
         when Gateway_Timeout => "timeout",
         when Other_Error => "other");
   function Image (Value : Problem) return String is
     (case Value is
         when No_Problem => "none",
         when Bad_Status_Line => "bad_status_line",
         when Headers_Too_Large => "headers_too_large",
         when Bad_Header => "bad_header",
         when Bad_Content_Type => "bad_content_type",
         when Bad_Chunk => "bad_chunk",
         when Bad_Event_Stream => "bad_event_stream",
         when Data_Without_Event => "data_without_event",
         when Json_Syntax => "json_syntax",
         when Json_Too_Deep => "json_too_deep",
         when Field_Too_Long => "field_too_long",
         when Number_Overflow => "number_overflow",
         when Type_Mismatch => "type_mismatch",
         when Out_Of_Order => "out_of_order",
         when Missing_Stop_Reason => "missing_stop_reason",
         when Response_Too_Large => "response_too_large",
         when Truncated_Event => "truncated_event",
         when Content_Too_Large => "content_too_large",
         when Bad_Content_Block => "bad_content_block");

   ---------------------------------------------------------------------------
   --  Limits every request carries (no defaults: the caller names them).
   ---------------------------------------------------------------------------
   Maximum_Output_Tokens : constant := 128_000;
   subtype Output_Tokens is Unsigned_32 range 1 .. Maximum_Output_Tokens;
   --  Ten minutes: a deadline is relative to the Ask.
   Maximum_Deadline_Ms : constant := 600_000;
   subtype Deadline_Ms is Unsigned_32 range 1 .. Maximum_Deadline_Ms;
   type Ceiling_Kind is (Unlimited, Nano_Usd);
   for Ceiling_Kind use (Unlimited => 0, Nano_Usd => 1);
   Maximum_Model_Bytes : constant := 64;
   Maximum_Key_Bytes : constant := 256;
   Maximum_Mission_Bytes : constant := 64;

   ---------------------------------------------------------------------------
   --  Request layout (Ask), little endian at these offsets; the sections
   --  follow the header in this order: model, mission, system, tools,
   --  conversation. The request is provider-neutral: a conversation of
   --  typed blocks and typed tool definitions, which llm.svc (the Messages
   --  API client) renders into its provider's request.
   ---------------------------------------------------------------------------
   Request_Max_Tokens_Offset    : constant := 0;   --  4 bytes
   Request_Deadline_Offset      : constant := 4;   --  4 bytes, milliseconds
   Request_Ceiling_Kind_Offset  : constant := 8;   --  1 byte (Ceiling_Kind)
   Request_Flags_Offset         : constant := 9;   --  1 byte (Flag_* bits)
   Request_Ceiling_Offset       : constant := 16;  --  8 bytes, nano-USD
   Request_Model_Length_Offset  : constant := 24;  --  4 bytes each
   Request_Mission_Length_Offset : constant := 28;
   Request_System_Length_Offset : constant := 32;
   Request_Tools_Length_Offset  : constant := 36;
   Request_Conversation_Length_Offset : constant := 40;
   Request_Header_Bytes         : constant := 48;

   --  Flags. Cache: mark the stable prefix (tools, system, the
   --  conversation so far) for the provider's prompt cache. Capture: keep
   --  every content block of the response (text, thinking, tool calls)
   --  and return them with the summary (Reply_Done), so a client can
   --  append the turn to its conversation and run the tool calls.
   Flag_Cache   : constant := 1;
   Flag_Capture : constant := 2;
   Known_Flags  : constant := Flag_Cache + Flag_Capture;

   --  Named ceilings (LIM-001): an ask is as long as its conversation
   --  needs, up to Maximum_Ask_Bytes (about a 1M-token context); a longer
   --  one is refused Request_Too_Large, a typed capacity error, never cut.
   --  Clients compact a conversation before it gets there.
   Maximum_Ask_Bytes : constant := 4 * 2**20;
   Maximum_System_Bytes : constant := 65_536;
   --  The exchange's window: big enough for the summary and a useful
   --  piece of text or content per Pull, and for the request; a ceiling
   --  bounds what one client may lend.
   Minimum_Window_Bytes : constant := 65_536;
   Maximum_Window_Bytes : constant := Maximum_Ask_Bytes + Minimum_Window_Bytes;

   ---------------------------------------------------------------------------
   --  Blocks: the conversation section, and the content a captured
   --  response returns, are sequences of blocks. Each is a Block_Header_Bytes
   --  header and then its three fields' bytes, A, B and C, in order:
   --    Text:        A = the text.
   --    Thinking:    A = the reasoning text (often empty), B = its signature.
   --    Redacted:    A = the provider's opaque data.
   --    Tool_Call:   A = the call's id, B = the tool's name, C = its input
   --                 (a JSON object).
   --    Tool_Result: A = the call's id, B = the result text; the error flag
   --                 says the call failed.
   --  Consecutive blocks with one role are one message.
   ---------------------------------------------------------------------------
   type Role is (User, Assistant);
   for Role use (User => 1, Assistant => 2);
   type Block_Kind is (Text_Block, Thinking_Block, Redacted_Block, Tool_Call_Block, Tool_Result_Block);
   for Block_Kind use
     (Text_Block => 1, Thinking_Block => 2, Redacted_Block => 3, Tool_Call_Block => 4,
      Tool_Result_Block => 5);
   Block_Role_Offset   : constant := 0;   --  1 byte (Role)
   Block_Kind_Offset   : constant := 1;   --  1 byte (Block_Kind)
   Block_Error_Offset  : constant := 2;   --  1 byte (1: a failed tool call)
   Block_A_Length_Offset : constant := 4; --  4 bytes each
   Block_B_Length_Offset : constant := 8;
   Block_C_Length_Offset : constant := 12;
   Block_Header_Bytes  : constant := 16;

   --  Tool definitions (the tools section): each a Tool_Header_Bytes header
   --  and then its name, description and input schema (a JSON object).
   Tool_Name_Length_Offset   : constant := 0;   --  2 bytes
   Tool_Description_Length_Offset : constant := 2;   --  2 bytes
   Tool_Schema_Length_Offset : constant := 4;   --  4 bytes
   Tool_Header_Bytes         : constant := 8;
   Maximum_Tool_Name_Bytes   : constant := 64;
   Maximum_Tool_Id_Bytes     : constant := 64;

   --  The ceiling on a captured response's content (Reply_Content pieces):
   --  llm.svc sizes each exchange's capture from its max_tokens
   --  (Content_Bytes_Per_Token each, plus Content_Overhead_Bytes), up to
   --  this; beyond it is the typed Content_Too_Large.
   Maximum_Content_Bytes : constant := 8 * 2**20;
   Content_Overhead_Bytes : constant := 65_536;
   Content_Bytes_Per_Token : constant := 32;

   ---------------------------------------------------------------------------
   --  Summary layout (Reply_Done), little endian at these offsets.
   ---------------------------------------------------------------------------
   Summary_Stop_Offset          : constant := 0;   --  1 byte (Stop_Reason)
   Summary_Error_Offset         : constant := 1;   --  1 byte (Error_Kind)
   Summary_Problem_Offset       : constant := 2;   --  1 byte (Problem)
   Summary_Priced_Offset        : constant := 3;   --  1 byte (1: cost known)
   Summary_Status_Offset        : constant := 4;   --  4 bytes, HTTP status (0: none)
   Summary_Input_Offset         : constant := 8;   --  8 bytes each
   Summary_Output_Offset        : constant := 16;
   Summary_Cache_Read_Offset    : constant := 24;
   Summary_Cache_Write_Offset   : constant := 32;
   Summary_Cost_Offset          : constant := 40;  --  nano-USD
   Summary_Duration_Offset      : constant := 48;  --  milliseconds
   Summary_Text_Bytes_Offset    : constant := 56;
   Summary_Model_Length_Offset  : constant := 64;  --  4 bytes
   Summary_Detail_Length_Offset : constant := 68;  --  4 bytes
   Summary_Content_Length_Offset : constant := 72; --  4 bytes: captured blocks' bytes (sent as Reply_Content)
   Summary_Tool_Calls_Offset    : constant := 76;  --  4 bytes: whole tool calls captured
   Summary_Model_Offset         : constant := 80;  --  Maximum_Model_Bytes
   Summary_Detail_Offset        : constant := Summary_Model_Offset + Maximum_Model_Bytes;
   --  The server's error message (redacted), cut to this.
   Maximum_Detail_Bytes         : constant := 160;
   Summary_Bytes                : constant := Summary_Detail_Offset + Maximum_Detail_Bytes;
   pragma Compile_Time_Error (Summary_Bytes > Minimum_Window_Bytes, "a window holds the summary");
end CuBit.Llm_Protocol;
