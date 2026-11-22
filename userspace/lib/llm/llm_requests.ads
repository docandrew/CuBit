------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A streaming Messages API request (POST /v1/messages), as HTTP/1.1 bytes,
--  from a provider-neutral request (AGENT-001 slice 1, AGENT-002): typed
--  tool definitions and a conversation of typed blocks
--  (CuBit.Llm_Protocol's tools and conversation sections). Pure: no I/O,
--  no allocation; the caller sizes the buffers from the lengths these
--  functions compute.
--
--  @description
--  The body is
--    {"model":M,"stream":true[,"tools":[...]][,"system":S],
--     "messages":[...],"max_tokens":N}
--  Everything that changes from one turn of a conversation to the next is
--  at the end: the conversation only grows, and max_tokens comes last, so
--  the bytes of one turn's request up to its last block are a prefix of
--  the next turn's. With Cache, prompt-cache breakpoints
--  ("cache_control":{"type":"ephemeral"}) mark the last tool, the system
--  text and the conversation's last block: tools, system and the history
--  so far are read from the cache on the next turn (render order tools,
--  system, messages).
--
--  Texts are written as JSON strings (escaped). A tool's input schema and
--  a tool call's input are JSON objects, embedded as they are once
--  Llm_Json has checked them (Valid_Tools, Valid_Conversation), so neither
--  can close the body's structure and add members of its own.
--
--  The header carries the documented headers (content-type,
--  anthropic-version, x-api-key) and Connection: close, so the response
--  ends with the connection. The key appears only in the x-api-key line of
--  the header, never in the body: the caller wipes the header once it is
--  written.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Llm_Protocol;

package Llm_Requests with Pure, SPARK_Mode is
   package P renames CuBit.Llm_Protocol;

   --  The API version this client speaks (claude-api reference, 2026-10-21).
   Api_Version : constant String := "2026-10-21";
   Messages_Path : constant String := "/v1/messages";
   --  The model a request names when its caller chooses Default.
   Default_Model : constant String := "claude-opus-5-5";
   User_Agent : constant String := "CuBit-llm/0.2";

   --  The longest section one request takes (each of system, tools and
   --  conversation): its JSON escape is at most six times as long.
   Maximum_Text : constant := 2**23;
   subtype Text_Length is Natural range 0 .. Maximum_Text;
   Maximum_Host : constant := 253 + 6;   --  a DNS name, ":" and a port
   --  A body never reaches this (a request's sections are far smaller);
   --  Body_Length saturates at it.
   Body_Ceiling : constant := 2**30;
   --  The highest index a section may have (its bounds stay far from
   --  Integer'Last, so index arithmetic cannot overflow).
   Maximum_Index : constant := 2**30;

   --  Model identifiers: lower-case letters, digits, '-' and '.'.
   function Valid_Model (Name : String) return Boolean is
     (Name'Length in 1 .. P.Maximum_Model_Bytes and then
      (for all C of Name => C in 'a' .. 'z' | '0' .. '9' | '-' | '.'));
   --  An API key is one printable ASCII token: no space, CR or LF can
   --  smuggle a second header line.
   function Valid_Key (Key : String) return Boolean is
     (Key'Length in 1 .. P.Maximum_Key_Bytes and then (for all C of Key => C in '!' .. '~'));
   --  The Host header: a DNS name, optionally ":port".
   function Valid_Host (Host : String) return Boolean is
     (Host'Length in 1 .. Maximum_Host and then
      (for all C of Host => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | ':'));
   --  Tool names (the API's ^[a-zA-Z0-9_-]{1,64}$) and tool call ids.
   function Valid_Tool_Name (Name : String) return Boolean is
     (Name'Length in 1 .. P.Maximum_Tool_Name_Bytes and then
      (for all C of Name => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-'));
   function Valid_Tool_Id (Id : String) return Boolean is
     (Id'Length in 1 .. P.Maximum_Tool_Id_Bytes and then
      (for all C of Id => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-'));

   --  A section's field, little endian.
   function Field (Text : String; Offset, Width : Natural) return Natural
   with Pre => Width in 1 .. 3 and then Text'Length <= Maximum_Text and then Text'Last <= Maximum_Index and then
               Offset <= Maximum_Text and then Offset + Width <= Text'Length,
        Post => Field'Result < (if Width = 1 then 2**8 elsif Width = 2 then 2**16 else 2**24);
   --  A 4-byte field, at most Maximum_Text (larger lengths read as
   --  Maximum_Text + 1, which no section can hold).
   function Length_Field (Text : String; Offset : Natural) return Natural
   with Pre => Text'Length <= Maximum_Text and then Text'Last <= Maximum_Index and then
               Offset <= Maximum_Text and then
               Offset + 4 <= Text'Length,
        Post => Length_Field'Result <= Maximum_Text + 1;

   --  A tools section: well-formed entries, each with a valid name and a
   --  JSON-object schema. An empty section is valid (no tools).
   function Valid_Tools (Tools : String) return Boolean
   with Pre => Tools'Length <= Maximum_Text and then Tools'Last <= Maximum_Index;

   --  A conversation section: well-formed blocks, each kind with its role
   --  and fields (a tool call's id, name and JSON-object input; a tool
   --  result's id; text that is not empty), beginning and ending with the
   --  user.
   function Valid_Conversation (Conversation : String) return Boolean
   with Pre => Conversation'Length <= Maximum_Text and then Conversation'Last <= Maximum_Index;

   --  The length of Text as the inside of a JSON string.
   function Escaped_Length (Text : String) return Natural
   with Pre => Text'Length <= Maximum_Text,
        Post => Escaped_Length'Result <= 6 * Text'Length;

   --  Text as the inside of a JSON string.
   function Escape (Text : String) return String
   with Pre => Text'Length <= Maximum_Text,
        Post => Escape'Result'Length = Escaped_Length (Text);

   function Decimal_Length (Value : Natural) return Positive
   with Post => Decimal_Length'Result <= 10;

   function Valid_Request (Model, System, Tools, Conversation : String) return Boolean is
     (Valid_Model (Model) and then System'Length <= Maximum_Text and then
      Tools'Length <= Maximum_Text and then Conversation'Length <= Maximum_Text and then
      Tools'Last <= Maximum_Index and then Conversation'Last <= Maximum_Index and then
      Valid_Tools (Tools) and then Valid_Conversation (Conversation));

   function Body_Length
     (Model : String; Max_Tokens : P.Output_Tokens; System, Tools, Conversation : String;
      Cache : Boolean) return Natural
   with Pre => Valid_Request (Model, System, Tools, Conversation),
        Post => Body_Length'Result <= Body_Ceiling;

   procedure Write_Body
     (Model : String; Max_Tokens : P.Output_Tokens; System, Tools, Conversation : String;
      Cache : Boolean; Into : out String)
   with Pre => Valid_Request (Model, System, Tools, Conversation) and then Into'First = 1 and then
               Into'Length = Body_Length (Model, Max_Tokens, System, Tools, Conversation, Cache)
               and then Into'Length < Body_Ceiling;

   function Header_Length (Host, Key : String; Content_Length : Natural) return Natural
   with Pre => Valid_Host (Host) and then Valid_Key (Key);

   procedure Write_Header
     (Host, Key : String; Content_Length : Natural; Into : out String)
   with Pre => Valid_Host (Host) and then Valid_Key (Key) and then Into'First = 1 and then
               Into'Length = Header_Length (Host, Key, Content_Length);
end Llm_Requests;
