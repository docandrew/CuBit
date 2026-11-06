------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Basic conversation compaction (AGENT-002, docs/agent-harness.md "The
--  loop", step 5): a conversation that reaches its limit is compacted, not
--  refused.
--
--  @description
--  Compaction drops whole earlier turns from the front and puts a visible
--  marker (a user text block saying what was dropped) in their place. A
--  turn starts at a user text block, so a tool round is never split: every
--  tool result kept answers a tool call kept. The kept tail loses its
--  thinking and redacted-thinking blocks: those were produced with the
--  dropped history present, and the provider's history-editing check
--  ("preserved thinking") refuses them after an edit; text and tool calls
--  stay. Summarizing the dropped turns with an extra model call is a later
--  step; this one keeps the agent running.
--
--  The limits are named (LIM-001): a conversation is compacted when its
--  section passes Compact_At_Bytes or its last request's prompt passed
--  Compact_At_Prompt_Tokens, to at most Compact_Target_Bytes when a turn
--  boundary allows it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with CuBit.Llm_Protocol;
with Agent_Blocks;

package Agent_Compaction with SPARK_Mode is
   package P renames CuBit.Llm_Protocol;
   use type Interfaces.Unsigned_64;

   --  About 500K tokens of conversation (four bytes a token), and the
   --  prompt size that triggers compaction on a 1M-token model; the
   --  target leaves room for the turns that follow.
   Compact_At_Bytes : constant := 2 * 2**20;
   Compact_At_Prompt_Tokens : constant := 600_000;
   Compact_Target_Bytes : constant := 2**20;
   pragma Compile_Time_Error
     (Compact_At_Bytes >= P.Maximum_Ask_Bytes, "compaction comes before the ask ceiling");

   Marker_Prefix : constant String := "[compacted: ";

   --  Whether a conversation of Conversation_Bytes, whose last request's
   --  prompt was Prompt_Tokens, is due for compaction.
   function Needed (Conversation_Bytes : Natural; Prompt_Tokens : Interfaces.Unsigned_64) return Boolean is
     (Conversation_Bytes > Compact_At_Bytes or else
      Prompt_Tokens > Interfaces.Unsigned_64 (Compact_At_Prompt_Tokens));

   type Plan is record
      --  False: no turn boundary past the first block; nothing can go.
      Found : Boolean := False;
      --  Where the kept tail starts in the conversation.
      Keep_From : Positive := 1;
      Dropped_Blocks : Natural := 0;
      Dropped_Bytes : Natural := 0;
   end record;

   function Valid_Section (Conversation : String) return Boolean is
     (Conversation'First = 1 and then Conversation'Length <= Agent_Blocks.Maximum_Section);

   --  The earliest turn start (a user text block after the first block)
   --  whose tail, with the marker, fits Target; if none fits, the latest
   --  turn start.
   function Plan_For (Conversation : String; Target : Natural) return Plan
   with Pre => Valid_Section (Conversation),
        Post => (if Plan_For'Result.Found then
                   Plan_For'Result.Keep_From in 2 .. Conversation'Last and then
                   Plan_For'Result.Dropped_Bytes = Plan_For'Result.Keep_From - 1);

   --  The marker's text: what was dropped, said to the model and shown.
   Maximum_Marker : constant := 256;
   function Marker_Text (Item : Plan) return String
   with Post => Marker_Text'Result'Length <= Maximum_Marker;

   --  The compacted conversation's length, and the conversation itself:
   --  the marker block, then the tail from Keep_From without thinking.
   function Compacted_Length (Conversation : String; Item : Plan) return Natural
   with Pre => Valid_Section (Conversation) and then Item.Found and then
               Item.Keep_From in 2 .. Conversation'Last,
        Post => Compacted_Length'Result <= Conversation'Length + P.Block_Header_Bytes + Maximum_Marker and then
                Compacted_Length'Result >= P.Block_Header_Bytes + Marker_Text (Item)'Length;

   procedure Compact (Conversation : String; Item : Plan; Into : out String; Last : out Natural)
   with Pre => Valid_Section (Conversation) and then Item.Found and then
               Item.Keep_From in 2 .. Conversation'Last and then
               Into'First = 1 and then Into'Length = Compacted_Length (Conversation, Item),
        Post => Last <= Into'Last;
end Agent_Compaction;
