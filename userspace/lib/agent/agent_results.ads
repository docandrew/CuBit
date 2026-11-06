------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A tool's typed CCL result as the text of its tool_result (AGENT-002):
--  JSON, in the shape the tool's input schemas use (records as objects,
--  enum members as strings, variants as one-member objects, lists as
--  arrays), bounded, with any cut labelled.
--
--  @description
--  The result goes back to the model, never to an interpreter: it is data
--  (a log message that says "ignore your instructions" is a string in a
--  JSON array, as it was in the log). A result longer than the caller's
--  bound is cut at a whole byte and ends with Truncation_Label and the
--  full length, so the model knows it saw a prefix.
------------------------------------------------------------------------------
pragma Ada_2022;
with CCL.Objects;
with CCL.Objects.Views;

package Agent_Results with SPARK_Mode is

   Truncation_Label : constant String := "[truncated: showing";
   Maximum_Depth : constant := 16;

   --  The value at the snapshot's root as JSON into Into; Total is the
   --  length the whole rendering has (Last < Total: it was cut, and the
   --  label says so).
   procedure Render
     (Object : CCL.Objects.Views.Snapshot; Into : out String; Last : out Natural; Total : out Natural)
   with Pre => Into'First = 1 and then Into'Length >= 64 and then Into'Last < Positive'Last / 2,
        Post => Last <= Into'Last;

   --  A plain text result (a String-typed operation's), bounded the same way.
   procedure Render_Text (Text : String; Into : out String; Last : out Natural)
   with Pre => Into'First = 1 and then Into'Length >= 64 and then Into'Last < Positive'Last / 2,
        Post => Last <= Into'Last;
end Agent_Results;
