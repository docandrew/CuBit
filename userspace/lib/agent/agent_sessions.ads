------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A session's record (AGENT-002, docs/agent-harness.md "Sessions and
--  missions"): the conversation as typed CCL values, one Agent_Record per
--  line, appended as the conversation grows, in the user's data scope.
--
--  @description
--  Each conversation block (Agent_Blocks) is one or more records:
--    (Agent_Record role => Agent_Role.User kind => Agent_Block_Kind.Text
--       failed => false a => "..." b => "" c => "" more => false)
--  A field longer than Field_Chunk continues in the next record (more =>
--  true), so every record stays within one CCL value's text bound. Reading
--  a record back is the CCL type checker's job (CCL.Typed_Values against
--  Schema): a tampered or malformed line is a typed error, never a guess.
--  Strings use CCL's escapes (\" \\ \n \r \t) and keep every other byte.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Llm_Protocol;

package Agent_Sessions with Pure, SPARK_Mode is
   package P renames CuBit.Llm_Protocol;

   Schema : constant String :=
     "(type Agent_Role (enum User Assistant)) " &
     "(type Agent_Block_Kind (enum Text Thinking Redacted Tool_Call Tool_Result)) " &
     "(type Agent_Record (record (role Agent_Role) (kind Agent_Block_Kind) (failed Boolean) " &
     "(a String) (b String) (c String) (more Boolean)))";
   Root_Name : constant String := "Agent_Record";

   --  Fields, in declaration order.
   Record_Role : constant := 1;
   Record_Kind : constant := 2;
   Record_Failed : constant := 3;
   Record_A : constant := 4;
   Record_B : constant := 5;
   Record_C : constant := 6;
   Record_More : constant := 7;

   --  The most of all three fields one record carries.
   Field_Chunk : constant := 2_048;
   Maximum_Field : constant := 2**20;

   function Kind_Name (Kind : P.Block_Kind) return String is
     (case Kind is
         when P.Text_Block => "Text",
         when P.Thinking_Block => "Thinking",
         when P.Redacted_Block => "Redacted",
         when P.Tool_Call_Block => "Tool_Call",
         when P.Tool_Result_Block => "Tool_Result");

   --  Text as the inside of a CCL string literal.
   function Quote (Text : String) return String
   with Pre => Text'Length <= Maximum_Field,
        Post => Quote'Result'Length <= 2 * Text'Length;

   --  The lines (each ending in a line feed) that record one block.
   function Lines
     (Item_Role : P.Role; Kind : P.Block_Kind; Failed : Boolean; A, B, C : String) return String
   with Pre => A'Length <= Maximum_Field and then B'Length <= Maximum_Field and then
               C'Length <= Maximum_Field and then A'Last < Positive'Last and then
               B'Last < Positive'Last and then C'Last < Positive'Last,
        Subprogram_Variant => (Decreases => A'Length + B'Length + C'Length);
end Agent_Sessions;
