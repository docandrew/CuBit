------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Conversation blocks (CuBit.Llm_Protocol's block layout): encoding the
--  ones a client adds (the person's text, tool results) and reading the
--  ones a captured response returns (assistant text, thinking, tool
--  calls). AGENT-002's sessions are a sequence of these.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Llm_Protocol;

package Agent_Blocks with Pure, SPARK_Mode is
   package P renames CuBit.Llm_Protocol;

   --  The longest section this reads (more than any request holds).
   Maximum_Section : constant := 2**24;
   --  The longest field one block encodes.
   Maximum_Field : constant := 2**20;

   function Encoded_Length (A, B, C : String) return Natural is
     (P.Block_Header_Bytes + A'Length + B'Length + C'Length)
   with Pre => A'Length <= Maximum_Field and then B'Length <= Maximum_Field and then
               C'Length <= Maximum_Field;

   function Encode
     (Item_Role : P.Role; Kind : P.Block_Kind; A : String; B : String := ""; C : String := "";
      Failed : Boolean := False) return String
   with Pre => A'Length <= Maximum_Field and then B'Length <= Maximum_Field and then
               C'Length <= Maximum_Field,
        Post => Encode'Result'Length = Encoded_Length (A, B, C);

   type Block_View is record
      Item_Role : P.Role := P.User;
      Kind : P.Block_Kind := P.Text_Block;
      Failed : Boolean := False;
      --  Each field's first byte (an index into the section) and length.
      A_First, B_First, C_First : Positive := 1;
      A_Length, B_Length, C_Length : Natural := 0;
      --  The index after the block.
      Next : Positive := 1;
   end record;

   --  The block at Section (At_Index): OK False when it is not a whole,
   --  well-formed block (unknown role or kind, fields past the end).
   procedure Read (Section : String; At_Index : Positive; View : out Block_View; OK : out Boolean)
   with Pre => Section'Length <= Maximum_Section and then At_Index in Section'Range and then
               Section'Last < Positive'Last - P.Block_Header_Bytes,
        Post => (if OK then View.Next in At_Index + 1 .. Section'Last + 1 and then
                 View.A_First + View.A_Length - 1 <= Section'Last and then
                 View.B_First + View.B_Length - 1 <= Section'Last and then
                 View.C_First + View.C_Length - 1 <= Section'Last and then
                 View.A_First >= Section'First and then View.B_First >= Section'First and then
                 View.C_First >= Section'First);
end Agent_Blocks;
