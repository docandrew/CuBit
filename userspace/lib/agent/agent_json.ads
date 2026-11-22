------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  JSON text (RFC 8259) parsed into an index of its values, for decoding a
--  model's tool-call input into a typed CCL value (Agent_Decoders,
--  AGENT-002). Pure: no I/O, no allocation.
--
--  @description
--  The text is never trusted: every byte is checked against the grammar,
--  nesting is bounded by Maximum_Depth and the values by Maximum_Nodes (a
--  typed Too_Deep or Too_Large, never a cut). Each node records its kind,
--  its bytes in the text, and the node after its subtree, so a decoder
--  walks an object's members without re-parsing. An object's children are
--  its keys and values in turn.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;

package Agent_Json with Pure, SPARK_Mode is

   --  Named ceilings: the longest tool-call input, its values, its nesting.
   Maximum_Input : constant := 16_384;
   Maximum_Nodes : constant := 2_048;
   Maximum_Depth : constant := 16;

   subtype Node_Count is Natural range 0 .. Maximum_Nodes;
   subtype Node_Index is Positive range 1 .. Maximum_Nodes;

   type Node_Kind is
     (Object_Node, Array_Node, String_Node, Number_Node, True_Node, False_Node, Null_Node);

   type Node is record
      Kind : Node_Kind := Null_Node;
      --  Its bytes in the text (a string's quotes included).
      First, Last : Natural := 0;
      --  The node after this one's subtree (Count + 1 at the end).
      Next : Positive := 1;
      --  Members: an object's key/value pairs, an array's elements.
      Members : Node_Count := 0;
   end record;
   type Node_Array is array (Node_Index) of Node;

   type Document is record
      Count : Node_Count := 0;
      Nodes : Node_Array := [others => (others => <>)];
   end record;

   type Parse_Result is (Parsed, Not_Json, Too_Deep, Too_Large);

   --  Text as one JSON value (whitespace around it allowed).
   procedure Parse (Text : String; Doc : out Document; Result : out Parse_Result)
   with Pre => Text'Length <= Maximum_Input and then Text'Last < Positive'Last,
        Post => (if Result = Parsed then Doc.Count >= 1 and then
                   (for all I in 1 .. Doc.Count =>
                      Doc.Nodes (I).Next in I + 1 .. Doc.Count + 1 and then
                      Doc.Nodes (I).First in Text'Range and then
                      Doc.Nodes (I).Last in Doc.Nodes (I).First .. Text'Last and then
                      (if Doc.Nodes (I).Kind = String_Node then
                         Doc.Nodes (I).Last > Doc.Nodes (I).First and then
                         Text (Doc.Nodes (I).First) = '"' and then Text (Doc.Nodes (I).Last) = '"')));

   --  A string node's content, unescaped (\uXXXX as UTF-8, a lone surrogate
   --  as U+FFFD). OK False when it does not fit Into.
   procedure Decode_String
     (Text : String; Item : Node; Into : out String; Last : out Natural; OK : out Boolean)
   with Pre => Item.Kind = String_Node and then Item.First in Text'Range and then
               Item.Last in Text'Range and then Item.Last > Item.First and then
               Into'First = 1 and then Into'Length >= 1 and then Into'Last < Positive'Last,
        Post => Last <= Into'Last;

   --  A number node as an integer: no fraction or exponent, within
   --  Integer_64.
   procedure Integer_Value
     (Text : String; Item : Node; Value : out Interfaces.Integer_64; OK : out Boolean)
   with Pre => Item.Kind = Number_Node and then Item.First in Text'Range and then
               Item.Last in Item.First .. Text'Last and then Text'Last < Positive'Last;
end Agent_Json;
