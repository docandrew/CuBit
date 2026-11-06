------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A JSON syntax check (RFC 8259) for text that a request embeds as JSON
--  rather than as an escaped string: a tool call's input and a tool's input
--  schema (AGENT-002). Pure: no I/O, no allocation.
--
--  @description
--  A value that passes is exactly one complete JSON value, so it cannot
--  close the object it is embedded in and add members of its own (a
--  "max_tokens" after a tool's input, say). Strings may hold any byte at or
--  above 16#20# (the provider checks the UTF-8); control bytes must be
--  escaped. Nesting is bounded by Maximum_Depth.
------------------------------------------------------------------------------
pragma Ada_2022;

package Llm_Json with Pure, SPARK_Mode is

   Maximum_Depth : constant := 64;
   --  The longest text checked (more than any request holds).
   Maximum_Text : constant := 2**24;

   --  Text is one JSON value, with only whitespace around it.
   function Valid_Value (Text : String) return Boolean
   with Pre => Text'Length <= Maximum_Text;

   --  Text is one JSON object.
   function Valid_Object (Text : String) return Boolean
   with Pre => Text'Length <= Maximum_Text;
end Llm_Json;
