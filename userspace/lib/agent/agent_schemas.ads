------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A tool's input schema (JSON Schema, as the Messages API's input_schema
--  takes it) generated from the CCL signature of the operation it is
--  (AGENT-002, docs/agent-harness.md "Tools are typed CCL operations").
--
--  @description
--  The schema says exactly what Agent_Decoders accepts, so the model is
--  told the type the call will be checked against:
--    Integer                 {"type":"integer"}
--    a range type            {"type":"integer","minimum":L,"maximum":H}
--    Boolean                 {"type":"boolean"}
--    Character               {"type":"string","minLength":1,"maxLength":1}
--    String                  {"type":"string","maxLength":N}
--    a record                {"type":"object","properties":{...},
--                             "required":[fields without a default],
--                             "additionalProperties":false}
--    an enum                 {"type":"string","enum":["A","B",...]}
--    a variant with payloads {"type":"object","properties":{"A":...},
--                             "minProperties":1,"maxProperties":1,
--                             "additionalProperties":false}
--                            (a member without a payload is {"type":"null"})
--    List<T>                 {"type":"array","items":T}
--  Functions, handlers, resources, streams and tasks are not data a model
--  may send: an operation that takes one is Unsupported, so it is never a
--  tool. A scalar parameter becomes an object with one property named by
--  the tool's label; an operation without a parameter takes the empty
--  object. Generation is deterministic (declaration order), so the tool
--  list is byte-identical from one request to the next and stays cached.
------------------------------------------------------------------------------
pragma Ada_2022;
with CCL.Types;

package Agent_Schemas with SPARK_Mode is

   --  The longest schema (named ceiling; over it is Too_Large).
   Maximum_Schema : constant := 8_192;
   Maximum_Depth : constant := 16;

   type Parameter_Kind is (No_Parameter, Integer_Parameter, Boolean_Parameter, Text_Parameter,
                           Typed_Parameter);
   --  An operation's parameter, as its CCL import declares it.
   type Parameter is record
      Kind : Parameter_Kind := No_Parameter;
      --  Text_Parameter: the longest text it takes.
      Text_Limit : Natural := 0;
      --  Typed_Parameter: its type in Types.
      Root : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
   end record;

   type Schema_Result is (Generated, Unsupported, Too_Large);

   --  The longest string a typed value's text may hold (CCL.Objects'
   --  text bytes).
   Typed_Text_Limit : constant := 8_192;

   procedure Input_Schema
     (Types : CCL.Types.Registry; Item : Parameter; Label : String;
      Into : out String; Last : out Natural; Result : out Schema_Result)
   with Pre => Into'First = 1 and then Into'Length = Maximum_Schema and then
               Label'Length in 1 .. CCL.Types.Maximum_Name_Length,
        Post => Last <= Into'Last;
end Agent_Schemas;
