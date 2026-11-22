------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A model's tool-call input, decoded into the typed parameter of the CCL
--  operation the tool is (AGENT-002): the endorser between untrusted model
--  output and the operations a mission grants.
--
--  @description
--  Model output is untrusted (docs/ccl-labelled-types.md, CCL-008, which
--  will make this a type label). Until CCL has the label, Untrusted_Input
--  is a private type: what a model produced can be held and logged by size,
--  but the only way to its content is Decode, whose result is a typed value
--  (a CCL object image, a text or an integer), never text that is "still
--  text". Decode accepts exactly the schema Agent_Schemas generates:
--    - an object's members are the record's fields: a missing field takes
--      its declared default or is Missing_Field, an unknown member is
--      Unknown_Field, a repeated one Duplicate_Field;
--    - numbers are integers within the field's range (no fraction, no
--      exponent, no rounding), strings fit their bound, an enum member is
--      named exactly;
--    - nothing is coerced: a number where a string is expected is
--      Wrong_Type, never "42".
--  A malformed call is a typed Decode_Error, which the loop returns to the
--  model as the tool's error result.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with CCL.Types;
with CCL.Objects;
with Agent_Json;
with Agent_Schemas;

package Agent_Decoders with SPARK_Mode is

   type Untrusted_Input is private;

   --  What the model sent as a call's input. Longer than
   --  Agent_Json.Maximum_Input: kept as Oversized (Decode says Too_Large).
   function From_Model (Text : String) return Untrusted_Input;
   function Size_Of (Input : Untrusted_Input) return Natural;

   type Decode_Error is
     (No_Error, Not_Json, Not_An_Object, Too_Deep, Too_Large, Missing_Field, Unknown_Field,
      Duplicate_Field, Wrong_Type, Out_Of_Range, Too_Long, Unknown_Member, Unsupported_Type);

   function Error_Name (Error : Decode_Error) return String is
     (case Error is
         when No_Error => "none",
         when Not_Json => "not JSON",
         when Not_An_Object => "not a JSON object",
         when Too_Deep => "nested too deeply",
         when Too_Large => "too large",
         when Missing_Field => "missing field",
         when Unknown_Field => "unknown field",
         when Duplicate_Field => "duplicate field",
         when Wrong_Type => "wrong type",
         when Out_Of_Range => "out of range",
         when Too_Long => "text too long",
         when Unknown_Member => "unknown member",
         when Unsupported_Type => "unsupported type");

   Maximum_Path : constant := 160;
   type Decode_Result is record
      Error : Decode_Error := No_Error;
      --  Where: the field path, as "place.root".
      Path : String (1 .. Maximum_Path) := [others => ' '];
      Path_Length : Natural range 0 .. Maximum_Path := 0;
   end record;

   --  A typed parameter: the image of a value of Root, bound to Contract.
   procedure Decode_Typed
     (Input : Untrusted_Input; Types : CCL.Types.Registry; Root : CCL.Types.Type_Reference;
      Label : String; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Image; Result : out Decode_Result)
   with Pre => Label'Length <= CCL.Types.Maximum_Name_Length,
        Post => (if Result.Error = No_Error then CCL.Objects.Validate (Image, Contract));

   --  A text parameter (one property, Label), at most Limit bytes.
   procedure Decode_Text
     (Input : Untrusted_Input; Label : String; Limit : Natural;
      Into : out String; Last : out Natural; Result : out Decode_Result)
   with Pre => Label'Length <= CCL.Types.Maximum_Name_Length and then Into'First = 1 and then
               Into'Length >= 1 and then Into'Last < Positive'Last,
        Post => Last <= Into'Last and then (if Result.Error = No_Error then Last <= Limit);

   --  An integer parameter (one property, Label).
   procedure Decode_Integer
     (Input : Untrusted_Input; Label : String;
      Value : out Interfaces.Integer_64; Result : out Decode_Result)
   with Pre => Label'Length <= CCL.Types.Maximum_Name_Length;

   --  No parameter: the empty object.
   procedure Decode_Nothing (Input : Untrusted_Input; Result : out Decode_Result);

   --  The kinds of parameter Decode_* take (Agent_Schemas.Parameter_Kind).
   subtype Parameter_Kind is Agent_Schemas.Parameter_Kind;

private
   type Untrusted_Input is record
      Text : String (1 .. Agent_Json.Maximum_Input) := [others => ' '];
      Length : Natural range 0 .. Agent_Json.Maximum_Input := 0;
      Size : Natural := 0;
      Oversized : Boolean := False;
   end record;
end Agent_Decoders;
