with CCL.Types;
with CuBit.Parameter_Signatures;

--  A program's typed parameters (docs/ccl-launch-parameters.md): the rules
--  a manifest's parameter record must meet beyond the CCL type check, and
--  its canonical signature (CuBit.Parameter_Signatures), rendered from the
--  checked types, so the same signature is the same text however the
--  manifest spelled it.
--
--  The record and every type it reaches must be declared by the manifest
--  itself (not the manifest schema), and be records, variants, enumerations,
--  range types, Boolean or String: no bare Integer (a parameter's numbers
--  are range types with their bounds) and no lists. A field whose value is,
--  or can hold, a number has no default: limits, capacities and deadlines
--  are always given by the launcher (explicit-not-default limits). Every
--  name must be usable as an Ada name in the generated binding.
--
--  The signature is the declarations in dependency order, the record last:
--    (type N (range Low High))
--    (type N (enum A B))
--    (type N (variant (A Payload) (B)))
--    (type N (record (field Type) (flag Boolean false) (mode Mode Mode.Fast)))
--  separated by single spaces.
package CCL.Parameter_Signatures is
   Maximum_Signature_Bytes : constant := CuBit.Parameter_Signatures.Maximum_Signature_Bytes;

   type Refusal is
     (Accepted,
      --  The named root is not a type, or not a record.
      Unknown_Type, Not_A_Record,
      --  A type the manifest schema declares, not the manifest.
      Schema_Type,
      --  A list, function, stream or other type a parameter cannot hold.
      Unsupported_Type,
      --  An Integer field or payload: use a range type.
      Bare_Integer,
      --  A default on a field whose value is, or can hold, a number.
      Defaulted_Number,
      --  A type, field or alternative name the Ada binding cannot use.
      Unusable_Name,
      --  The signature is longer than Maximum_Signature_Bytes.
      Too_Long);

   type Result is record
      Why : Refusal := Accepted;
      --  The type the refusal is about, and for a field the field.
      Culprit_Type, Culprit_Field : CCL.Types.Name;
      Text : String (1 .. Maximum_Signature_Bytes) := [others => ' '];
      Length : Natural range 0 .. Maximum_Signature_Bytes := 0;
   end record;

   --  Check Root (by name) in Types and render its signature. Types before
   --  First_Own are the manifest schema's.
   procedure Render
     (Types : CCL.Types.Registry; Root_Name : String; First_Own : CCL.Types.Type_Reference;
      Item : out Result);

   --  Whether Name can be an Ada identifier in a generated decoder or
   --  binding: not a reserved word, nor a name the generated code itself
   --  declares or uses unqualified (Choice, Text, R, Interfaces, CCL,
   --  CuBit, Decode, Boolean, Value, Identity, Parameters).
   function Ada_Usable (Name : String) return Boolean;
end CCL.Parameter_Signatures;
