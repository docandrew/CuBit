--  Process-local type numbers are not portable identities. Match nominal
--  definitions without mutating either registry or evaluating constructors.
--  This does not authenticate a schema or grant authority to use it.
package CCL.Types.Correspondence with SPARK_Mode is
   function Resolve
     (Source : Registry; Root : Type_Reference; Target : Registry)
      return Type_Reference
   with Global => null, Post =>
     (Resolve'Result = Invalid_Type or else Known (Target, Resolve'Result));
   --  Invalid_Type means absent or conflicting. Require the same nominal name,
   --  product/sum/resource shape, field/alternative/parameter order and names,
   --  and recursively
   --  corresponding payloads. Unrelated declarations may differ. Primitive IDs
   --  are shared; declared IDs need not be. No structural-only duck typing.
end CCL.Types.Correspondence;
