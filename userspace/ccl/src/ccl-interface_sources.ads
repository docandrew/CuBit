with CCL.Types;

--  Interfaces whose types are CCL source (docs/ccl-typed-manifests.md): the
--  source is checked by the CCL type checker, then its declarations are
--  imported into a host's registry. No Ada description of the types exists.
package CCL.Interface_Sources with SPARK_Mode is
   --  Check Source (type declarations only) and import every type it
   --  declares, in order, into Types. Names Types already holds must be
   --  identical definitions (CCL.Types.Import_Definition).
   procedure Declare_Types (Source : String; Types : in out CCL.Types.Registry; Accepted : out Boolean);
   --  A type by name (Invalid_Type when absent).
   function Named_Type (Types : CCL.Types.Registry; Name : String) return CCL.Types.Type_Reference is
     (CCL.Types.Find (Types, CCL.Types.Named (Name)));
end CCL.Interface_Sources;
