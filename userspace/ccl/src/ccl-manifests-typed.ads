--  Manifests as typed CCL values (docs/ccl-typed-manifests.md): the source
--  is an expression of type Executable_Manifest, checked by the CCL type
--  checker against the CCL declarations in interfaces/executable-manifest.ccl, then read
--  by field and alternative name into the declaration
--  that CCL.Manifests.Encoding writes. The rules the type cannot state yet
--  (names, paths, addresses, bounds) are CCL.Manifests.Model's, shared with
--  the keyword reader, so both accept exactly the same declarations.
package CCL.Manifests.Typed with SPARK_Mode => On is
   --  Schema_Source: interfaces/executable-manifest.ccl, the CCL declarations of
   --  Executable_Manifest and its constructors.
   procedure Compile (Source, Catalog_Source, Schema_Source : String; Result : out Compilation_Result);
end CCL.Manifests.Typed;
