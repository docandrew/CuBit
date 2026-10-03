with CCL.Manifests.Model;

--  Capability slot assignment and the ELF sections (.cubit.id, .cubit.caps,
--  .cubit.access, .cubit.streams, .cubit.resources) for a checked
--  declaration. One encoder for every manifest frontend.
package CCL.Manifests.Encoding with SPARK_Mode => On is
   --  Position: where a failure found here is reported.
   procedure Encode
     (Decl : in out Model.Declaration; Catalog : Model.Catalog_Model;
      Position : Natural; Result : in out Compilation_Result);
end CCL.Manifests.Encoding;
