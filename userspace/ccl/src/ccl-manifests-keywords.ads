with CCL.Manifests.Model;

--  Manifests and catalogs in the v1 keyword notation.
package CCL.Manifests.Keywords with SPARK_Mode => On is
   procedure Compile (Source, Catalog_Source : String; Result : out Compilation_Result);
   procedure Read_Catalog
     (Catalog_Source : String; Catalog : out Model.Catalog_Model; Result : out Compilation_Result);
end CCL.Manifests.Keywords;
