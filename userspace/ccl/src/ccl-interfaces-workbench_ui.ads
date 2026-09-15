with CCL.Catalog;
package CCL.Interfaces.Workbench_UI with SPARK_Mode is
   -- SHA-256 of interfaces/workbench-ui.ccl-interface, not a provider signature.
   Descriptor_Digest : constant CCL.Catalog.Descriptor_Digest :=
     [16#F97581518FB34922#, 16#A626B0E9D3EDEC18#,
      16#D5CD3F018FE10FCC#, 16#668DFF6AAF7B9A0E#];
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Workbench_UI;
