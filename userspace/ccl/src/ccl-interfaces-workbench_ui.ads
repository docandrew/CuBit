with CCL.Catalog;
package CCL.Interfaces.Workbench_UI with SPARK_Mode is
   -- SHA-256 of interfaces/workbench-ui.ccl-interface, not a provider signature.
   Descriptor_Digest : constant CCL.Catalog.Descriptor_Digest :=
     [16#B1DCDC35BA6D2D2C#, 16#74CEA54DEC98337B#,
      16#17B68AE652C7BB13#, 16#D6F5C31A245F4248#];
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Workbench_UI;
