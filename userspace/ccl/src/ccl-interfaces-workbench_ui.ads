with CCL.Catalog;
package CCL.Interfaces.Workbench_UI with SPARK_Mode is
   -- SHA-256 of interfaces/workbench-ui.ccl-interface, not a provider signature.
   Descriptor_Digest : constant CCL.Catalog.Descriptor_Digest :=
     [16#2785B17C8606FACF#, 16#9359BD004648C430#,
      16#ACE52BC051E3BA88#, 16#E4F9AA67B04E97AF#];
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Workbench_UI;
