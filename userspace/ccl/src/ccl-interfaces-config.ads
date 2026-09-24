with CCL.Catalog;
package CCL.Interfaces.Config with SPARK_Mode is
   type Operation is (Get, Keys);
   function Name (Op : Operation) return String is
     (case Op is when Get => "get", when Keys => "keys");
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Config;
