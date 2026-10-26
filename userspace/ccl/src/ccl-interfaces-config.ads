with CCL.Catalog;
package CCL.Interfaces.Config with SPARK_Mode is

   --  What Publish adds to a catalog: one interface and this many data
   --  schemas. CCL_Host_Environment sums these to check the catalog's
   --  capacity at compile time; tests/ccl-install's probe checks a real run.
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 0;
   type Operation is (Get, Keys);
   function Name (Op : Operation) return String is
     (case Op is when Get => "get", when Keys => "keys");
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Config;
