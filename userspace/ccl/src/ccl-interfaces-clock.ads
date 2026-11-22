with CCL.Catalog;

package CCL.Interfaces.Clock with
   SPARK_Mode => On
is

   --  What Publish adds to a catalog: one interface and this many data
   --  schemas. CCL_Host_Environment sums these to check the catalog's
   --  capacity at compile time; tests/ccl-install's probe checks a real run.
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 0;
   --  Declared types Publish adds to the catalog's registry (its named
   --  types and the list types it specializes).
   PUBLISHED_TYPES : constant := 0;
   DESCRIPTOR_DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#7DEA_1745_99CE_1FB1#,
      16#A09C_B4F8_1B3D_E54C#,
      16#7E67_C846_742B_4022#,
      16#F76C_6038_9B96_78D2#];

   --  Publish Clock's immutable public description.  This grants discovery
   --  only: the caller must separately install an authorized local binding
   --  before a compiled program can be linked.
   procedure Publish
     (Item  : in out CCL.Catalog.Interface_Catalog;
      Error : out CCL.Catalog.Catalog_Error);

   procedure Resolve_Monotonic_Ms
     (Item   : CCL.Catalog.Interface_Catalog;
      Result : out CCL.Catalog.Resolved_Operation;
      Found  : out Boolean);
end CCL.Interfaces.Clock;
