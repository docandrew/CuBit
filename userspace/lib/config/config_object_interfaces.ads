with CCL.Catalog;
with CCL.Objects;
with CCL.Types;
with Config_Read_Outcomes;

-- Shared descriptor construction for native, embedded and remote CCL hosts.
-- The caller must already be authorized to discover these definitions and
-- must authenticate/pin their provider. Publication grants no invocation right.
package Config_Object_Interfaces with SPARK_Mode is
   type Operation is (Open_Collection, Read_Value, Write_Value, Close_Collection);
   function Name (Action : Operation) return String is
     (case Action is when Open_Collection => "open", when Read_Value => "read",
        when Write_Value => "write", when Close_Collection => "close");

   procedure Publish
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Alias : String; Interface_Key : CCL.Catalog.Descriptor_Digest;
      Contract : CCL.Objects.Binding;
      Read_Description : Config_Read_Outcomes.Description;
      Resource_Kind : out CCL.Types.Type_Reference;
      Accepted : out Boolean);
   -- A pinned collection of T exposes open() -> ConfigCollection<T>,
   -- read(receiver) -> ConfigRead<T>, write(receiver, T) -> ConfigWrite,
   -- close(receiver) -> Integer. The endpoint/name/access scope stay in the
   -- host's granted binding, never in caller-selected metadata. Open is the
   -- existing durable create-or-open operation, not an arbitrary path lookup.
   -- All changes are transactional: failure leaves Catalog unchanged.
   -- This describes one approved specialization, not live service discovery
   -- or general source-level type-argument inference.
end Config_Object_Interfaces;
