--  Host-owned approved schema descriptions, not invocation authority. Keys
--  come from validated discovery/config bindings; this package neither hashes
--  descriptions nor authenticates their publishers. The catalog is append-only
--  so a live key can never silently change the meaning of an existing object.
package CCL.Objects.Catalog with SPARK_Mode is
   Maximum_Schemas : constant := 32;
   subtype Schema_Count is Natural range 0 .. Maximum_Schemas;
   type Schema_Catalog is private;
   type Publication_Result is
     (Published, Already_Published, Unbound_Contract, Identity_Conflict,
      Definition_Conflict, Capacity_Exhausted);

   function Length (Item : Schema_Catalog) return Schema_Count;
   function Visible_Types (Item : Schema_Catalog) return CCL.Types.Registry;
   -- Publish a visible nominal definition without assigning it a schema key.
   -- Both paths use the same append-only registry, so existing roots stay stable.
   procedure Publish_Type
     (Item : in out Schema_Catalog; Source : CCL.Types.Registry;
      Root : CCL.Types.Type_Reference; Ref : out CCL.Types.Type_Reference;
      Result : out CCL.Types.Import_Result)
     with Global => null;
   function Root_Of (Item : Schema_Catalog; Key : Schema_Key) return CCL.Types.Type_Reference;
   procedure Publish
     (Item : in out Schema_Catalog; Contract : Binding;
      Result : out Publication_Result)
   with Global => null,
     Post => (if Result = Published then Length (Item) = Length (Item'Old) + 1
              else Item = Item'Old);

   --  Missing keys return an unbound contract. This returns metadata only;
   --  endpoints, Config handles, discovery policy and grants stay with the host.
   procedure Resolve
     (Item : Schema_Catalog; Key : Schema_Key; Contract : out Binding)
   with Global => null,
     Post => (if Is_Bound (Contract) then Identity (Contract) = Key);
private
   subtype Schema_Index is Natural range 0 .. Maximum_Schemas - 1;
   type Entry_Description is record
      Key : Schema_Key := No_Schema;
      Root : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
   end record;
   type Entry_Array is array (Schema_Index) of Entry_Description;
   type Schema_Catalog is record
      Count : Schema_Count := 0;
      Types : CCL.Types.Registry;
      Entries : Entry_Array := [others => <>];
   end record;
   function Length (Item : Schema_Catalog) return Schema_Count is (Item.Count);
   function Visible_Types (Item : Schema_Catalog) return CCL.Types.Registry is (Item.Types);
end CCL.Objects.Catalog;
