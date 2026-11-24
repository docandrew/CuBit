--  Host-owned approved schema descriptions, not invocation authority. Keys
--  come from validated discovery/config bindings; this package neither hashes
--  descriptions nor authenticates their publishers. The catalog is append-only
--  so a live key can never silently change the meaning of an existing object.
private with CCL.Growing_Tables;

package CCL.Objects.Catalog with SPARK_Mode is
   --  Schemas one catalog may hold: a named ceiling, not a size. The
   --  entries grow from what is published (CCL.Growing_Tables), as the
   --  type registry they share does; past the ceiling, or when memory runs
   --  out, publication is refused (Capacity_Exhausted). Checked against
   --  CCL_Host_Environment.MOST_SCHEMAS at compile time. 4096: Resolve and
   --  Publish search the keys linearly.
   Schema_Ceiling : constant := 4_096;
   subtype Schema_Count is Natural range 0 .. Schema_Ceiling;
   type Schema_Catalog is private with Default_Initial_Condition => Length (Schema_Catalog) = 0;
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
   type Entry_Description is record
      Key : Schema_Key := No_Schema;
      Root : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
   end record;
   package Entry_Tables is new CCL.Growing_Tables (Element => Entry_Description, Ceiling => Schema_Ceiling);
   type Schema_Catalog is record
      Types : CCL.Types.Registry;
      Entries : Entry_Tables.Table;
   end record;
   function Length (Item : Schema_Catalog) return Schema_Count is (Entry_Tables.Length (Item.Entries));
   function Visible_Types (Item : Schema_Catalog) return CCL.Types.Registry is (Item.Types);
end CCL.Objects.Catalog;
