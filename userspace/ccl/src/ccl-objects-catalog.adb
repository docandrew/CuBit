with CCL.Types.Correspondence;

package body CCL.Objects.Catalog with SPARK_Mode is
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Import_Result;

   procedure Publish_Type
     (Item : in out Schema_Catalog; Source : CCL.Types.Registry;
      Root : CCL.Types.Type_Reference; Ref : out CCL.Types.Type_Reference;
      Result : out CCL.Types.Import_Result) is
   begin
      CCL.Types.Import_Definition (Source, Root, Item.Types, Ref, Result);
   end Publish_Type;

   function Root_Of (Item : Schema_Catalog; Key : Schema_Key) return CCL.Types.Type_Reference is
   begin
      for I in 1 .. Length (Item) loop
         if Entry_Tables.Get (Item.Entries, I).Key = Key then return Entry_Tables.Get (Item.Entries, I).Root; end if;
      end loop;
      return CCL.Types.Invalid_Type;
   end Root_Of;

   procedure Publish
     (Item : in out Schema_Catalog; Contract : Binding;
      Result : out Publication_Result)
   is
      Types : CCL.Types.Registry := Item.Types;
      Entries : Entry_Tables.Table := Item.Entries;
      Added : Boolean;
      Ref : CCL.Types.Type_Reference;
      Imported_As : CCL.Types.Import_Result;
   begin
      if not Is_Bound (Contract) then
         Result := Unbound_Contract;
         return;
      end if;
      for I in 1 .. Length (Item) loop
         if Entry_Tables.Get (Item.Entries, I).Key = Contract.Key then
            Ref := CCL.Types.Correspondence.Resolve
              (Contract.Types, Contract.Root, Item.Types);
            Result := (if Ref = Entry_Tables.Get (Item.Entries, I).Root then Already_Published
                       else Identity_Conflict);
            return;
         end if;
      end loop;
      CCL.Types.Import_Definition
        (Contract.Types, Contract.Root, Types, Ref, Imported_As);
      if Imported_As /= CCL.Types.Imported then
         Result := (if Imported_As = CCL.Types.Import_Full then Capacity_Exhausted
                    else Definition_Conflict);
         return;
      end if;
      --  All fallible work precedes publication; one shared registry stores
      --  the dependency closure, not one registry/image per import or value.
      Entry_Tables.Append (Entries, (Contract.Key, Ref), Added);
      if not Added then
         Result := Capacity_Exhausted;
         return;
      end if;
      Item.Types := Types;
      Item.Entries := Entries;
      Result := Published;
   end Publish;

   procedure Resolve
     (Item : Schema_Catalog; Key : Schema_Key; Contract : out Binding)
   is
   begin
      Contract := (others => <>);
      for I in 1 .. Length (Item) loop
         if Entry_Tables.Get (Item.Entries, I).Key = Key then
            --  Publish accepts only bound persistable roots, then translates
            --  their full closure into this append-only registry.
            Contract := (Types => Item.Types, Root => Entry_Tables.Get (Item.Entries, I).Root,
                         Key => Key, Bound => True);
            return;
         end if;
      end loop;
   end Resolve;
end CCL.Objects.Catalog;
