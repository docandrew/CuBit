with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;

with CCL.Interface_Sources;

package body CCL.Interfaces.Bible with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean)
   is
      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
      procedure Bind (Name : String; Key : CCL.Objects.Schema_Key; Contract : in out CCL.Objects.Binding) is
      begin
         if Accepted then
            CCL.Objects.Bind (Types, Named (Name), Key, Contract, Accepted);
         end if;
      end Bind;
   begin
      Bound := (others => <>);
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      Bind ("Parse_Result", PARSE_KEY, Bound.Parse);
      Bind ("Verse_Result", VERSE_KEY, Bound.Verse);
      Bind ("Passage_Result", PASSAGE_RESULT_KEY, Bound.Passage_Result);
      Bind ("Search_Result", SEARCH_KEY, Bound.Search);
      Bind ("Reference", REFERENCE_KEY, Bound.Reference);
      Bind ("Passage", PASSAGE_KEY, Bound.Passage);
      Bind ("Verse_Query", VERSE_QUERY_KEY, Bound.Verse_Query);
      Bind ("Passage_Query", PASSAGE_QUERY_KEY, Bound.Passage_Query);
      Bind ("Search_Query", SEARCH_QUERY_KEY, Bound.Search_Query);
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Types : CCL.Types.Registry := CCL.Catalog.Visible_Types (Item);
      Bound : Contracts;
      Accepted : Boolean;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      procedure Publish_Schema (Contract : CCL.Objects.Binding) is
         Result : CCL.Objects.Catalog.Publication_Result;
      begin
         if Accepted then
            CCL.Catalog.Publish_Schema (Item, Contract, Result);
            Accepted := Result in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published;
         end if;
      end Publish_Schema;
      --  Reading the read-only Bible images observes nothing of the system
      --  and grants nothing; Observe is the class of a read.
      function Text_To (Result : CCL.Objects.Schema_Key) return CCL.Host_Values.Import_Declaration is
        ((Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAXIMUM_TEXT,
          Result => CCL.Host_Values.Object_Value, Result_Schema => Result,
          Authority => CCL.VM.Observe_Authority, others => <>));
      function Object_To (Argument, Result : CCL.Objects.Schema_Key) return CCL.Host_Values.Import_Declaration is
        ((Argument => CCL.Host_Values.Object_Value, Argument_Schema => Argument,
          Result => CCL.Host_Values.Object_Value, Result_Schema => Result,
          Authority => CCL.VM.Observe_Authority, others => <>));
      function Contract_Of (Op : Operation) return CCL.Host_Values.Import_Declaration is
        (case Op is
            when Parse => Text_To (PARSE_KEY),
            when Verse => Object_To (REFERENCE_KEY, VERSE_KEY),
            when Verse_In => Object_To (VERSE_QUERY_KEY, VERSE_KEY),
            when Passage => Object_To (PASSAGE_KEY, PASSAGE_RESULT_KEY),
            when Passage_In => Object_To (PASSAGE_QUERY_KEY, PASSAGE_RESULT_KEY),
            when Read => Text_To (PASSAGE_RESULT_KEY),
            when Search => Text_To (SEARCH_KEY),
            when Search_In => Object_To (SEARCH_QUERY_KEY, SEARCH_KEY));
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Bound, Accepted);
      Publish_Schema (Bound.Parse);
      Publish_Schema (Bound.Verse);
      Publish_Schema (Bound.Passage_Result);
      Publish_Schema (Bound.Search);
      Publish_Schema (Bound.Reference);
      Publish_Schema (Bound.Passage);
      Publish_Schema (Bound.Verse_Query);
      Publish_Schema (Bound.Passage_Query);
      Publish_Schema (Bound.Search_Query);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("bible", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation (Name (Op), 1, Contract_Of (Op), Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;
end CCL.Interfaces.Bible;
