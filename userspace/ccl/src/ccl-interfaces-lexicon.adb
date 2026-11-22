with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;

with CCL.Interface_Sources;

package body CCL.Interfaces.Lexicon with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean)
   is
      Specialized : CCL.Types.List_Result;
      Misspellings_Type : CCL.Types.Type_Reference;
      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
   begin
      Bound := (others => <>);
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then
         CCL.Types.Specialize_List (Types, Named ("Misspelling"), Misspellings_Type, Specialized);
         Accepted := Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Definition"), DEFINITION_KEY, Bound.Definition, Accepted); end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("Thesaurus_Result"), THESAURUS_KEY, Bound.Thesaurus, Accepted);
      end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Synonyms"), SYNONYMS_KEY, Bound.Synonyms, Accepted); end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("Near_Spellings"), SPELLINGS_KEY, Bound.Spellings, Accepted);
      end if;
      if Accepted then
         CCL.Objects.Bind (Types, Misspellings_Type, MISSPELLINGS_KEY, Bound.Misspellings, Accepted);
      end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Page_Query"), PAGE_KEY, Bound.Page, Accepted); end if;
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
      --  A word or a text in, a typed value out. Asking the lexicon service
      --  observes nothing of the system and grants nothing; Observe is the
      --  class of a read from a service.
      function Word_To (Result : CCL.Objects.Schema_Key) return CCL.Host_Values.Import_Declaration is
        ((Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAXIMUM_WORD,
          Result => CCL.Host_Values.Object_Value, Result_Schema => Result,
          Authority => CCL.VM.Observe_Authority, others => <>));
      function Page_To (Result : CCL.Objects.Schema_Key) return CCL.Host_Values.Import_Declaration is
        ((Argument => CCL.Host_Values.Object_Value, Argument_Schema => PAGE_KEY,
          Result => CCL.Host_Values.Object_Value, Result_Schema => Result,
          Authority => CCL.VM.Observe_Authority, others => <>));
      function Contract_Of (Op : Operation) return CCL.Host_Values.Import_Declaration is
        (case Op is
            when Dict => Word_To (DEFINITION_KEY),
            when Synonym => Word_To (SYNONYMS_KEY),
            when Thesaurus => Word_To (THESAURUS_KEY),
            when Suggest => Word_To (SPELLINGS_KEY),
            when Spell_Check =>
              (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAXIMUM_TEXT,
               Result => CCL.Host_Values.Object_Value, Result_Schema => MISSPELLINGS_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>),
            when Known =>
              (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAXIMUM_WORD,
               Result => CCL.Host_Values.Boolean_Value,
               Authority => CCL.VM.Observe_Authority, others => <>),
            when Dict_Page => Page_To (DEFINITION_KEY),
            when Thesaurus_Page => Page_To (THESAURUS_KEY));
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Bound, Accepted);
      Publish_Schema (Bound.Definition);
      Publish_Schema (Bound.Thesaurus);
      Publish_Schema (Bound.Synonyms);
      Publish_Schema (Bound.Spellings);
      Publish_Schema (Bound.Misspellings);
      Publish_Schema (Bound.Page);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("lexicon", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation (Name (Op), 1, Contract_Of (Op), Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid and then Is_Command (Op) then
            CCL.Catalog.Mark_Command (Entry_Operation);
         end if;
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;
end CCL.Interfaces.Lexicon;
