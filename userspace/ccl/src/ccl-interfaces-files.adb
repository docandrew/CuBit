with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;

with CCL.Interface_Sources;

package body CCL.Interfaces.Files with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   KIND_MEMBERS : constant := File_Kind'Pos (File_Kind'Last) + 1;

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean)
   is
      Specialized : CCL.Types.List_Result;
      Listing_Type : CCL.Types.Type_Reference;
      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
   begin
      Bound := (others => <>);
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then
         CCL.Types.Specialize_List (Types, Named ("File_Metadata"), Listing_Type, Specialized);
         Accepted := Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("File_Kind"), KIND_KEY, Bound.Kind, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Place"), PLACE_KEY, Bound.Place, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Child"), CHILD_KEY, Bound.Child, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("File_Metadata"), METADATA_KEY, Bound.Metadata, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Listing_Type, LISTING_KEY, Bound.Listing, Accepted); end if;
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
      function Contract_Of (Op : Operation) return CCL.Host_Values.Import_Declaration is
        (case Op is
            --  Reading the process's own workspace place.
            when Home =>
              (Argument => CCL.Host_Values.Integer_Value,
               Result => CCL.Host_Values.Object_Value, Result_Schema => PLACE_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>),
            --  Naming a child or the parent grants nothing; listing is checked.
            when Up =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => PLACE_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => PLACE_KEY,
               Authority => CCL.VM.No_Authority, others => <>),
            when Enter =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => CHILD_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => PLACE_KEY,
               Authority => CCL.VM.No_Authority, others => <>),
            when List =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => PLACE_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => LISTING_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>));
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Bound, Accepted);
      Publish_Schema (Bound.Kind);
      Publish_Schema (Bound.Place);
      Publish_Schema (Bound.Child);
      Publish_Schema (Bound.Metadata);
      Publish_Schema (Bound.Listing);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("fs", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation
           (Name (Op), (if Op = Home then 0 else 1), Contract_Of (Op), Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;
end CCL.Interfaces.Files;
