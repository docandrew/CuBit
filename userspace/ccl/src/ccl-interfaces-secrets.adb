with CCL.Host_Values;
with CCL.VM;
with CCL.Types;
with CCL.Objects.Catalog;
with CCL.Interface_Sources;

package body CCL.Interfaces.Secrets with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Types : CCL.Types.Registry := CCL.Catalog.Visible_Types (Item);
      Bound : Contracts := (others => <>);
      Accepted : Boolean;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
      procedure Publish_Schema (Contract : CCL.Objects.Binding) is
         Result : CCL.Objects.Catalog.Publication_Result;
      begin
         if Accepted then
            CCL.Catalog.Publish_Schema (Item, Contract, Result);
            Accepted := Result in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published;
         end if;
      end Publish_Schema;
      --  Storing, forgetting and revoking change the store; status observes.
      function Contract_Of (Op : Operation) return CCL.Host_Values.Import_Declaration is
        ((Argument => CCL.Host_Values.Object_Value, Argument_Schema => SECRET_KEY,
          Result => CCL.Host_Values.Object_Value,
          Result_Schema => (if Op in Set | Forget then SECRET_KEY else STATE_KEY),
          Authority => (if Op = Status then CCL.VM.Observe_Authority else CCL.VM.Control_Authority),
          others => <>));
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then CCL.Objects.Bind (Types, Named ("Secret"), SECRET_KEY, Bound.Secret, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Secret_State"), STATE_KEY, Bound.State, Accepted); end if;
      Publish_Schema (Bound.Secret);
      Publish_Schema (Bound.State);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("secrets", 1, 0, DIGEST, Descriptor, Error);
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
end CCL.Interfaces.Secrets;
