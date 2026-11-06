with CCL.Host_Values;
with CCL.VM;
with CCL.Types;
with CCL.Objects.Catalog;
with CCL.Interface_Sources;

package body CCL.Interfaces.Llm with SPARK_Mode is
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
      --  Asking spends money and reaches the network: control. Reading an
      --  exchange's text or reply observes it; cancelling acts on it.
      CONTRACTS_OF : constant array (Operation) of CCL.Host_Values.Import_Declaration :=
        [Ask =>
           (Argument => CCL.Host_Values.Object_Value, Argument_Schema => REQUEST_KEY,
            Result => CCL.Host_Values.Object_Value, Result_Schema => EXCHANGE_KEY,
            Authority => CCL.VM.Control_Authority, others => <>),
         Text =>
           (Argument => CCL.Host_Values.Object_Value, Argument_Schema => EXCHANGE_KEY,
            Result => CCL.Host_Values.Text_Value, Result_Stream => True,
            Authority => CCL.VM.Observe_Authority, others => <>),
         Reply =>
           (Argument => CCL.Host_Values.Object_Value, Argument_Schema => EXCHANGE_KEY,
            Result => CCL.Host_Values.Object_Value, Result_Schema => REPLY_KEY, Result_Task => True,
            Authority => CCL.VM.Observe_Authority, others => <>),
         Cancel =>
           (Argument => CCL.Host_Values.Object_Value, Argument_Schema => EXCHANGE_KEY,
            Result => CCL.Host_Values.Object_Value, Result_Schema => EXCHANGE_KEY,
            Authority => CCL.VM.Control_Authority, others => <>)];
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then CCL.Objects.Bind (Types, Named ("Llm_Request"), REQUEST_KEY, Bound.Request, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Llm_Exchange"), EXCHANGE_KEY, Bound.Exchange, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Llm_Reply"), REPLY_KEY, Bound.Reply, Accepted); end if;
      Publish_Schema (Bound.Request);
      Publish_Schema (Bound.Exchange);
      Publish_Schema (Bound.Reply);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("llm", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation (Name (Op), 1, CONTRACTS_OF (Op), Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;
end CCL.Interfaces.Llm;
