with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;
with CCL.Interface_Sources;

package body CCL.Interfaces.Calc with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean)
   is
      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
   begin
      Bound := (others => <>);
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then CCL.Objects.Bind (Types, Named ("Calc_Form"), FORM_KEY, Bound.Form, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Calc_Number"), NUMBER_KEY, Bound.Number, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Calc_Options"), OPTIONS_KEY, Bound.Options, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Calc_Request"), REQUEST_KEY, Bound.Request, Accepted); end if;
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
      --  Text in (an expression), a Calc_Number out; no authority.
      EVALUATE_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Text_Value,
         Argument_Text_Limit => CCL.Host_Values.Maximum_Text_Length,
         Result => CCL.Host_Values.Object_Value, Result_Schema => NUMBER_KEY,
         Authority => CCL.VM.No_Authority, others => <>);
      --  A Calc_Request in, a Calc_Number out; no authority.
      EVALUATE_WITH_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Object_Value, Argument_Schema => REQUEST_KEY,
         Result => CCL.Host_Values.Object_Value, Result_Schema => NUMBER_KEY,
         Authority => CCL.VM.No_Authority, others => <>);
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Bound, Accepted);
      Publish_Schema (Bound.Form);
      Publish_Schema (Bound.Number);
      Publish_Schema (Bound.Options);
      Publish_Schema (Bound.Request);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("calc", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation
           (Name (Op), 1, (if Op = Evaluate_With then EVALUATE_WITH_CONTRACT else EVALUATE_CONTRACT),
            Entry_Operation, Error);
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
end CCL.Interfaces.Calc;
