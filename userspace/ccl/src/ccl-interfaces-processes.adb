with CCL.Host_Values;
with CCL.Language;
with CCL.Objects.Catalog;
with CCL.VM;

package body CCL.Interfaces.Processes with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Language.Analysis_Status;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean) is
      Checked : CCL.Language.Analysis_Result;
      Types : CCL.Types.Registry;
      Process_Type, List_Type : CCL.Types.Type_Reference;
      Listed : CCL.Types.List_Result;
   begin
      Bound := (others => <>);
      --  The declarations, then a trivial expression: one checked program.
      CCL.Language.Analyze (TYPE_SOURCE & "0", Checked);
      Accepted := CCL.Language.Analysis_Status_Of (Checked) = CCL.Language.Analysis_Succeeded;
      if not Accepted then return; end if;
      Types := CCL.Language.Analysis_Types (Checked);
      Process_Type := CCL.Types.Find (Types, CCL.Types.Named ("Process"));
      CCL.Types.Specialize_List (Types, Process_Type, List_Type, Listed);
      Accepted := Listed in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      if Accepted then CCL.Objects.Bind (Types, Process_Type, PROCESS_KEY, Bound.Process, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, List_Type, PROCESSES_KEY, Bound.Processes, Accepted); end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Bound : Contracts;
      Accepted : Boolean;
      Result : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      --  Listing what runs observes the system; it takes no argument.
      LIST_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Integer_Value,
         Result => CCL.Host_Values.Object_Value, Result_Schema => PROCESSES_KEY,
         Authority => CCL.VM.Observe_Authority, others => <>);
      procedure Publish_Schema (Contract : CCL.Objects.Binding) is
      begin
         if Accepted then
            CCL.Catalog.Publish_Schema (Item, Contract, Result);
            Accepted := Result in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published;
         end if;
      end Publish_Schema;
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Bound, Accepted);
      Publish_Schema (Bound.Process);
      Publish_Schema (Bound.Processes);
      if not Accepted then return; end if;
      CCL.Catalog.Define_Interface ("proc", 1, 0, DIGEST, Descriptor, Error);
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Define_Host_Operation (Name (List), 0, LIST_CONTRACT, Entry_Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;
end CCL.Interfaces.Processes;
