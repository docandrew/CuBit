with CCL.Host_Values;
with CCL.VM;
with CCL.Types;
with CCL.Objects.Catalog;
with CCL.Interface_Sources;

package body CCL.Interfaces.Install with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean)
   is
      Specialized : CCL.Types.List_Result;
      List_Type : CCL.Types.Type_Reference;
      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
   begin
      Bound := (others => <>);
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then
         CCL.Types.Specialize_List (Types, Named ("Disk"), List_Type, Specialized);
         Accepted := Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Disk"), DISK_KEY, Bound.Disk, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, List_Type, DISKS_KEY, Bound.Disks, Accepted); end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("Install_Request"), REQUEST_KEY, Bound.Request, Accepted);
      end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("Install_Report"), REPORT_KEY, Bound.Report, Accepted);
      end if;
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
      --  Listing disks observes the machine; installing changes it.
      CONTRACTS_OF : constant array (Operation) of CCL.Host_Values.Import_Declaration :=
        [Disks =>
           (Argument => CCL.Host_Values.Integer_Value,
            Result => CCL.Host_Values.Object_Value, Result_Schema => DISKS_KEY,
            Authority => CCL.VM.Observe_Authority, others => <>),
         Install =>
           (Argument => CCL.Host_Values.Object_Value, Argument_Schema => REQUEST_KEY,
            Result => CCL.Host_Values.Object_Value, Result_Schema => REPORT_KEY,
            Authority => CCL.VM.Control_Authority, others => <>)];
      PARAMETERS : constant array (Operation) of Natural := [Disks => 0, Install => 1];
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Bound, Accepted);
      Publish_Schema (Bound.Disk);
      Publish_Schema (Bound.Disks);
      Publish_Schema (Bound.Request);
      Publish_Schema (Bound.Report);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("system", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation
           (Name (Op), PARAMETERS (Op), CONTRACTS_OF (Op), Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;
end CCL.Interfaces.Install;
