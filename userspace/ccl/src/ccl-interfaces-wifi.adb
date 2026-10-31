with CCL.Host_Values;
with CCL.Language;
with CCL.Objects.Catalog;
with CCL.VM;

package body CCL.Interfaces.Wifi with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Language.Analysis_Status;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean) is
      Checked : CCL.Language.Analysis_Result;
      Types : CCL.Types.Registry;
      Network_Type, List_Type, Status_Type : CCL.Types.Type_Reference;
      Listed : CCL.Types.List_Result;
   begin
      Bound := (others => <>);
      --  The declarations, then a trivial expression: one checked program.
      CCL.Language.Analyze (TYPE_SOURCE & "0", Checked);
      Accepted := CCL.Language.Analysis_Status_Of (Checked) = CCL.Language.Analysis_Succeeded;
      if not Accepted then return; end if;
      Types := CCL.Language.Analysis_Types (Checked);
      Network_Type := CCL.Types.Find (Types, CCL.Types.Named ("Wifi_Network"));
      CCL.Types.Specialize_List (Types, Network_Type, List_Type, Listed);
      Accepted := Listed in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      if Accepted then CCL.Objects.Bind (Types, Network_Type, NETWORK_KEY, Bound.Network, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, List_Type, NETWORKS_KEY, Bound.Networks, Accepted); end if;
      Status_Type := CCL.Types.Find (Types, CCL.Types.Named ("Wifi_Status"));
      if Accepted then CCL.Objects.Bind (Types, Status_Type, STATUS_KEY, Bound.Connection, Accepted); end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Bound : Contracts;
      Accepted : Boolean;
      Result : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      --  Reading what the radios heard observes the system; it takes no argument.
      SCAN_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Integer_Value,
         Result => CCL.Host_Values.Object_Value, Result_Schema => NETWORKS_KEY,
         Authority => CCL.VM.Observe_Authority, others => <>);
      --  Joining and leaving act on the system; each answers the status.
      JOIN_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAXIMUM_SSID_ARGUMENT,
         Result => CCL.Host_Values.Object_Value, Result_Schema => STATUS_KEY,
         Authority => CCL.VM.Control_Authority, others => <>);
      LEAVE_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Integer_Value,
         Result => CCL.Host_Values.Object_Value, Result_Schema => STATUS_KEY,
         Authority => CCL.VM.Control_Authority, others => <>);
      STATUS_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Integer_Value,
         Result => CCL.Host_Values.Object_Value, Result_Schema => STATUS_KEY,
         Authority => CCL.VM.Observe_Authority, others => <>);
      --  The passphrase never reaches the program: the console asks for it.
      PASSPHRASE_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAXIMUM_SSID_ARGUMENT,
         Result => CCL.Host_Values.Text_Value, Result_Text_Limit => 192,
         Authority => CCL.VM.Control_Authority, others => <>);
      procedure Add (Item : Operation; Parameters : Natural; Contract : CCL.Host_Values.Import_Declaration) is
      begin
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Define_Host_Operation (Name (Item), Parameters, Contract, Entry_Operation, Error);
         end if;
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end Add;
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
      Publish_Schema (Bound.Network);
      Publish_Schema (Bound.Networks);
      Publish_Schema (Bound.Connection);
      if not Accepted then return; end if;
      CCL.Catalog.Define_Interface ("wifi", 1, 0, DIGEST, Descriptor, Error);
      Add (Scan, 0, SCAN_CONTRACT);
      Add (Join, 1, JOIN_CONTRACT);
      Add (Leave, 0, LEAVE_CONTRACT);
      Add (Status, 0, STATUS_CONTRACT);
      Add (Passphrase, 1, PASSPHRASE_CONTRACT);
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;
end CCL.Interfaces.Wifi;
