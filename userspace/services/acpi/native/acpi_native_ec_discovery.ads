pragma Ada_2022;
with ACPI_Service;
with ACPI_EC_Discovery;
with ACPI_One_Service;with AML_Execute;with ACPI_Native_Initializer_History;
-- Post-_INI single-controller enrollment observation. ECDT gives the early
-- root handler and independently granted port bounds, NOT an assumed device
-- path. Fresh namespace identity/_STA/_HID/_GLK/_GPE/_CRS are checked here.
package ACPI_Native_EC_Discovery is
   package Driver is new ACPI_EC_Discovery(ACPI_Service,ACPI_One_Service.Runtime.Evaluate_Named);
   type Outcome is (Not_Attempted,Ready,No_EC,Boot_Unavailable,Budget_Exhausted,
      Multiple_Controllers,Discovery_Failed,Device_Unusable,Source_Unsupported,
      Resources_Disagree,Recovery_Required,Notifications_Pending);
   type Checkpoint_Kind is (Not_Started,Scan_Roots,Describe_Device,Identify_Device,Discover_Controller,Check_Status,Validate_Resources,Scan_Finished);
   type Report is record
      Status:Outcome:=Not_Attempted;
      Candidates,Charged:Natural:=0;
      At_Checkpoint:Checkpoint_Kind:=Not_Started;
      At_Node,Identity_Node:ACPI_Service.Namespace.Node_ID:=ACPI_Service.Namespace.Root;
      Identity_Access,Identity_Cleanup:ACPI_Service.Values.Access_Status:=ACPI_Service.Values.Wrong_Phase;
      Identity_Execution:AML_Execute.Execution_Status:=AML_Execute.Unsupported_Value;
      Identity_Kind_Known:Boolean:=False;
      Identity_Kind:ACPI_Service.Values.Value_Kind:=ACPI_Service.Values.Integer_Description;
      Retained:Natural:=0;
      Device_Path:ACPI_Native_Initializer_History.Path_Text:=[others=>' '];
      Path_Complete:Boolean:=False;
      Description:Driver.Metadata;
      Failure:Driver.Report;
   end record;
   procedure Configure(Service:aliased in out ACPI_Service.State;Budget:Natural;Result:out Report);
   function Observe return Report;
   function Current(Service:ACPI_Service.State) return Boolean;
   function Pending return Boolean;
end ACPI_Native_EC_Discovery;
