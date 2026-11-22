pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_FADT;
generic
   with package Core is new ACPI_Service_Core (<>);
   with procedure Initialize_Local (Facts : ACPI_FADT.Description; Accepted : out Boolean);
   with procedure Execute_Fields (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status);
   with procedure Close_Local;
-- One process-lifetime module callback for the snapshot/driver loader.
-- Only the already admitted, frozen catalog supplies FADT facts. Callbacks
-- are synchronous, nonreentrant and must not capture/mutate Service except
-- through Execute_Fields. Failure closes admission, never releases backing.
package ACPI_Local_Module_Startup is
   type Phase is (Fresh, Starting, Completed, Failed);
   type Reason is (Not_Attempted, Modules_Completed, Invalid_Core,
      Catalog_Not_Frozen, No_Work_Budget, Missing_FADT, Duplicate_FADT,
      Malformed_FADT, Local_Initialization_Refused, Module_Execution_Refused);
   type Report is record
      Stage : Phase := Fresh;
      Status : Reason := Not_Attempted;
      FADT_Index : Natural := 0;
      Local_Attempted, Local_Ready, Module_Attempted : Boolean := False;
      Initialization_Raised, Execution_Raised : Boolean := False;
      Close_Attempted, Close_Completed : Boolean := False;
   end record;
   function Observe return Report;
   procedure Execute_Module_Startup (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status);
   -- Same callback shape used by ACPI_Native_Snapshot_Load.With_Driver.
   -- Closing new PCI/setup admission remains a separate root action AFTER
   -- device initialization (_INI etc), not immediately after module AML.
   -- Callback exceptions become terminal refusal, with no replay or release.
   -- Close_Completed means the close callback returned normally; it does not
   -- establish physical cleanup or authorize retirement. An incomplete close
   -- remains observable and must retain the owner and its backing.
private
   Info : Report;
end ACPI_Local_Module_Startup;
