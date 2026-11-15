-- Hosted test model only; never a native runtime/provider implementation.
with ACPI_Service; with Firmware_Tables; with GPIO_Bootstrap_Payload; with Interfaces;
package ACPI_Native_GPIO_Observation is
 package Driver is
 type Outcome is (Not_Started,Capacity_Required,Described,No_Supported_Identity_Observed,Lookup_Failed,Cleanup_Failed,Observation_Failed,Budget_Exhausted);
 type Fatal_Observation is record Present : Boolean:=False; end record;
 type Detail is record Required : Natural:=0; end record;
 type Report is record Status : Outcome:=Not_Started; Charged : Natural:=0; Fatal : Fatal_Observation; Field_Uncertainty : Boolean:=False; Observation_Result : Detail; end record;
 type Action_Result is (Accepted,Wrong_Context); end Driver;
 package Payload is package P renames GPIO_Bootstrap_Payload;
 type Initializer_Provenance is record Complete:Boolean:=False;Invocations:Interfaces.Unsigned_32:=0;Ordered_Digest:Interfaces.Unsigned_64:=0;end record;
 type Outcome is (Payload_Ready,Invalid_Epoch); end Payload;
 procedure Discover(Service : aliased in out ACPI_Service.State; Budget : Natural; Data : out Firmware_Tables.Bytes; Result : out Driver.Report; Action : out Driver.Action_Result);
 procedure Copy_Resources(Service : in out ACPI_Service.State; Data : out Firmware_Tables.Bytes; Result : out Driver.Report; Action : out Driver.Action_Result);
 procedure Copy_Admission_Payload(Service : ACPI_Service.State; Epoch : Interfaces.Unsigned_64; History : Payload.Initializer_Provenance; Data : out Payload.P.Request; Result : out Payload.Outcome);
 procedure Close(Service : in out ACPI_Service.State; Result : out Driver.Report; Action : out Driver.Action_Result);
 function Pending return Boolean;
end ACPI_Native_GPIO_Observation;