-- Hosted test model only; never a native runtime/provider implementation.
with Prep_Model; package body ACPI_Native_GPIO_Observation is
 use type Interfaces.Unsigned_32,Interfaces.Unsigned_64;
 procedure Discover(Service : aliased in out ACPI_Service.State; Budget : Natural; Data : out Firmware_Tables.Bytes; Result : out Driver.Report; Action : out Driver.Action_Result) is pragma Unreferenced(Service);
 begin
 Data:=[others=>0];
 if Data'Length/=0 then raise Program_Error; end if;
 Prep_Model.Discoveries:=Prep_Model.Discoveries+1; Action:=Driver.Accepted;
 Result:=(Status=>Driver.Capacity_Required,Charged=>3,Observation_Result=>(Required=>32),others=><>);
 if Budget<3 then Result.Status:=Driver.Budget_Exhausted; Result.Charged:=Budget;
 elsif Prep_Model.Mode=2 then Result.Status:=Driver.No_Supported_Identity_Observed;
 elsif Prep_Model.Mode=3 then Result.Status:=Driver.Lookup_Failed;
 elsif Prep_Model.Mode=9 then Result.Status:=Driver.Observation_Failed; Result.Fatal.Present:=True;
 elsif Prep_Model.Mode=10 then Result.Status:=Driver.Observation_Failed; Result.Field_Uncertainty:=True; Prep_Model.Uncertain:=True;
 else Prep_Model.Pending:=True; Prep_Model.Roots:=1;
 if Prep_Model.Mode=11 then Result.Observation_Result.Required:=0; end if;
 end if;
 end Discover;
 procedure Copy_Resources(Service : in out ACPI_Service.State; Data : out Firmware_Tables.Bytes; Result : out Driver.Report; Action : out Driver.Action_Result) is pragma Unreferenced(Service);
 begin
 Prep_Model.Copies:=Prep_Model.Copies+1; Data:=[others=>0]; Action:=Driver.Accepted;
 Result:=(Status=>Driver.Described,Charged=>3,others=><>);
 if Prep_Model.Mode in 4 | 21 then Result.Status:=Driver.Cleanup_Failed;
 else Prep_Model.Pending:=False; Prep_Model.Roots:=0; end if;
 end Copy_Resources;
 procedure Copy_Admission_Payload(Service : ACPI_Service.State; Epoch : Interfaces.Unsigned_64; History : Payload.Initializer_Provenance; Data : out Payload.P.Request; Result : out Payload.Outcome) is pragma Unreferenced(Service);
 begin
 Prep_Model.Payloads:=Prep_Model.Payloads+1;
 if Prep_Model.Roots/=0 or Prep_Model.Pending or Epoch/=16#1_0000_0007# then raise Program_Error; end if;
 Data:=(others=><>); Result:=Payload.Payload_Ready;
 if Prep_Model.Mode=6 then Result:=Payload.Invalid_Epoch; return; end if;
 if not History.Complete or else History.Invocations/=3 or else History.Ordered_Digest/=16#0123_4567_89AB_CDEF# then raise Program_Error;end if;
 Data.Initializer_Invocations:=History.Invocations;Data.Initializer_Ordered_Digest:=History.Ordered_Digest;
 Data.Spans:=1; Data.Segments:=2; Data.Path(1):="_SB_"; Data.Path(2):="GPI0";
 Data.HID:="INTC1057"; Data.Table:="DSDT"; Data.Table_ID:="TESTGPIO"; Data.Epoch:=Epoch; Data.STA:=15;
 Data.Items(1):=(Base=>16#F000_0000#,Length=>4096,From=>Payload.P.Memory32_Fixed,Writable=>True);
 end Copy_Admission_Payload;
 procedure Close(Service : in out ACPI_Service.State; Result : out Driver.Report; Action : out Driver.Action_Result) is pragma Unreferenced(Service);
 begin Prep_Model.Closes:=Prep_Model.Closes+1; Result:=(others=><>); Action:=Driver.Accepted;
 if Prep_Model.Mode/=21 then Prep_Model.Pending:=False; Prep_Model.Roots:=0; end if;
 end Close;
 function Description return Metadata is ((Device=>ACPI_Service.Node_Descriptor(Prep_Model.Source_Device)));
 function Pending return Boolean is (Prep_Model.Pending);
end ACPI_Native_GPIO_Observation;