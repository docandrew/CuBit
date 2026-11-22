pragma Ada_2022;
package body ACPI_Native_GPIO_Observation is
   package One renames ACPI_One_Service;
   package Fields renames One.Root.Fields;
   use type Fields.Field_Phase,One.Runtime.Status.Phase;
   Run:Driver.Session;
   function Hardware_Quiet(Service:ACPI_Service.State) return Boolean is
      pragma Unreferenced(Service);
   begin
      return One.Runtime.Current=One.Runtime.Status.Fresh
        and then Fields.Observe.Stage=Fields.Owner_Ready
        and then Fields.Control_Idle and then not Fields.Observe.IO_Pending
        and then not Fields.Uncertain.Present and then Fields.EC_Commands.Quiescent;
   end Hardware_Quiet;
   procedure Discover(Service:aliased in out ACPI_Service.State;
      Budget:Natural;Data:out Firmware_Tables.Bytes;
      Result:out Driver.Report;Action:out Driver.Action_Result) is
   begin Driver.Discover(Run,Service,Budget,Data,Action);Result:=Driver.Observe(Run);end Discover;
   procedure Copy_Resources(Service:in out ACPI_Service.State;Data:out Firmware_Tables.Bytes;
      Result:out Driver.Report;Action:out Driver.Action_Result) is
   begin Driver.Copy_Resources(Run,Service,Data,Action);Result:=Driver.Observe(Run);end Copy_Resources;
   procedure Close(Service:in out ACPI_Service.State;Result:out Driver.Report;Action:out Driver.Action_Result) is
   begin Driver.Close(Run,Service,Action);Result:=Driver.Observe(Run);end Close;
   procedure Copy_Admission_Payload(Service:ACPI_Service.State;
      Epoch:Interfaces.Unsigned_64;History:Payload.Initializer_Provenance;Data:out Payload.P.Request;Result:out Payload.Outcome) is
   begin Payload.Build(Service,Run,Epoch,History,Data,Result);end Copy_Admission_Payload;
   function Description return Driver.Observation.Metadata is (Driver.Description(Run));
   function Pending return Boolean is (Driver.Pending(Run));
   function Is_Current(Service:ACPI_Service.State) return Boolean is (Driver.Is_Current(Run,Service));
end ACPI_Native_GPIO_Observation;
