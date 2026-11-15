pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_GPIO_Bootstrap_Observation;
with GPIO_Bootstrap_Payload;
with Interfaces;
-- Copied observations to the shared codec's typed request. No AML, IPC,
-- hardware access or admission. The outer lifecycle establishes pre-consumer
-- ordering and the closed/quiet IO boundary before sending these facts.
generic
   with package Core is new ACPI_Service_Core(<>);
   with package Observation is new ACPI_GPIO_Bootstrap_Observation(Core=>Core,others=><>);
   type Source_Session is limited private;
   with function Describe_Source(Run:Source_Session) return Observation.Metadata;
   with function Current_Source(Run:Source_Session;Service:Core.State) return Boolean;
package ACPI_GPIO_Admission_Payload with SPARK_Mode is
   package P renames GPIO_Bootstrap_Payload;
   type Outcome is (Payload_Ready,Stale_Observation,Invalid_Epoch,
      Invalid_Path,Path_Too_Deep,Unknown_Definition,Ambiguous_Definition,
      Unsupported_Table,Unsupported_Status,Unsupported_Interrupt,Invalid_Payload,Incomplete_Initializer_History);
   type Initializer_Provenance is record
      Complete : Boolean := False;
      Invocations : Interfaces.Unsigned_32 := 0;
      Ordered_Digest : Interfaces.Unsigned_64 := P.Empty_Initializer_Digest;
   end record;
   Empty_Payload:constant P.Request:=(others=><>);
   use type P.Request,Interfaces.Unsigned_64,Interfaces.Unsigned_32;
   -- Epoch is supplied by the owner of the single immutable startup catalog.
   -- It is NOT extracted by narrowing/reinterpreting a Core handle/identity.
   procedure Build(Service:Core.State;Run:Source_Session;
      Epoch:Interfaces.Unsigned_64;History:Initializer_Provenance;Data:out P.Request;Result:out Outcome)
     with Pre=>Core.Valid(Service),
       Post=>(if Result=Payload_Ready then P.Valid(Data) and then Data.Epoch=Epoch and then History.Complete
              and then Data.Initializer_Invocations=History.Invocations
              and then Data.Initializer_Ordered_Digest=History.Ordered_Digest
              else Data=Empty_Payload);
end ACPI_GPIO_Admission_Payload;
