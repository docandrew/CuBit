with Interfaces;with ACPI_Service;with ACPI_Requests;with ACPI_Local_Runtime;
with ACPI_Requests.With_Local_Runtime;
package ACPI_One_Service is
   Owner_Allowed, Close_Allowed : Boolean := True;
   function Ready(Service:ACPI_Service.State) return Boolean;
   function Idle return Boolean;
   procedure Claim(Service:ACPI_Service.State;Good:out Boolean);
   procedure Close_Owner;
   procedure Enter_Delays(Good:out Boolean);
   procedure Begin_Delays(Deadline:Interfaces.Unsigned_64;Good:out Boolean);
   procedure End_Delays(Good:out Boolean);
   procedure Stop_Delays;
   package Runtime is new ACPI_Local_Runtime(ACPI_Service,Ready,Idle,Claim,Close_Owner,
      Enter_Delays,Begin_Delays,End_Delays,Stop_Delays,
      ACPI_Service.Evaluate_Named,ACPI_Service.Invoke_Retained,ACPI_Service.Invoke_With_Arguments);
   package Runtime_Access is new ACPI_Requests.With_Local_Runtime(Runtime.Enter_Runtime,Runtime.Begin_Event,Runtime.End_Event);
   procedure Attach(Server:ACPI_Requests.State;Accepted:out Boolean);
   package Root is
      package Hardware is function Admitting return Boolean;end Hardware;
      package Fields is
         function Control_Idle return Boolean;
         package EC_Commands is function Quiescent return Boolean;end EC_Commands;
      end Fields;
   end Root;
end ACPI_One_Service;
