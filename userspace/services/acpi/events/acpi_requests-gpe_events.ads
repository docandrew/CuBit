pragma Ada_2022;
with ACPI_GPE_Methods;
with ACPI_Bootstrap.GPE_Events;
generic
   with package Handlers is new ACPI_GPE_Methods(Core=>ACPI_Service,others=><>);
package ACPI_Requests.GPE_Events with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old(Allow);
   package Controller is new ACPI_Bootstrap.GPE_Events(Handlers);
   procedure Resolve(Server:State;Event:Handlers.Event_Number;Item:out Handlers.Binding;
      Allowed:out Controller.Admission;Result:out Handlers.Resolution_Report) with Pre=>Valid(Server);
   procedure Invoke(Server:in out State;Run:in out Handlers.Invocation;Item:Handlers.Binding;
      Budget:Natural;Allowed:out Controller.Admission;Action:out Handlers.Action_Result)
      with Pre=>Valid(Server),Post=>Valid(Server) and then Revision(Server)=Revision(Server)'Old;
   procedure Close(Server:in out State;Run:in out Handlers.Invocation;Action:out Handlers.Action_Result)
      with Pre=>Valid(Server),Post=>Valid(Server) and then Revision(Server)=Revision(Server)'Old;
end ACPI_Requests.GPE_Events;
