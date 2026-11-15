pragma Ada_2022;
with ACPI_GPE_Methods;
generic
   with package Handlers is new ACPI_GPE_Methods(Core=>ACPI_Service,others=><>);
-- Trusted same-Core method bridge only. Source registration, masking,
-- trigger selection and event replay protection belong to the GPE owner.
package ACPI_Bootstrap.GPE_Events with SPARK_Mode is
   type Admission is (Not_Ready,Notifications_Pending,Results_Pending,Admitted);
   procedure Resolve(Session:State;Event:Handlers.Event_Number;Item:out Handlers.Binding;
      Allowed:out Admission;Result:out Handlers.Resolution_Report) with Pre=>Valid(Session);
   procedure Invoke(Session:in out State;Run:in out Handlers.Invocation;Item:Handlers.Binding;
      Budget:Natural;Allowed:out Admission;Action:out Handlers.Action_Result)
      with Pre=>Valid(Session),Post=>Valid(Session);
   -- Cleanup is available even after readiness/identity changes. Retain Run
   -- and its original Core until cleanup or whole-owner retention is settled.
   procedure Close(Session:in out State;Run:in out Handlers.Invocation;Action:out Handlers.Action_Result)
      with Pre=>Valid(Session),Post=>Valid(Session);
end ACPI_Bootstrap.GPE_Events;
