pragma Ada_2022;
with ACPI_Sleep_Objects;
with ACPI_Bootstrap.Sleep_States;
generic
   with package Driver is new ACPI_Sleep_Objects (Core => ACPI_Service, others => <>);
package ACPI_Requests.Sleep_States with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Controller is new ACPI_Bootstrap.Sleep_States (Driver);
   -- Same private Core and fixed evaluator as this owner. Descriptive FADT
   -- model only; no method/node/address supplied by an observer or wire peer.
   procedure Observe (Server : in out State; Registers : Driver.Register_Model;
      Budget : Natural; Result : out Controller.Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old and then Result.Charged <= Budget;
end ACPI_Requests.Sleep_States;
