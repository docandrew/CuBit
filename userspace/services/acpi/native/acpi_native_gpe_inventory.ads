pragma Ada_2022;
with ACPI_Requests; with ACPI_One_Service.GPE_Events; with ACPI_SCI_Registers;
-- Descriptive cache only. Source-control operations still recheck the SCI
-- owner, and invocation revalidates each opaque binding on the same Core.
package ACPI_Native_GPE_Inventory is
   package Methods renames ACPI_One_Service.GPE_Events.Methods;
   subtype Source_Count is Natural range 0 .. ACPI_SCI_Registers.GPE_Count;
   type Phase is (Fresh, Ready, Failed);
   type Failure is (None, Not_Ready, EC_Source_Unavailable, Owner_Refused, Metadata_Exception);
   type Entry_Status is (Unobserved, Not_Available, EC_Source, No_Handler,
      Handler_Ready, Handler_Declined);
   type Report is record
      Stage : Phase := Fresh;
      Why : Failure := None;
      Attempted : Boolean := False;
      Available, EC_Excluded, Bound, Missing, Declined : Source_Count := 0;
      Has_Declined : Boolean := False;
      First_Declined : ACPI_SCI_Registers.GPE_Number := 0;
      Decline_Reason : Methods.Resolution_Outcome := Methods.Missing;
   end record;
   function Observe return Report;
   -- Read-only same-Core lookup, no AML, hardware IO, logging or enablement.
   -- Unready calls do not consume configuration; an attempted scan is one-shot.
   -- EC evidence comes only from the trusted local admitted source owner.
   procedure Configure (Server : ACPI_Requests.State; EC_Present : Boolean;
      EC_GPE : ACPI_SCI_Registers.GPE_Number; Accepted : out Boolean);
   function Status (GPE : ACPI_SCI_Registers.GPE_Number) return Entry_Status;
   -- No partially scanned binding escapes. This is metadata, not source
   -- authority; source availability and method identity can change later.
   procedure Lookup (GPE : ACPI_SCI_Registers.GPE_Number;
      Item : out Methods.Binding; Found : out Boolean);
end ACPI_Native_GPE_Inventory;
