pragma Ada_2022;
with Interfaces;
with ACPI_Service_Core;
with ACPI_HID_I2C_Candidate;
with ACPI_HID_I2C_Resolved;
with ACPI_HID_Offer_Builder;
with GPIO_Bootstrap_Payload;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Candidates is new ACPI_HID_I2C_Candidate (<>);
   with package Resolver is new ACPI_HID_I2C_Resolved (Core, Candidates);
   with package Builder is new ACPI_HID_Offer_Builder (Candidates);
package ACPI_HID_Offer_Provenance with SPARK_Mode is
   package Wire renames Builder.Wire;
   type Stamp is record
      Epoch, Catalog_Revision, Observation_Sequence : Interfaces.Unsigned_64 := 0;
      Initializer_Count : Interfaces.Unsigned_32 := 0;
      Initializer_Digest : Interfaces.Unsigned_64 := 0;
   end record;
   type Outcome is (Prepared, Owner_Busy, Stale_Observation, Invalid_Admission,
      GPIO_Binding_Mismatch, Invalid_Path, Path_Too_Deep, Unknown_Definition,
      Ambiguous_Definition, Unsupported_Table, Builder_Refused);
   -- The serialized owner supplies a freshly completed HID probe, real PCI
   -- observations, and the exact previously submitted GPIO payload/descriptor.
   -- It separately checks the attributable admission receipt and establishes
   -- the closed, quiet handoff boundary. This routine does neither.
   -- It reads metadata only: no AML, hardware access, IPC or new authority.
   -- Paths/defining table come from this Core, not caller-provided strings.
   -- All three bindings and the admitted GPIO incarnation must still match.
   -- Output remains an ordered historical description, not an atomic hardware
   -- snapshot or proof that the device/controller is currently operational.
   procedure Build (Service : Core.State; Observation : Resolver.Observation;
      Admission_Device : Core.Node_Descriptor; Admission : GPIO_Bootstrap_Payload.Request;
      PCI : Wire.PCI_Observation; History : Stamp;
      Result : out Builder.Report; Status : out Outcome)
     with Pre => Core.Valid (Service);
end ACPI_HID_Offer_Provenance;
