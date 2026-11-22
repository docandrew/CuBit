pragma Ada_2022;
with ACPI_HID_I2C_Candidate;
with HID_Offer_Payload;
generic
   with package Candidates is new ACPI_HID_I2C_Candidate (<>);
package ACPI_HID_Offer_Builder with Pure, SPARK_Mode is
   package Wire renames HID_Offer_Payload;
   use type Wire.Request;
   -- The serialized owning Core supplies this after re-observing the device,
   -- both resource sources and PCI identity/properties. Resolved segment0
   -- may use the explicitly recorded standard absent-SEG default. These are descriptions,
   -- not freshness evidence or hardware authority by themselves.
   type Context is record
      Epoch, Catalog_Revision, Observation_Sequence : Wire.Word := 0;
      PCI : Wire.PCI_Observation;
      GPIO_UID_Kind : Wire.UID_Kind := Wire.No_UID;
      GPIO_UID : Wire.Word := 0;
      GPIO_HID : Wire.Text_8 := [others => Wire.NUL];
      Table : Wire.Text_4 := [others => Wire.NUL];
      Table_ID : Wire.Text_8 := [others => Wire.NUL];
      Table_Digest : Wire.Word := 0;
      Initializer_Count : Wire.Small_Word := 0;
      Initializer_Digest, Resource_Digest : Wire.Word := 0;
      Device_Path, I2C_Path, GPIO_Path : Wire.Canonical_Path;
   end record;
   type Outcome is (Built, Unsupported_Candidate, Span_Too_Long, Invalid_Description);
   type Report is record
      Status : Outcome := Unsupported_Candidate;
      Value : Wire.Request := Wire.Empty_Request;
   end record;
   procedure Build (Facts : Candidates.Candidate; Provenance : Context;
      Result : out Report)
     with Global => null,
       Post => (if Result.Status = Built then Wire.Valid (Result.Value)
                else Result.Value = Wire.Empty_Request);
   -- No AML, address selection, pin translation, role defaulting or bus-speed
   -- clamping. All four raw spans are copied; unused wire storage is zero.
   -- Unresolved PCI stays unresolved. A later receiver must still authenticate
   -- the sender and independently admit the controller and GPIO binding.
end ACPI_HID_Offer_Builder;
