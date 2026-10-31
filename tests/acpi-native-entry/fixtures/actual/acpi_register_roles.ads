pragma Ada_2022;
with Interfaces;
-- Shared register-service badge, minted only by the trusted launcher after
-- startup-plan approval. Classify kernel authorityTag only, never wire data,
-- process IDs or the caller's claimed identity. A tag value is not authority.
package ACPI_Register_Roles with Pure, SPARK_Mode is
   type Role_Tag is new Interfaces.Unsigned_64;
   Frontend_Tag : constant Role_Tag := 16#4143_5049_4652_4E54#; -- ACPIFRNT
   function Is_Frontend (Stamp : Role_Tag) return Boolean is
     (Stamp = Frontend_Tag);
end ACPI_Register_Roles;
