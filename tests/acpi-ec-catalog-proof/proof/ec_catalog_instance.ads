with ACPI_EC_Transactions; with ACPI_EC_Registers; with EC_Proof_Boundary;
package EC_Catalog_Instance with SPARK_Mode is
 package Engine is new ACPI_EC_Transactions (EC_Proof_Boundary.Read, EC_Proof_Boundary.Write, EC_Proof_Boundary.Clock, EC_Proof_Boundary.Owned);
 pragma Assert (Engine.Coherent);
 package Catalog is new ACPI_EC_Registers (Engine);
 pragma Assert (Catalog.Coherent);
end EC_Catalog_Instance;
