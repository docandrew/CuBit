with ACPI_EC_Transactions;
with EC_Proof_Boundary;
package EC_Proof_Instance with SPARK_Mode is
   package Owner is new ACPI_EC_Transactions
     (EC_Proof_Boundary.Read, EC_Proof_Boundary.Write,
      EC_Proof_Boundary.Clock, EC_Proof_Boundary.Owned);
end EC_Proof_Instance;
