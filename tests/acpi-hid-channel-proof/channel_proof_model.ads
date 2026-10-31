with ACPI_HID_I2C_Candidate;
with ACPI_HID_I2C_Channels;
package Channel_Proof_Model with SPARK_Mode is
   -- Match the native owner's current copied-span budget.
   package Candidates is new ACPI_HID_I2C_Candidate (256);
   package Channels is new ACPI_HID_I2C_Channels (Candidates);
end Channel_Proof_Model;
