pragma Ada_2022;
with System;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with ACPI_Passive_Query;
with ACPI_Grant_Byte_IO;
-- Actual CuBit syscall binding. This native edge is outside the SPARK proof
-- boundary; use it behind ACPI_Observer_Loans, not as a general mapping API.
package ACPI_Runtime_Query_Grants with SPARK_Mode => Off is
   procedure Acquire_Writable_Grant
     (Loan : CuBit.Grant_References.Reference; Expected_Owner : CuBit.Process_IDs.Process_ID;
      Byte_Length : ACPI_Passive_Query.Word; Address : out System.Address; Accepted : out Boolean);
   procedure Return_Grant (Loan : CuBit.Grant_References.Reference; Accepted : out Boolean);
   package IO is new ACPI_Grant_Byte_IO (Acquire_Writable_Grant, Return_Grant);
end ACPI_Runtime_Query_Grants;
