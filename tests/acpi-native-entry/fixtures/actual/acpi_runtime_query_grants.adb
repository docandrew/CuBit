pragma Ada_2022;
with CuBit.Memory_Grants;
package body ACPI_Runtime_Query_Grants with SPARK_Mode => Off is
   procedure Acquire_Writable_Grant
     (Loan : CuBit.Grant_References.Reference; Expected_Owner : CuBit.Process_IDs.Process_ID;
      Byte_Length : ACPI_Passive_Query.Word; Address : out System.Address; Accepted : out Boolean) is
   begin
      CuBit.Memory_Grants.Acquire
        (reference => Loan, expectedOwner => Expected_Owner, byteOffset => 0,
         byteLength => Byte_Length, requiredAccess => CuBit.Memory_Grants.Write_Access,
         mappedAddress => Address, success => Accepted);
   end Acquire_Writable_Grant;
   procedure Return_Grant (Loan : CuBit.Grant_References.Reference; Accepted : out Boolean) is
   begin CuBit.Memory_Grants.Return_Acquisition (Loan, Accepted); end Return_Grant;
end ACPI_Runtime_Query_Grants;
