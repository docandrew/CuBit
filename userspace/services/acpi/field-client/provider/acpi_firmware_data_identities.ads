pragma Ada_2022;
with ACPI_Firmware_Data_Frames;
-- One library-level issuer shared by all clients in this process. Never
-- instantiate/reset per Field, provider context or namespace lifetime.
package ACPI_Firmware_Data_Identities with SPARK_Mode,
   Abstract_State => Issuance, Initializes => Issuance is
   package F renames ACPI_Firmware_Data_Frames;
   pragma Unevaluated_Use_Of_Old (Allow);
   use type F.Identifier;
   function Issued_Count return F.Identifier with Ghost, Global => (Input => Issuance);
   procedure Issue (ID : out F.Identifier; Accepted : out Boolean)
      with Global => (In_Out => Issuance),
      Post =>
         (if Issued_Count'Old < F.Identifier'Last then
            Accepted and then ID = Issued_Count'Old + 1 and then Issued_Count = ID
          else not Accepted and then ID = F.No_ID and then Issued_Count = Issued_Count'Old);
end ACPI_Firmware_Data_Identities;
