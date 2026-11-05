pragma Ada_2022;
package body ACPI_Firmware_Data_Identities with SPARK_Mode,
   Refined_State => (Issuance => Issued) is
   Issued : F.Identifier := F.No_ID;
   function Issued_Count return F.Identifier is (Issued);
   procedure Issue (ID : out F.Identifier; Accepted : out Boolean) is
   begin
      ID := F.No_ID; Accepted := False;
      if Issued = F.Identifier'Last then return; end if;
      Issued := Issued + 1; ID := Issued; Accepted := True;
   end Issue;
end ACPI_Firmware_Data_Identities;
