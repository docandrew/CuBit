pragma Ada_2022;
with CuBit.Canonical_Readers;

-- Canonical launch profile codec. CMOS facts are supplied by procmgr, never config.
package CCL.Setting_Codecs.ACPI_Firmware_Profile with SPARK_Mode, Pure is
   subtype Text is CuBit.Canonical_Readers.Text;
   type ACPI_Profile_Value is (Neutral, Known_Windows);
   type CMOS_Profile_Value is (No_CMOS, PC_AT_64, Intel_128, Intel_256);
   type ACPI_Firmware_Profile_Value is record
      kind : ACPI_Profile_Value := ACPI_Profile_Value'First;
      cmos_profile : CMOS_Profile_Value := No_CMOS;
      cmos_known, cmos_nmi_disabled : Boolean := False;
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a ACPI_Firmware_Profile. Otherwise Item is (others => <>).
   function Bind_CMOS
     (Item : ACPI_Firmware_Profile_Value; Trusted_Profile : CMOS_Profile_Value;
      Known, NMI_Disabled, Approved : Boolean) return ACPI_Firmware_Profile_Value
     with Global => null,
       Post => Bind_CMOS'Result.kind = Item.kind
         and then Bind_CMOS'Result.cmos_known =
           (Approved and then Known and then Trusted_Profile /= No_CMOS)
         and then Bind_CMOS'Result.cmos_profile =
           (if Approved and then Known and then Trusted_Profile /= No_CMOS then Trusted_Profile else No_CMOS)
         and then Bind_CMOS'Result.cmos_nmi_disabled =
           (Approved and then Known and then Trusted_Profile /= No_CMOS and then NMI_Disabled);
   function Encode (Item : ACPI_Firmware_Profile_Value) return String;
   procedure Decode (Source : String; Item : out ACPI_Firmware_Profile_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.ACPI_Firmware_Profile;

