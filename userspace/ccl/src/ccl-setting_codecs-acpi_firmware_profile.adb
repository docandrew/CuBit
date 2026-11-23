pragma Ada_2022;

package body CCL.Setting_Codecs.ACPI_Firmware_Profile with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_ACPI_Profile
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out ACPI_Profile_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := ACPI_Profile_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "ACPI_Profile.Neutral") then
         R.Expect_Word (Source, Position, OK, "ACPI_Profile.Neutral");
         Item := Neutral;
      elsif R.Next_Is_Word (Source, Position, "ACPI_Profile.Known_Windows") then
         R.Expect_Word (Source, Position, OK, "ACPI_Profile.Known_Windows");
         Item := Known_Windows;
      else
         OK := False;
      end if;
   end Read_ACPI_Profile;

   procedure Read_CMOS_Profile
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out CMOS_Profile_Value)
     with Always_Terminates, Pre => R.In_Source (Source, Position),
       Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := No_CMOS;
      if not OK then return;
      elsif R.Next_Is_Word (Source, Position, "CMOS_Profile.No_CMOS") then
         R.Expect_Word (Source, Position, OK, "CMOS_Profile.No_CMOS"); Item := No_CMOS;
      elsif R.Next_Is_Word (Source, Position, "CMOS_Profile.PC_AT_64") then
         R.Expect_Word (Source, Position, OK, "CMOS_Profile.PC_AT_64"); Item := PC_AT_64;
      elsif R.Next_Is_Word (Source, Position, "CMOS_Profile.Intel_128") then
         R.Expect_Word (Source, Position, OK, "CMOS_Profile.Intel_128"); Item := Intel_128;
      elsif R.Next_Is_Word (Source, Position, "CMOS_Profile.Intel_256") then
         R.Expect_Word (Source, Position, OK, "CMOS_Profile.Intel_256"); Item := Intel_256;
      else OK := False; end if;
   end Read_CMOS_Profile;

   procedure Read_ACPI_Firmware_Profile
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out ACPI_Firmware_Profile_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(ACPI_Firmware_Profile");
      R.Expect (Source, Position, OK, " kind => ");
      Read_ACPI_Profile (Source, Position, OK, Item.kind);
      R.Expect (Source, Position, OK, " cmos_profile => ");
      Read_CMOS_Profile (Source, Position, OK, Item.cmos_profile);
      R.Expect (Source, Position, OK, " cmos_known => ");
      R.Read_Boolean (Source, Position, OK, Item.cmos_known);
      R.Expect (Source, Position, OK, " cmos_nmi_disabled => ");
      R.Read_Boolean (Source, Position, OK, Item.cmos_nmi_disabled);
      R.Expect (Source, Position, OK, ")");
   end Read_ACPI_Firmware_Profile;

   function Bind_CMOS
     (Item : ACPI_Firmware_Profile_Value; Trusted_Profile : CMOS_Profile_Value;
      Known, NMI_Disabled, Approved : Boolean) return ACPI_Firmware_Profile_Value
   is
      Available : constant Boolean := Approved and then Known and then Trusted_Profile /= No_CMOS;
   begin
      return (kind => Item.kind,
        cmos_profile => (if Available then Trusted_Profile else No_CMOS),
        cmos_known => Available, cmos_nmi_disabled => Available and then NMI_Disabled);
   end Bind_CMOS;

   function Encode (Item : ACPI_Firmware_Profile_Value) return String is
      Kind_Text : constant String := (if Item.kind = Neutral then "ACPI_Profile.Neutral" else "ACPI_Profile.Known_Windows");
      CMOS_Text : constant String :=
        (case Item.cmos_profile is
           when No_CMOS => "CMOS_Profile.No_CMOS",
           when PC_AT_64 => "CMOS_Profile.PC_AT_64",
           when Intel_128 => "CMOS_Profile.Intel_128",
           when Intel_256 => "CMOS_Profile.Intel_256");
   begin
      return "(ACPI_Firmware_Profile kind => " & Kind_Text &
        " cmos_profile => " & CMOS_Text & " cmos_known => " &
        (if Item.cmos_known then "true" else "false") & " cmos_nmi_disabled => " &
        (if Item.cmos_nmi_disabled then "true" else "false") & ")";
   end Encode;

   procedure Decode (Source : String; Item : out ACPI_Firmware_Profile_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_ACPI_Firmware_Profile (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.ACPI_Firmware_Profile;

