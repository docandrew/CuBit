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
      R.Expect (Source, Position, OK, ")");
   end Read_ACPI_Firmware_Profile;

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

