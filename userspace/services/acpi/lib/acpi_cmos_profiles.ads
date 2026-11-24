pragma Ada_2022;
with ACPI_CMOS_Indices;

package ACPI_CMOS_Profiles with SPARK_Mode, Pure is
   package I renames ACPI_CMOS_Indices;
   use type I.Byte;
   -- ACPI 6.6 sections 9.14.1--9.14.3. PNP0B00 guarantees 64 bytes;
   -- PNP0B01 identifies the Intel-compatible protocol. Port presence is
   -- insufficient to identify a device or prove its extension is enabled.
   -- Intel 9 Series PCH datasheet, sections 12.6.1 and 12.6.2: two 128-byte
   -- banks, standard 0x70/0x71 and enabled extended 0x72/0x73.
   type Device_Kind is (Unknown_Device, PC_AT_64, Intel_Compatible, Dallas);
   type Profile is record
      Device : Device_Kind := Unknown_Device;
      Extended_Enabled : Boolean := False;
   end record;
   function Supported (Item : Profile) return Boolean is
     (Item.Device in PC_AT_64 | Intel_Compatible);
   function Last_Cell (Item : Profile) return I.Byte is
     (if Item.Device = PC_AT_64 then 63
      elsif Item.Extended_Enabled then 255 else 127)
     with Pre => Supported (Item);

   type Port_Pair is (Standard_Pair, Extended_Pair);
   type Route_Kind is (Ready, Unsupported_Device, Outside_Profile);
   type Route (Kind : Route_Kind := Unsupported_Device) is record
      case Kind is
         when Ready =>
            Pair : Port_Pair;
            Index : I.Hardware_Index;
         when others => null;
      end case;
   end record;
   function Locate (Item : Profile; Cell : I.Byte) return Route
     with Global => null,
       Post =>
         (if not Supported (Item) then Locate'Result.Kind = Unsupported_Device
          elsif Cell > Last_Cell (Item) then Locate'Result.Kind = Outside_Profile
          else Locate'Result.Kind = Ready)
         and then
         (if Locate'Result.Kind = Ready then
            Supported (Item) and then Cell <= Last_Cell (Item)
            and then Locate'Result.Index = Cell mod 128
            and then Locate'Result.Pair =
              (if Cell < 128 then Standard_Pair else Extended_Pair)
            and then (if Locate'Result.Pair = Extended_Pair then
               Item.Device = Intel_Compatible and then Item.Extended_Enabled));
end ACPI_CMOS_Profiles;
