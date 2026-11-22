pragma Ada_2022;
with Interfaces;
with ACPI_FADT.Transactions;
-- Relative PCI configuration geometry only. No physical address, mapping,
-- register access or authority is supplied or created by this package.
package ACPI_PCI_Config_Reads with SPARK_Mode, Pure is
   package T renames ACPI_FADT.Transactions;
   subtype Word is Interfaces.Unsigned_64;
   use type Word, T.Access_Width;
   Devices_Per_Bus : constant := 32;
   Functions_Per_Device : constant := 8;
   Function_Bytes : constant := 4096;
   Bus_Bytes : constant := Devices_Per_Bus * Functions_Per_Device * Function_Bytes;
   type Segment_Number is range 0 .. 65_535;
   type Bus_Number is range 0 .. 255;
   type Bus_Key is record
      Segment : Segment_Number := 0;
      Bus : Bus_Number := 0;
   end record;
   type Device_Number is range 0 .. Devices_Per_Bus - 1;
   type Function_Number is range 0 .. Functions_Per_Device - 1;
   type Location is record
      Segment : Segment_Number := 0;
      Bus : Bus_Number := 0;
      Device : Device_Number := 0;
      Func : Function_Number := 0;
   end record;
   -- Logical function identity. Known=False never means function 0:0:0.0.
   -- This metadata is separate from the capability authorizing any access.
   type Route (Known : Boolean := False) is record
      case Known is
         when True => Device : Location;
         when False => null;
      end case;
   end record;
   type Region is record
      Device : Location;
      -- PCI config byte offsets, never SystemMemory addresses.
      Base, Length : Word := 0;
   end record;
   type ECAM_Offset is range 0 .. Bus_Bytes - 1;
   subtype Read_Bytes is Positive range 1 .. 4;
   type Outcome is (Read_Ready, Invalid_Window, Write_Refused,
      Unsupported_Segment, Unsupported_Bus, Unsupported_Width,
      Invalid_Region, Outside_Region, Misaligned);
   type View (Status : Outcome := Invalid_Window) is record
      case Status is
         when Read_Ready =>
            First : ECAM_Offset;
            Bytes : Read_Bytes;
            Width : T.Access_Width;
         when others => null;
      end case;
   end record;
   function Valid_Region (Item : Region) return Boolean is
     (Item.Base < Function_Bytes and then Item.Length /= 0
      and then Item.Length <= Function_Bytes - Item.Base);
   -- First bring-up slice: an independently inspected, read-only, uncached
   -- segment-zero/bus-zero ECAM root, exactly one bus in size. Window_Bytes
   -- checks extent only; callers must separately establish kernel provenance,
   -- rights, cache mode, live mapping and named descriptor ownership/lifetime.
   function Plan
     (Item : Region; Offset : Word; Width : T.Access_Width;
      Window_Bytes : Word; For_Write : Boolean := False) return View
   with Global => null,
     Post => (if Plan'Result.Status = Read_Ready then
       not For_Write and then Window_Bytes = Bus_Bytes
       and then Item.Device.Segment = 0 and then Item.Device.Bus = 0
       and then Valid_Region (Item) and then Width /= T.Qword_Access
       and then Offset < Item.Length
       and then Word (T.Octets (Width)) <= Item.Length - Offset
       and then Plan'Result.Width = Width
       and then Plan'Result.Bytes = T.Octets (Width)
       and then Word (Plan'Result.First) =
         (Word (Item.Device.Device) * Functions_Per_Device + Word (Item.Device.Func))
           * Function_Bytes + Item.Base + Offset
       and then Word (Plan'Result.First) / Function_Bytes =
         Word (Item.Device.Device) * Functions_Per_Device + Word (Item.Device.Func)
       and then Word (Plan'Result.First) mod Word (Plan'Result.Bytes) = 0
       and then Word (Plan'Result.First) <= Bus_Bytes - Word (Plan'Result.Bytes));
end ACPI_PCI_Config_Reads;
