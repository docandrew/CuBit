pragma Ada_2022;
-- Fixed block extents only. These results do not authorize mapping or I/O;
-- GAS access widths, platform requirements and individual transactions need
-- separate validation. In particular, a span is not one register access.
package ACPI_FADT.Registers with SPARK_Mode, Pure is
   type Space_Kind is (System_Memory, System_IO);
   type Address_Source is (Legacy_Address, Extended_Address);
   type Status is (Described, Absent, Hardware_Reduced, Bad_Length, No_Usable_Address);
   subtype Block_Length is Positive range 1 .. 255;
   type Block_Result (Code : Status := Absent) is record
      case Code is
         when Described =>
            Kind : Block_Kind;
            Space : Space_Kind;
            Base : Unsigned_64;
            Length : Block_Length;
            Source : Address_Source;
            Extended_Rejected : Boolean;
         when others => null;
      end case;
   end record;
   function Fits (Base : Unsigned_64; Length : Block_Length; Last : Unsigned_64) return Boolean is
     (Base /= 0 and then Base <= Last and then Unsigned_64 (Length - 1) <= Last - Base);
   function Valid_Length (Kind : Block_Kind; Length : Unsigned_8) return Boolean is
     (case Kind is
        when PM1A_Event | PM1B_Event => Length >= 4 and then Length mod 2 = 0,
        when PM1A_Control | PM1B_Control => Length >= 2,
        when PM2_Control => Length >= 1,
        when PM_Timer => Length = 4,
        when GPE0 | GPE1 => Length >= 2 and then Length mod 2 = 0);
   -- Prefer the full extended span when addressable in a supported space;
   -- otherwise report any legacy fallback explicitly. Fields suppressed by
   -- HW_REDUCED_ACPI are never described, even if their addresses look valid.
   function Describe (Item : Description; Kind : Block_Kind;
                      Last_Memory_Address : Unsigned_64;
                      Last_IO_Port : Unsigned_16 := Unsigned_16'Last) return Block_Result
     with Post =>
       (if ACPI_FADT.Hardware_Reduced (Item) then Describe'Result.Code = Hardware_Reduced)
       and then (if Describe'Result.Code = Described then
         Describe'Result.Kind = Kind
         and then not ACPI_FADT.Hardware_Reduced (Item)
         and then Valid_Length (Kind, Item.Lengths (Kind))
         and then Describe'Result.Length = Natural (Item.Lengths (Kind))
         and then Fits (Describe'Result.Base, Describe'Result.Length,
           (if Describe'Result.Space = System_Memory then Last_Memory_Address else Unsigned_64 (Last_IO_Port)))
         and then (if Describe'Result.Source = Legacy_Address then
           Describe'Result.Space = System_IO and then Describe'Result.Base = Unsigned_64 (Item.Legacy (Kind))
         else Item.Extended (Kind).Present
           and then Describe'Result.Base = Item.Extended (Kind).Value.Address));
   function Is_Paired (Kind : Block_Kind) return Boolean is
     (Kind in PM1A_Event | PM1B_Event | GPE0 | GPE1);
   -- Status and enable banks partition PM1 event/GPE blocks into equal halves.
   -- The result retains its block kind to distinguish paired register banks.
   function Enable_Base (Block : Block_Result) return Unsigned_64
     with Pre => Block.Code = Described and then Is_Paired (Block.Kind)
       and then Block.Length mod 2 = 0
       and then Fits (Block.Base, Block.Length, Unsigned_64'Last),
     Post => Enable_Base'Result >= Block.Base
       and then Enable_Base'Result - Block.Base = Unsigned_64 (Block.Length / 2);
end ACPI_FADT.Registers;
