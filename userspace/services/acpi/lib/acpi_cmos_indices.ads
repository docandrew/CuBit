pragma Ada_2022;
with Interfaces;

-- Address planning only. Neither a valid extent nor a selected byte grants
-- port authority or establishes a hardware extension's presence.
package ACPI_CMOS_Indices with SPARK_Mode, Pure is
   subtype Byte is Interfaces.Unsigned_8;
   subtype Word is Interfaces.Unsigned_64;
   subtype Hardware_Index is Byte range 0 .. 127;
   use type Byte, Word;

   -- The RTC time, alarm and control registers occupy the first 14 bytes.
   -- A nonzero CENTURY comes from this owner's validated immutable FADT.
   function Read_Only (Cell, Century : Byte) return Boolean is
     (Cell <= 13 or else (Century /= 0 and then Cell = Century));

   type Decision_Kind is
     (Accepted, Empty_Region, Outside_Device, Outside_Region, Read_Only_Cell);
   type Decision (Kind : Decision_Kind := Empty_Region) is record
      case Kind is
         when Accepted => Cell : Byte;
         when others => null;
      end case;
   end record;

   -- Last_Cell is a trusted, already validated device-profile bound, not an
   -- AML operand. The provider separately binds a physical bank and grant.
   function Resolve
     (Base, Length, Offset : Word; Last_Cell, Century : Byte;
      For_Write : Boolean) return Decision
     with Global => null,
       Post =>
         (if Resolve'Result.Kind = Accepted then
            Length /= 0 and then Base <= Word (Last_Cell)
            and then Length - 1 <= Word (Last_Cell) - Base
            and then Offset < Length
            and then Word (Resolve'Result.Cell) = Base + Offset
            and then Resolve'Result.Cell <= Last_Cell
            and then (not For_Write or else not Read_Only (Resolve'Result.Cell, Century)))
         and then
         (if Length /= 0 and then Base <= Word (Last_Cell)
            and then Length - 1 <= Word (Last_Cell) - Base and then Offset < Length
          then Resolve'Result.Kind =
            (if For_Write and then Read_Only (Byte (Base + Offset), Century)
             then Read_Only_Cell else Accepted));

   -- The bit is retained from an authoritative handover/owner state; reading
   -- port 0x70 is not a portable way to recover it. The index cannot alter it.
   function Selector (Index : Hardware_Index; NMI_Disabled : Boolean) return Byte
     with Global => null,
       Post => (Selector'Result and 16#7F#) = Index
         and then ((Selector'Result and 16#80#) /= 0) = NMI_Disabled;
end ACPI_CMOS_Indices;
