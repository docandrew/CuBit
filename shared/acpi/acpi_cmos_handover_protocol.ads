pragma Ada_2022;
with Interfaces;

-- Boot-only devmgr/procmgr exchange before OP_READY, never an AML/runtime
-- channel. Metadata describes the accompanying exact IO capability; it is
-- not authority and must be checked against that capability's description.
package ACPI_CMOS_Handover_Protocol with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   Request_Label : constant := 16#A7C0#;
   Reply_Label : constant := 16#A7C1#;
   Request_Words : constant := 0;
   Reply_Words : constant := 4;
   First_Port : constant Word := 16#70#;
   type Profile is (Unidentified, Legacy_64, Intel_128, Intel_256);
   type Receipt is record
      Device : Profile := Unidentified;
      NMI_Disabled : Boolean := False;
   end record;
   type Words is array (0 .. 3) of Word;
   function Port_Count (Item : Profile) return Word is
     (if Item = Intel_256 then 4 else 2);
   type Decoded (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Receipt;
         when False => null;
      end case;
   end record;
   -- Flag bit0 means a completed selector history is known; bit1 is its
   -- retained NMI value. Every other flag bit is reserved and must be zero.
   function Decode (Input : Words) return Decoded
     with Post => Decode'Result.Valid =
       (Input (0) = First_Port and then Input (3) in 1 .. 3
        and then Input (2) in 1 | 3
        and then Input (1) = (if Input (3) = 3 then 4 else 2))
       and then (if Decode'Result.Valid then
         Decode'Result.Value.Device = Profile'Val (Input (3))
         and then Decode'Result.Value.NMI_Disabled = (Input (2) = 3));
   function Encode (Item : Receipt) return Words
     with Pre => Item.Device /= Unidentified,
       Post => Decode (Encode'Result).Valid
         and then Decode (Encode'Result).Value = Item;
end ACPI_CMOS_Handover_Protocol;
