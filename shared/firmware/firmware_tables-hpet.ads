pragma Ada_2022;
with Interfaces; use Interfaces;

-- Checked ACPI descriptor and register transforms, independent of MMIO.
package Firmware_Tables.HPET with SPARK_Mode, Pure is
   type Descriptor (Valid : Boolean := False) is record
      case Valid is
         when True => Base : Root_Address;
         when False => null;
      end case;
   end record;

   function Decode (Data : Bytes; Last_Address : Address_Value) return Descriptor
     with Post =>
       (if Decode'Result.Valid then
          Decode'Result.Base mod 1024 = 0 and then
          Decode'Result.Base <= Last_Address and then
          Last_Address - Decode'Result.Base >= 1023);

   -- HPET general configuration: stop counting/interrupt generation and
   -- release legacy routing. All manufacturer/reserved bits are preserved.
   function Quiescent_Config (Value : Unsigned_32) return Unsigned_32 is
     (Value and not Unsigned_32'(3))
     with Post =>
       (Quiescent_Config'Result and 3) = 0 and
       (Quiescent_Config'Result and not Unsigned_32'(3)) =
         (Value and not Unsigned_32'(3));
end Firmware_Tables.HPET;
