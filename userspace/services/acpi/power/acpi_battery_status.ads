pragma Ada_2022;
with Interfaces;
--  Pure decoding of the four integer elements extracted from _BST. This is
--  separate from evaluating AML, establishing device identity/presence, and
--  retaining/releasing package handles. ACPI 6.6 section 10.2.2.11.
package ACPI_Battery_Status with SPARK_Mode, Pure is
   subtype Raw_Integer is Interfaces.Unsigned_64;
   type Raw_Package is array (Positive range <>) of Raw_Integer;
   type Field is (No_Field, State_Field, Rate_Field, Capacity_Field, Voltage_Field);
   type Outcome is (Decoded, Wrong_Element_Count, Integer_Too_Wide,
                    Conflicting_Directions, Invalid_Measurement);
   type Direction is (Neither, Discharging, Charging);
   subtype Quantity_Value is Interfaces.Unsigned_32 range 0 .. 16#7FFF_FFFF#;
   --  Unknown is distinct from zero. Rate/capacity units require _BIX/_BIF;
   --  voltage is mV. Technology-dependent unknown restrictions belong to the
   --  owner once it has battery information, not this dynamic-only decoder.
   type Measurement is record
      Known : Boolean := False;
      Value : Quantity_Value := 0;
   end record;
   type Sample is record
      Flow : Direction := Neither;
      Critical, Charge_Limiting : Boolean := False;
      Reserved_State_Bits : Interfaces.Unsigned_32 := 0;
      Rate, Remaining_Capacity, Voltage : Measurement;
   end record;
   Empty_Sample : constant Sample := (others => <>);
   type Decode_Result is record
      Status : Outcome := Wrong_Element_Count;
      Failure_Field : Field := No_Field;
      Value : Sample := Empty_Sample;
   end record;
   Expected_Elements : constant Positive := 4;
   Unknown_Quantity : constant Raw_Integer := 16#FFFF_FFFF#;
   Maximum_Quantity : constant Raw_Integer := 16#7FFF_FFFF#;
   --  Preserve unknown state bits for diagnostics/future versions. Never
   --  truncate an AML integer to DWORD. A refusal returns no partial sample.
   --  Neither means no direction reported; it does not imply full or absent.
   function Decode (Values : Raw_Package) return Decode_Result;
end ACPI_Battery_Status;
