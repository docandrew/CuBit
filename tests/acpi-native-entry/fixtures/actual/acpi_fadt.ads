pragma Ada_2022;
with Interfaces; use Interfaces;
with Firmware_Tables;
-- ACPI 6.6 section 5.2.9 wire decoding. Descriptors are untrusted metadata,
-- never access authority. Structural admission does not validate hardware.
package ACPI_FADT with SPARK_Mode, Pure is
   type Generic_Address is record
      Space, Width, Bit_Offset, Access_Size : Unsigned_8 := 0;
      Address : Unsigned_64 := 0;
   end record;
   type Optional_Address is record
      Present : Boolean := False;
      Value : Generic_Address;
   end record;
   type Optional_Pointer is record
      Present : Boolean := False;
      Value : Unsigned_64 := 0;
   end record;
   type Block_Kind is (PM1A_Event, PM1B_Event, PM1A_Control, PM1B_Control,
                       PM2_Control, PM_Timer, GPE0, GPE1);
   type Legacy_Blocks is array (Block_Kind) of Unsigned_32;
   type Extended_Blocks is array (Block_Kind) of Optional_Address;
   type Block_Lengths is array (Block_Kind) of Unsigned_8;
   type Description is record
      Revision : Unsigned_8 := 0;
      FACS, DSDT : Unsigned_32 := 0;
      Profile : Unsigned_8 := 0;
      SCI : Unsigned_16 := 0;
      SMI_Command : Unsigned_32 := 0;
      Enable, Disable, S4_Request, P_State_Control : Unsigned_8 := 0;
      Legacy : Legacy_Blocks := [others => 0];
      Lengths : Block_Lengths := [others => 0];
      GPE1_Base, C_State_Control : Unsigned_8 := 0;
      C2_Latency, C3_Latency, Flush_Size, Flush_Stride : Unsigned_16 := 0;
      Duty_Offset, Duty_Width, Day_Alarm, Month_Alarm, Century : Unsigned_8 := 0;
      IA_PC_Boot : Unsigned_16 := 0;
      Flags : Unsigned_32 := 0;
      Reset : Optional_Address;
      Reset_Value_Present : Boolean := False;
      Reset_Value : Unsigned_8 := 0;
      ARM_Boot_Present : Boolean := False;
      ARM_Boot : Unsigned_16 := 0;
      Minor_Present : Boolean := False;
      Minor : Unsigned_8 := 0;
      X_FACS, X_DSDT : Optional_Pointer;
      Extended : Extended_Blocks;
      Sleep_Control, Sleep_Status : Optional_Address;
      Hypervisor : Optional_Pointer;
   end record;
   type Result (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Description;
         when False => null;
      end case;
   end record;
   -- Exact SDT extent and checksum required; optional fields are decoded only
   -- when their entire wire extent is present. Unknown revision/flag/space
   -- values are retained for a separate capability/compatibility decision.
   function Decode (Data : Firmware_Tables.Bytes) return Result
     with Post => (if Decode'Result.Valid then Data'Length >= 116);
   -- An addressability bound, not a grant or backing-memory validation.
   function Select_Pointer (Legacy : Unsigned_32; Extended : Optional_Pointer;
                            Last_Address : Unsigned_64) return Unsigned_64 is
     (if Extended.Present and then Extended.Value /= 0 and then Extended.Value <= Last_Address
      then Extended.Value
      elsif Unsigned_64 (Legacy) <= Last_Address then Unsigned_64 (Legacy)
      else 0)
     with Post => Select_Pointer'Result <= Last_Address;
   function Hardware_Reduced (Item : Description) return Boolean is
     (Item.Revision >= 5 and then (Item.Flags and 16#10_0000#) /= 0);
end ACPI_FADT;
