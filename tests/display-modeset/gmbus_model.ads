--  A register-level model of the PCH GMBUS controller and one DDC bus: an
--  EDID EEPROM at 0x50 (with an E-DDC segment pointer at 0x30) and an SCDC
--  register file at 0x54. It follows the GMBUS1/2/3 protocol i915 drives
--  (software-ready cycles, HW_RDY per data word, WAIT phase, STOP, NAK via
--  SATOER) closely enough to catch ordering mistakes; it is not silicon.
with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
package GMBUS_Model is
   package R renames Intel_GPU_Display_Registers;
   type Byte_Array is array (Natural range <>) of Unsigned_8;

   procedure Reset (EDID : Byte_Array; Connected_Pin : Unsigned_32);
   --  Faults to inject.
   No_Ready : Boolean := False;     --  HW_RDY never sets (timeout)
   Not_Writable : Boolean := False; --  writes are refused (page not granted)

   procedure Read (Offset : R.Register_Offset; Value : out Unsigned_32);
   procedure Write (Offset : R.Register_Offset; Value : Unsigned_32; Written : out Boolean);
   procedure Now_Us (Value : out Unsigned_64);

   --  Observations.
   function SCDC_Register (Index : Unsigned_8) return Unsigned_8;
   function Stops return Natural;
   function Released return Boolean;  --  GMBUS0 cleared and bus idle
   function Protocol_Errors return Natural;
end GMBUS_Model;
