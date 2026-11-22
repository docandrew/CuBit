--  A DisplayPort sink behind the AUX channel registers: DPCD and an EDID
--  EEPROM (segment pointer 0x30, offset at 0x50, I2C-over-AUX with MOT).
--  Faults: DEFER a number of times, NAK, a receive error once, no reply.
with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
package AUX_Model is
   package R renames Intel_GPU_Display_Registers;
   type Byte_Array is array (Natural range <>) of Unsigned_8;
   procedure Reset (EDID : Byte_Array);
   Defers : Natural := 0;
   Nak_All : Boolean := False;
   Receive_Error_Once : Boolean := False;
   Never_Done : Boolean := False;
   procedure Read (Offset : R.Register_Offset; Value : out Unsigned_32);
   procedure Write (Offset : R.Register_Offset; Value : Unsigned_32; Written : out Boolean);
   procedure Now_Us (Value : out Unsigned_64);
   function DPCD (Address : Natural) return Unsigned_8;
end AUX_Model;
