with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
--  The PCH GMBUS I2C controller (DDC): EDID reads and SCDC register access
--  over an HDMI connector's DDC pins (DISP-004). Sequences follow i915 v6.16
--  intel_gmbus.c (do_gmbus_xfer, gmbus_xfer_index_read, gmbus_xfer_write,
--  gmbus_wait): index-cycle reads up to the controller's byte count, a STOP
--  cycle, idle wait, then GMBUS0 cleared; a NAK (SATOER) clears the
--  controller with SW_CLR_INT. Every wait has a deadline; every outcome is
--  a typed status for the log. Interrupts are not used (polled).
generic
   --  One GMBUS register (absolute MMIO offset), all ones when unreadable.
   with procedure Read (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : out Unsigned_32);
   with procedure Write
     (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : Unsigned_32; Written : out Boolean);
   --  Monotonic microseconds (Unsigned_64'Last when unavailable).
   with procedure Now_Us (Value : out Unsigned_64);
package Intel_GPU_GMBUS with SPARK_Mode is
   --  GMBUS pin pairs on ICP+ PCHs (i915 gmbus_pins_icp): DDI A/B and TC1..4.
   type Pin is (Pin_DDI_A, Pin_DDI_B, Pin_TC1, Pin_TC2, Pin_TC3, Pin_TC4);
   Pin_Code : constant array (Pin) of Unsigned_32 := [1, 2, 9, 10, 11, 12];

   --  7-bit I2C addresses.
   subtype Address is Unsigned_8 range 0 .. 16#7F#;
   DDC_EDID : constant Address := 16#50#;
   DDC_Segment : constant Address := 16#30#;
   SCDC : constant Address := 16#54#;

   --  Bytes per transfer: the GMBUS1 byte count (9 bits on gen9+, i915
   --  GEN9_GMBUS_BYTE_COUNT_MAX).
   Max_Transfer : constant := 511;
   subtype Transfer_Length is Positive range 1 .. Max_Transfer;
   type Bytes is array (Positive range <>) of Unsigned_8;

   type Status is (OK, Not_Writable, Busy_Before_Start, NAK, Timeout_Ready, Timeout_Wait_Phase,
                   Timeout_Idle);
   function Name (Item : Status) return String is
     (case Item is
        when OK => "ok", when Not_Writable => "gmbus-not-writable",
        when Busy_Before_Start => "busy-before-start", when NAK => "nak",
        when Timeout_Ready => "timeout-hw-ready", when Timeout_Wait_Phase => "timeout-wait-phase",
        when Timeout_Idle => "timeout-idle");

   --  Read Data'Length bytes from Device at register Index (index cycle:
   --  write Index, repeated start, read).
   procedure Read_Indexed
     (Line : Pin; Device : Address; Index : Unsigned_8; Data : out Bytes; Result : out Status)
     with Pre => Data'Length in Transfer_Length and then Data'First = 1;

   --  Write Data to Device (Data (1) is usually the register index).
   procedure Write_Bytes (Line : Pin; Device : Address; Data : Bytes; Result : out Status)
     with Pre => Data'Length in Transfer_Length and then Data'First = 1;

   --  EDID block Block (0 .. 255): segment Block / 2 (E-DDC) at offset
   --  (Block mod 2) * 128.
   EDID_Block_Bytes : constant := 128;
   subtype EDID_Block_Index is Natural range 0 .. 255;
   procedure Read_EDID_Block
     (Line : Pin; Block : EDID_Block_Index; Data : out Bytes; Result : out Status)
     with Pre => Data'Length = EDID_Block_Bytes and then Data'First = 1;
end Intel_GPU_GMBUS;
