with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
--  DisplayPort AUX channel (DISP-004): native DPCD reads and EDID over
--  I2C-over-AUX for DP and eDP outputs. Register use and retry rules from
--  i915 v6.16 intel_dp_aux.c (intel_dp_aux_xfer, skl_get_aux_send_ctl,
--  intel_dp_aux_pack): wait for SEND_BUSY clear, write the message to
--  DP_AUX_CH_DATA1..5 (big-endian words), start with the control word,
--  wait for DONE, clear the sticky status; up to five tries on a timeout or
--  receive error. DEFER replies are retried (DRM core drm_dp_i2c_xfer).
--  eDP panels need VDD for AUX: a panel the firmware lit has it.
generic
   --  One AUX register (absolute MMIO offset), all ones when unreadable.
   with procedure Read (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : out Unsigned_32);
   with procedure Write
     (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : Unsigned_32; Written : out Boolean);
   with procedure Now_Us (Value : out Unsigned_64);
package Intel_GPU_DP_AUX with SPARK_Mode is
   --  AUX channels A, B and the Type-C ports (TGL+: DP_AUX_CH_CTL (ch) =
   --  0x64010 + 0x100 * ch, ch = the DDI's number).
   type Channel is (Channel_A, Channel_B, Channel_C, Channel_TC1, Channel_TC2, Channel_TC3, Channel_TC4);

   --  A transfer carries at most 16 data bytes (DP AUX).
   Max_Payload : constant := 16;
   subtype Payload_Length is Positive range 1 .. Max_Payload;
   type Bytes is array (Positive range <>) of Unsigned_8;

   type Status is (OK, Not_Writable, Busy, Timeout, Receive_Error, NAK, Defer_Exhausted, Short_Reply);
   function Name (Item : Status) return String is
     (case Item is
        when OK => "ok", when Not_Writable => "aux-not-writable", when Busy => "busy",
        when Timeout => "timeout", when Receive_Error => "receive-error", when NAK => "nak",
        when Defer_Exhausted => "defer-exhausted", when Short_Reply => "short-reply");

   --  Native DPCD read of Data'Length bytes at Address (20-bit).
   subtype DPCD_Address is Unsigned_32 range 0 .. 16#F_FFFF#;
   procedure Read_DPCD (Line : Channel; Address : DPCD_Address; Data : out Bytes; Result : out Status)
     with Pre => Data'Length in Payload_Length and then Data'First = 1;

   --  EDID block 0 .. 255 over I2C-over-AUX (segment pointer 0x30 for
   --  blocks 2 and up, offset write and 16-byte reads at 0x50).
   EDID_Block_Bytes : constant := 128;
   subtype EDID_Block_Index is Natural range 0 .. 255;
   procedure Read_EDID_Block (Line : Channel; Block : EDID_Block_Index; Data : out Bytes; Result : out Status)
     with Pre => Data'Length = EDID_Block_Bytes and then Data'First = 1;
end Intel_GPU_DP_AUX;
