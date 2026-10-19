--  A register model of one display-engine output for the HDMI modeset
--  sequencer: registers hold what is written, and the status bits behave as
--  i915 waits for them (PLL power state and lock follow their requests,
--  TRANSCONF's state bit follows enable, DDI_BUF_CTL idles when disabled,
--  the frame counter and scanline move only while the pipe runs). It also
--  checks the ordering rules the sequence must keep, counting violations:
--  CFGCR written only while the PLL is powered and disabled; the DPCLKA
--  select changed only while the port clock is gated; TRANSCONF enabled
--  only with a locked PLL and an enabled DDI function; DDI_BUF_CTL enabled
--  only after the pipe runs; timings written only while the pipe is off.
with Interfaces; use Interfaces;
package Modeset_Model is
   procedure Reset;
   --  Faults.
   PLL_Never_Locks : Boolean := False;
   Pipe_Never_Starts : Boolean := False;
   Lock_Fault_Once : Boolean := False;  --  the first lock attempt fails only
   Underrun_On_Enable : Boolean := False;

   procedure Set (Offset, Value : Unsigned_32);  --  preload without checks
   function Get (Offset : Unsigned_32) return Unsigned_32;

   function Read (Offset : Unsigned_32) return Unsigned_32;
   procedure Write (Offset, Value : Unsigned_32; Written : out Boolean);
   function Now_Us return Unsigned_64;
   procedure Pause;
   procedure Write_TMDS_Config (Value : Unsigned_8; Done : out Boolean);
   procedure Log (Text : String);

   function Violations return Natural;
   function SCDC_Value return Unsigned_8;
   Verbose : Boolean := False;
end Modeset_Model;
