with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
with Intel_GPU_Port_Detection;
--  DISP-009: hot-plug interrupts, in i915 v6.16 order (gen11_irq_handler,
--  gen11_display_irq_handler, gen8_de_irq_handler, gen11_hpd_irq_handler,
--  icp_irq_handler; arming as gen3_irq_init, icp_hpd_irq_setup and
--  gen11_hpd_irq_setup). Only HPD sources: the PCH's port pins (SDE) and
--  the north display's Type-C pins (DE HPD). Every other display and GT
--  source stays masked (GPU-001).
--
--  Order at boot: Quiesce (master off, every GT and display interrupt
--  enable zero), devmgr's MSI enable (Intel_GPU_MSI_Enable), then Arm (the
--  hot-plug pins only). Writes go to three pages intel-gpu holds: the GT
--  interrupt page 0x190000 (Intel_GPU_Reset_Pages), the DE interrupt page
--  0x44000 (Intel_GPU_Display_Pages) and the PCH page 0xC4000
--  (Intel_GPU_Modeset_Pages.South_Hotplug).
generic
   with procedure Read (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : out Unsigned_32);
   with procedure Write (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : Unsigned_32);
package Intel_GPU_Hotplug_IRQ with SPARK_Mode is
   package R renames Intel_GPU_Display_Registers;
   package PD renames Intel_GPU_Port_Detection;

   --  Top-level registers (i915_reg.h, gen11 interrupt layout).
   GEN11_GFX_MSTR_IRQ : constant := 16#19_0010#;
   Master_Enable : constant Unsigned_32 := 16#8000_0000#;   --  GEN11_MASTER_IRQ
   Master_Display : constant Unsigned_32 := 16#0001_0000#;  --  GEN11_DISPLAY_IRQ
   GEN11_DISPLAY_INT_CTL : constant := 16#4_4200#;
   Display_Enable : constant Unsigned_32 := 16#8000_0000#;  --  GEN11_DISPLAY_IRQ_ENABLE
   Display_PCH : constant Unsigned_32 := 16#0080_0000#;     --  GEN11_DE_PCH_IRQ
   Display_HPD : constant Unsigned_32 := 16#0020_0000#;     --  GEN11_DE_HPD_IRQ
   All_Bits : constant Unsigned_32 := Unsigned_32'Last;

   --  Interrupt enables Quiesce zeroes: i915 gen11_gt_irq_reset (GT) and
   --  gen11_display_irq_reset (display). IMRs are left as they are, so the
   --  IIR bits intel-gpu polls (flip, underrun) still latch; with IER zero
   --  they never raise an interrupt.
   --  (No CCS or GSC enables: ADL-N has neither; i915 skips them too.)
   type GT_Enable is (Render_Copy, VCS_VECS, GuC_SG, GPM_WGBoxPerf, Crypto);
   GT_Enable_Register : constant array (GT_Enable) of R.Register_Offset :=
     [Render_Copy => 16#19_0030#, VCS_VECS => 16#19_0034#, GuC_SG => 16#19_0038#,
      GPM_WGBoxPerf => 16#19_003C#, Crypto => 16#19_0040#];
   GEN8_DE_PIPE_IER_A : constant := 16#4_440C#;
   Pipe_Interrupt_Stride : constant := 16#10#;
   GEN8_DE_PORT_IER : constant := 16#4_444C#;
   GEN8_DE_MISC_IER : constant := 16#4_446C#;
   GEN11_GU_MISC_IER : constant := 16#4_44FC#;
   function Pipe_IER (Item : R.Transcoder) return R.Register_Offset is
     (GEN8_DE_PIPE_IER_A + Pipe_Interrupt_Stride * R.Transcoder'Pos (Item));
   --  Pipes whose power well is up (only their registers are written).
   type Pipe_Set is array (R.Transcoder) of Boolean;

   type Port_Set is array (PD.Port) of Boolean;
   --  i915 HOTPLUG_CTL status field: no detect, short pulse, long pulse.
   type Pulse is (No_Pulse, Short_Pulse, Long_Pulse);
   type Pulses is array (PD.Port) of Pulse;

   --  The interrupt bits of the ports: SDE (combo pins, Type-C legacy) and
   --  DE HPD (Type-C DP-alt and TBT).
   function South_Bits (Ports : Port_Set) return Unsigned_32;
   function North_Bits (Ports : Port_Set) return Unsigned_32;

   --  Arm detection and interrupts for Ports: clear stale IIR bits, enable
   --  each pin's detection (HOTPLUG_CTL read-modify-write), set IER, then
   --  unmask the pins (IMR) last, then the display and master enables.
   procedure Arm (Ports : Port_Set);

   --  Before the MSI is enabled: the master off, then every interrupt
   --  enable zero (GT; the powered pipes, DE port, misc, GU misc, DE HPD;
   --  SDE). Quiet: read back, the master and display enables are clear and
   --  every enable written reads zero.
   procedure Quiesce (Powered : Pipe_Set; Quiet : out Boolean);

   --  One interrupt: master off first, the display control off, the DE HPD
   --  IIR acknowledged then its HOTPLUG_CTL status latches cleared (write
   --  back), the SDE IIR acknowledged then its HOTPLUG_CTL latches, the
   --  display control back on, the master back on last. Fired: the ports
   --  whose IIR bit was set, with the pulse their latch recorded.
   --  Unexpected: the master or the display control named a source other
   --  than the hot-plug ones (GT, PCU, pipes, ports): left pending, never
   --  acknowledged here; with Quiesce those are masked, so it is a fault.
   procedure Handle (Fired : out Pulses; Unexpected : out Boolean);

   --  Mask one storming pin (IMR bit set), as i915's storm handling does.
   procedure Mask (Port : PD.Port);
   procedure Unmask (Port : PD.Port);
end Intel_GPU_Hotplug_IRQ;
