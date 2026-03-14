------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Intel High Definition Audio controller driver definitions
--
--  @description
--  Register map, types and procedures for Intel HDA (Azalia) controllers.
--  Implements CORB/RIRB command transport, codec discovery, widget
--  configuration, and stream DMA (BDL) for PCM playback.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package HDA is

   ---------------------------------------------------------------------------
   --  Controller register offsets (Intel HDA spec section 3)
   ---------------------------------------------------------------------------
   REG_GCAP    : constant := 16#00#;   --  Global Capabilities (16-bit)
   REG_VMIN    : constant := 16#02#;   --  Minor Version (8-bit)
   REG_VMAJ    : constant := 16#03#;   --  Major Version (8-bit)
   REG_OUTPAY  : constant := 16#04#;   --  Output Payload Capability (16-bit)
   REG_INPAY   : constant := 16#06#;   --  Input Payload Capability (16-bit)
   REG_GCTL    : constant := 16#08#;   --  Global Control (32-bit)
   REG_WAKEEN  : constant := 16#0C#;   --  Wake Enable (16-bit)
   REG_STATESTS : constant := 16#0E#;  --  State Change Status (16-bit)
   REG_INTCTL  : constant := 16#20#;   --  Interrupt Control (32-bit)
   REG_INTSTS  : constant := 16#24#;   --  Interrupt Status (32-bit)

   --  Immediate Command Interface registers
   REG_ICW     : constant := 16#60#;   --  Immediate Cmd (32-bit)
   REG_IRR     : constant := 16#64#;   --  Immediate Resp (32-bit)
   REG_ICS     : constant := 16#68#;   --  Immediate Cmd Status (16)
   ICS_ICB     : constant Unsigned_16 := 16#0001#;  --  Busy
   ICS_IRV     : constant Unsigned_16 := 16#0002#;  --  Valid

   --  CORB registers
   REG_CORBLBASE : constant := 16#40#; --  CORB Lower Base (32-bit)
   REG_CORBUBASE : constant := 16#44#; --  CORB Upper Base (32-bit)
   REG_CORBWP    : constant := 16#48#; --  CORB Write Pointer (16-bit)
   REG_CORBRP    : constant := 16#4A#; --  CORB Read Pointer (16-bit)
   REG_CORBCTL   : constant := 16#4C#; --  CORB Control (8-bit)
   REG_CORBSTS   : constant := 16#4D#; --  CORB Status (8-bit)
   REG_CORBSIZE  : constant := 16#4E#; --  CORB Size (8-bit)

   --  RIRB registers
   REG_RIRBLBASE : constant := 16#50#; --  RIRB Lower Base (32-bit)
   REG_RIRBUBASE : constant := 16#54#; --  RIRB Upper Base (32-bit)
   REG_RIRBWP    : constant := 16#58#; --  RIRB Write Pointer (16-bit)
   REG_RINTCNT   : constant := 16#5A#; --  Response Interrupt Count (16-bit)
   REG_RIRBCTL   : constant := 16#5C#; --  RIRB Control (8-bit)
   REG_RIRBSTS   : constant := 16#5D#; --  RIRB Status (8-bit)
   REG_RIRBSIZE  : constant := 16#5E#; --  RIRB Size (8-bit)

   --  Stream Descriptor registers (output stream 0 at offset 0x80+0x20*N)
   --  For simplicity we use a single output stream (stream index 0).
   --  The actual offset depends on the number of input streams reported in
   --  GCAP.  We compute it at runtime.
   SD_CTL    : constant := 16#00#;     --  Stream Descriptor Control (24-bit)
   SD_STS    : constant := 16#03#;     --  Stream Descriptor Status (8-bit)
   SD_LPIB   : constant := 16#04#;     --  Link Position in Buffer (32-bit)
   SD_CBL    : constant := 16#08#;     --  Cyclic Buffer Length (32-bit)
   SD_LVI    : constant := 16#0C#;     --  Last Valid Index (16-bit)
   SD_FMT    : constant := 16#12#;     --  Format (16-bit)
   SD_BDLPL  : constant := 16#18#;     --  BDL Pointer Lower (32-bit)
   SD_BDLPU  : constant := 16#1C#;     --  BDL Pointer Upper (32-bit)

   --  GCTL bits
   GCTL_CRST   : constant Unsigned_32 := 1;         --  Controller Reset

   --  INTCTL bits
   INTCTL_GIE  : constant Unsigned_32 := 16#8000_0000#; --  Global Interrupt Enable
   INTCTL_CIE  : constant Unsigned_32 := 16#4000_0000#; --  Controller Interrupt Enable

   --  Stream descriptor control bits
   SD_CTL_RUN  : constant Unsigned_32 := 16#02#;    --  Stream Run
   SD_CTL_IOCE : constant Unsigned_32 := 16#04#;    --  Interrupt on Completion Enable
   SD_CTL_STRIPE1 : constant Unsigned_32 := 0;

   --  Stream descriptor status bits
   SD_STS_BCIS : constant Unsigned_8 := 16#04#;     --  Buffer Completion Interrupt

   --  CORB/RIRB control bits
   CORBCTL_RUN  : constant Unsigned_8 := 16#02#;
   RIRBCTL_RUN  : constant Unsigned_8 := 16#02#;
   RIRBCTL_INT  : constant Unsigned_8 := 16#01#;

   ---------------------------------------------------------------------------
   --  HDA codec verb building helpers
   ---------------------------------------------------------------------------
   --  Verb format: [codec(4)] [NID(8)] [verb+payload(20)]

   --  Common verb IDs (12-bit verbs with 8-bit payload)
   VERB_GET_PARAM       : constant Unsigned_32 := 16#F00_00#;
   VERB_SET_CONN_SEL    : constant Unsigned_32 := 16#701_00#;
   VERB_SET_PIN_CTRL    : constant Unsigned_32 := 16#707_00#;
   VERB_SET_AMP_GAIN    : constant Unsigned_32 := 16#300_00#;  --  3xx
   VERB_SET_STREAM_FMT  : constant Unsigned_32 := 16#200_00#;  --  2xx
   VERB_SET_CHAN_STREAM  : constant Unsigned_32 := 16#706_00#;
   VERB_SET_EAPD        : constant Unsigned_32 := 16#70C_00#;
   VERB_SET_POWER_STATE : constant Unsigned_32 := 16#705_00#;
   VERB_GET_CONN_LIST   : constant Unsigned_32 := 16#F02_00#;
   VERB_GET_PIN_CTRL    : constant Unsigned_32 := 16#F07_00#;

   --  Parameter IDs (for GET_PARAM verb payload)
   PARAM_VENDOR_ID      : constant Unsigned_32 := 16#00#;
   PARAM_NODE_COUNT     : constant Unsigned_32 := 16#04#;
   PARAM_FN_GROUP_TYPE  : constant Unsigned_32 := 16#05#;
   PARAM_AUDIO_CAPS     : constant Unsigned_32 := 16#09#;
   PARAM_PIN_CAPS       : constant Unsigned_32 := 16#0C#;
   PARAM_CONN_LIST_LEN  : constant Unsigned_32 := 16#0E#;
   PARAM_OUT_AMP_CAPS   : constant Unsigned_32 := 16#12#;

   --  Pin control bits
   PIN_OUT_ENABLE       : constant Unsigned_8 := 16#40#;

   --  Amp gain/mute set bits
   AMP_SET_OUTPUT       : constant Unsigned_32 := 16#80_00#;
   AMP_SET_LEFT         : constant Unsigned_32 := 16#20_00#;
   AMP_SET_RIGHT        : constant Unsigned_32 := 16#10_00#;

   ---------------------------------------------------------------------------
   --  Stream format register encoding (48kHz / 16-bit / stereo)
   ---------------------------------------------------------------------------
   --  Bits 14:    0 (PCM)
   --  Bits 13-11: 000 (48kHz base)
   --  Bits 10-8:  000 (48kHz mult = x1)
   --  Bits 7:     0
   --  Bits 6-4:   001 (16-bit)
   --  Bits 3-0:   0001 (2 channels - 1)
   FMT_48KHZ_16BIT_STEREO : constant Unsigned_16 := 16#0011#;

   ---------------------------------------------------------------------------
   --  BDL (Buffer Descriptor List) entry
   ---------------------------------------------------------------------------
   type BDLEntry is record
      addr : Unsigned_64;    --  Physical address of buffer
      len  : Unsigned_32;    --  Length in bytes
      ioc  : Unsigned_32;    --  Bit 0: interrupt on completion
   end record
      with Convention => C, Size => 128;

   for BDLEntry use record
      addr at  0 range 0 .. 63;
      len  at  8 range 0 .. 31;
      ioc  at 12 range 0 .. 31;
   end record;

   MAX_BDL_ENTRIES : constant := 32;
   type BDLArray is array (0 .. MAX_BDL_ENTRIES - 1) of BDLEntry
      with Convention => C;

   ---------------------------------------------------------------------------
   --  Memory layout constants
   ---------------------------------------------------------------------------
   BAR_VIRT_BASE    : constant Unsigned_64 := 16#6000_0000_0000#;
   BAR_MAP_PAGES    : constant Unsigned_64 := 4;

   DMA_VIRT_BASE    : constant Unsigned_64 := 16#7000_0000_0000#;
   --  DMA layout (32 pages = 128KB):
   --    Page 0:    CORB (256 entries x 4 bytes = 1KB, padded to 4KB)
   --    Page 1:    RIRB (256 entries x 8 bytes = 2KB, padded to 4KB)
   --    Page 2:    BDL (32 entries x 16 bytes = 512 bytes, padded to 4KB)
   --    Pages 3-4: Staging buffer 0 (8KB = 2 pages)
   --    Pages 5-6: Staging buffer 1 (8KB = 2 pages, double-buffer)
   --    Pages 7+:  PCM data buffers for BDL entries
   DMA_CORB_OFF     : constant Unsigned_64 := 0;
   DMA_RIRB_OFF     : constant Unsigned_64 := 16#1000#;
   DMA_BDL_OFF      : constant Unsigned_64 := 16#2000#;
   DMA_STAGING0_OFF : constant Unsigned_64 := 16#3000#;
   DMA_STAGING1_OFF : constant Unsigned_64 := 16#5000#;
   DMA_PCMBUF_OFF   : constant Unsigned_64 := 16#7000#;

   DMA_ORDER         : constant Unsigned_64 := 5;   --  32 pages = 128KB
   DMA_PAGES         : constant Unsigned_64 := 32;

   --  PCM buffer size per BDL entry (1024 bytes = 256 samples stereo 16-bit)
   PCM_PERIOD_BYTES  : constant Unsigned_32 := 1024;
   --  Number of BDL entries to use (double-buffered periods)
   NUM_BDL_ENTRIES   : constant := 4;

   --  Staging buffer size (must match mixer grant region = 2 pages = 8KB)
   STAGING_SIZE      : constant Unsigned_32 := 8192;

   ---------------------------------------------------------------------------
   --  CORB/RIRB entry types
   ---------------------------------------------------------------------------
   CORB_SIZE : constant := 256;
   RIRB_SIZE : constant := 256;

   type CORBArray is array (0 .. CORB_SIZE - 1) of Unsigned_32
      with Convention => C;

   type RIRBEntry is record
      resp  : Unsigned_32;
      respEx : Unsigned_32;   --  codec addr (bits 3-0) + unsolicited flag
   end record
      with Convention => C, Size => 64;

   for RIRBEntry use record
      resp   at 0 range 0 .. 31;
      respEx at 4 range 0 .. 31;
   end record;

   type RIRBArray is array (0 .. RIRB_SIZE - 1) of RIRBEntry
      with Convention => C;

   ---------------------------------------------------------------------------
   --  Grant buffer layout (shared with mixer)
   ---------------------------------------------------------------------------
   GRANT_REGION_BASE : constant Unsigned_64 := 16#4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 256 * 4096;

   ---------------------------------------------------------------------------
   --  Procedures
   ---------------------------------------------------------------------------

   --  Initialize the HDA controller: reset, configure CORB/RIRB, discover
   --  codecs, configure output path, set up stream DMA.
   procedure initController;

   --  Send a verb to codec 0 and return the response.
   function sendVerb (nid  : Unsigned_8;
                      verb : Unsigned_32) return Unsigned_32;

   --  Configure the output path: find DAC + pin, set format, amp, pin enable.
   procedure configureOutput;

   --  Start playback stream DMA.
   procedure startStream;

   --  Stop playback stream DMA.
   procedure stopStream;

   --  Fill a BDL buffer slot with PCM data from the staging area.
   --  slotIdx: which BDL buffer (0..NUM_BDL_ENTRIES-1)
   --  srcOff:  byte offset into staging buffer to copy from
   --  numBytes: number of bytes to copy
   procedure fillBuffer (slotIdx  : Natural;
                         srcOff   : Unsigned_32;
                         numBytes : Unsigned_32);

   --  Get the current stream position (LPIB) in bytes.
   function getPosition return Unsigned_32;

   --  Write a sine-wave test pattern into a BDL buffer for testing.
   procedure writeSineTest (slotIdx : Natural);

   --  Number of output streams found on the controller.
   numOutputStreams : Natural := 0;

   --  First output stream descriptor base offset (computed from GCAP).
   outputStreamBase : Unsigned_64 := 0;

   --  Physical DMA base address (filled in during init).
   dmaPhysBase : Unsigned_64 := 0;

   --  Discovered codec info
   codecFound  : Boolean := False;
   codecVendor : Unsigned_32 := 0;

   --  Staging buffer base address in grant region (set by OP_AUDIO_HW_INIT)
   stagingBase : Unsigned_64 := GRANT_REGION_BASE;

   --  Discovered output path
   dacNID      : Unsigned_8 := 0;
   pinNID      : Unsigned_8 := 0;

end HDA;
