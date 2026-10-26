------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a bring-up step reports: success, or why it stopped and which
--  register said so. Every stage logs one line from this.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with RTW89_Registers; use RTW89_Registers;

package RTW89_Outcomes with Pure is

   type Reason is
     (None,
      Not_Responding,      --  MMIO reads all-ones
      Poll_Timeout,        --  a status bit never reached its value
      XTAL_Timeout,        --  crystal serial interface stuck
      MDIO_Timeout,
      DBI_Timeout,
      Unsupported_Link,    --  PCIe PHY rate not Gen1/Gen2
      DMAC_Disabled,       --  DMAC function bits not set when required
      DMA_Unreachable,     --  DMA region above what the device can address
      Efuse_Malformed,
      Efuse_Address_Unusable,
      Firmware_Missing,
      Firmware_Rejected,   --  container or header refused by our parser
      Firmware_Ring_Full,
      Firmware_Download_Failed,
      Firmware_Not_Ready,
      CMAC_Disabled,       --  CMAC 0 function bits not set when required
      Page_Count_Mismatch, --  HCI flow control's public pages do not add up
      H2C_Ring_Full,       --  a command to the firmware was not consumed
      Table_Unconfigured,  --  a BB/RF table has no configuration for this board
      Table_Aborted,       --  a BB/RF table's branches matched nothing
      Calibration_Failed,  --  an RF calibration step did not complete
      Synthesizer_Unlocked); --  the RF synthesizer did not lock on a channel

   type Outcome is record
      Why    : Reason := None;
      Where  : Register := 0;
      Value  : Unsigned_32 := 0;
   end record;

   Success : constant Outcome := (None, 0, 0);

   function OK (Result : Outcome) return Boolean is (Result.Why = None);

   function Failure (Why : Reason; Where : Register; Value : Unsigned_32)
     return Outcome is ((Why, Where, Value))
   with Pre => Why /= None;

   function Name (Why : Reason) return String is
     (case Why is
        when None                     => "none",
        when Not_Responding           => "not-responding",
        when Poll_Timeout             => "poll-timeout",
        when XTAL_Timeout             => "xtal-si-timeout",
        when MDIO_Timeout             => "mdio-timeout",
        when DBI_Timeout              => "dbi-timeout",
        when Unsupported_Link         => "unsupported-pcie-rate",
        when DMAC_Disabled            => "dmac-disabled",
        when DMA_Unreachable          => "dma-unreachable",
        when Efuse_Malformed          => "efuse-malformed",
        when Efuse_Address_Unusable   => "efuse-mac-unusable",
        when Firmware_Missing         => "firmware-missing",
        when Firmware_Rejected        => "firmware-rejected",
        when Firmware_Ring_Full       => "fwcmd-ring-full",
        when Firmware_Download_Failed => "fwdl-failed",
        when Firmware_Not_Ready       => "firmware-not-ready",
        when CMAC_Disabled            => "cmac-disabled",
        when Page_Count_Mismatch      => "hfc-page-mismatch",
        when H2C_Ring_Full            => "h2c-ring-full",
        when Table_Unconfigured       => "table-unconfigured",
        when Table_Aborted            => "table-aborted",
        when Calibration_Failed       => "calibration-failed",
        when Synthesizer_Unlocked     => "synthesizer-unlocked");

end RTW89_Outcomes;
