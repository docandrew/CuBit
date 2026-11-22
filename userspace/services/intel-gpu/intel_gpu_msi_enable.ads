with Interfaces; use Interfaces;
with Intel_GPU_PCI_Power;
--  DISP-000-irq / DISP-009: devmgr's one allowlisted configuration write
--  that turns intel-gpu's MSI on (docs/msi-interrupts.md, "intel-gpu: the
--  hook for the next step", step 2), and the rule that admits it.
--
--  devmgr programmed the MSI address and data at enumeration with MSI
--  Enable clear (Prepare_MSI). The enable is one 16-bit write of the MSI
--  control word: Enable set, Multiple Message Enable 0 (one message), every
--  other bit as read. It is planned from a fresh configuration snapshot and
--  admitted once, only for the display-claim owner (intel-gpu), only after
--  devmgr's PCI interrupt-disable executor finished (MSI and MSI-X off,
--  INTx disabled), and only when the request reports the GPU's own sources
--  masked (Sources_Masked). That report is the driver's word: devmgr does
--  not read GPU registers. What it guarantees is that the enable cannot
--  run early or for anyone else.
package Intel_GPU_MSI_Enable with SPARK_Mode is
   --  devmgr request: word 0 = Sources_Masked, words 1 .. 3 zero.
   Request_Label : constant := 16#023D#;
   --  "GEN11_GFX_MSTR_IRQ is clear and every GT and display enable but the
   --  hot-plug pins is zero" (Intel_GPU_Hotplug_IRQ.Quiesce read it back).
   Sources_Masked : constant Unsigned_64 := 16#4D53_4B44#;

   --  PCI Local Bus 3.0, 6.8.1: the MSI capability.
   Capability_Pointer : constant := 16#34#;
   Status_Register : constant := 6;
   Status_Capabilities : constant Unsigned_8 := 16#10#;
   Command_Register : constant := 4;
   Command_INTx_Disable : constant Unsigned_16 := 16#0400#;
   MSI_Capability_ID : constant Unsigned_8 := 16#05#;
   MSIX_Capability_ID : constant Unsigned_8 := 16#11#;
   MSI_Control_Offset : constant := 2;
   MSI_Address_Offset : constant := 4;
   MSIX_Control_Offset : constant := 2;
   MSI_Enable_Bit : constant Unsigned_16 := 16#0001#;
   MSI_Multiple_Enable : constant Unsigned_16 := 16#0070#;
   MSIX_Enable_Bit : constant Unsigned_16 := 16#8000#;
   --  x86 message address window (Intel SDM 11.11.1).
   Message_Address_Base : constant Unsigned_32 := 16#FEE0_0000#;
   Message_Address_Window : constant Unsigned_32 := 16#000F_FFFF#;
   First_Capability : constant := 16#40#;
   Last_Capability : constant := 16#FC#;
   Capability_Steps : constant := 48;

   subtype Config_Offset is Natural range 0 .. 254;
   type Plan (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Offset : Config_Offset;
            Before, After : Unsigned_16;
         when False => null;
      end case;
   end record;

   --  The control word write, from a fresh snapshot: valid only when the
   --  function has exactly one MSI capability, MSI and MSI-X are off, INTx
   --  is disabled and the message address is programmed (x86 window).
   function Plan_Enable (Data : Intel_GPU_PCI_Power.Configuration) return Plan
     with Post => (if Plan_Enable'Result.Valid then
                     Plan_Enable'Result.Offset mod 2 = 0 and then
                     (Plan_Enable'Result.Before and MSI_Enable_Bit) = 0 and then
                     Plan_Enable'Result.After =
                       ((Plan_Enable'Result.Before and not MSI_Multiple_Enable) or MSI_Enable_Bit));

   --  devmgr's admission.
   type Decision is
     (Enable, Refused_Not_Owner, Refused_Already_Attempted, Refused_Interrupts_Not_Disabled,
      Refused_Sources_Not_Masked, Refused_Configuration);
   function Decide
     (Requester_Is_Owner, Already_Attempted, PCI_Interrupts_Disabled : Boolean;
      Report : Unsigned_64; Fresh : Plan) return Decision is
     (if not Requester_Is_Owner then Refused_Not_Owner
      elsif Already_Attempted then Refused_Already_Attempted
      elsif not PCI_Interrupts_Disabled then Refused_Interrupts_Not_Disabled
      elsif Report /= Sources_Masked then Refused_Sources_Not_Masked
      elsif not Fresh.Valid then Refused_Configuration
      else Enable)
     with Post => (Decide'Result = Enable) =
       (Requester_Is_Owner and then not Already_Attempted and then PCI_Interrupts_Disabled and then
        Report = Sources_Masked and then Fresh.Valid);

   --  devmgr's admission of a hot-plug register page (Intel_GPU_Modeset_
   --  Pages.South_Hotplug): the display-claim owner, once.
   function Page_Allowed (Requester_Is_Owner, Already_Granted : Boolean) return Boolean is
     (Requester_Is_Owner and then not Already_Granted);

   function Name (Item : Decision) return String is
     (case Item is
        when Enable => "enable", when Refused_Not_Owner => "not-display-owner",
        when Refused_Already_Attempted => "already-attempted",
        when Refused_Interrupts_Not_Disabled => "pci-interrupts-not-disabled-first",
        when Refused_Sources_Not_Masked => "sources-not-reported-masked",
        when Refused_Configuration => "msi-configuration-not-ready");
end Intel_GPU_MSI_Enable;
