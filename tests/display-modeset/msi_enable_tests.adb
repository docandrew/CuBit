--  DISP-000-irq / DISP-009: devmgr's MSI enable for intel-gpu and the
--  hot-plug page grant (Intel_GPU_MSI_Enable). The enable is refused for
--  any requester but the display-claim owner, a second time, before the
--  PCI interrupt-disable executor finished, without the masked report, and
--  when the configuration is not exactly "one MSI capability, programmed,
--  off; MSI-X off; INTx disabled". When admitted it is one control-word
--  write: Enable set, one message, every other bit kept.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_PCI_Power;
with Intel_GPU_MSI_Enable;
procedure MSI_Enable_Tests is
   package M renames Intel_GPU_MSI_Enable;
   use type M.Decision;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "msi enable check failed: " & What;
      end if;
   end Check;
   MSI_At : constant := 16#AC#;
   MSIX_At : constant := 16#D0#;
   --  An ADL-N-like configuration after devmgr's Prepare_MSI and the
   --  interrupt-disable executor: INTx disabled, MSI (64-bit, address and
   --  data programmed) off, MSI-X off.
   function Ready_Config return Intel_GPU_PCI_Power.Configuration is
      Data : Intel_GPU_PCI_Power.Configuration := [others => 0];
   begin
      Data (M.Command_Register + 1) := Unsigned_8 (Shift_Right (M.Command_INTx_Disable, 8));
      Data (M.Status_Register) := M.Status_Capabilities;
      Data (M.Capability_Pointer) := MSI_At;
      Data (MSI_At) := M.MSI_Capability_ID;
      Data (MSI_At + 1) := MSIX_At;
      Data (MSI_At + 2) := 16#80#;   --  64-bit address capable, Multiple Message Capable 0
      Data (MSI_At + 4) := 16#00#;
      Data (MSI_At + 5) := 16#20#;   --  0xFEE02000: CPU 2
      Data (MSI_At + 6) := 16#E0#;
      Data (MSI_At + 7) := 16#FE#;
      Data (MSIX_At) := M.MSIX_Capability_ID;
      Data (MSIX_At + 1) := 0;
      return Data;
   end Ready_Config;
   Data : Intel_GPU_PCI_Power.Configuration;
   P : M.Plan;
begin
   Data := Ready_Config;
   P := M.Plan_Enable (Data);
   Check (P.Valid, "ready configuration");
   if P.Valid then
      Check (P.Offset = MSI_At + M.MSI_Control_Offset and then P.Before = 16#80# and then P.After = 16#81#,
             "one word: MSI control, Enable set, the rest kept");
   end if;
   --  Multiple Message Enable is cleared (one message).
   Data (MSI_At + 2) := 16#80# or 16#20#;
   P := M.Plan_Enable (Data);
   Check (P.Valid and then P.After = 16#81#, "one message");
   --  Configurations that refuse.
   Data := Ready_Config;
   Data (MSI_At + 2) := 16#81#;
   Check (not M.Plan_Enable (Data).Valid, "already enabled");
   Data := Ready_Config;
   Data (MSI_At + 7) := 0;
   Check (not M.Plan_Enable (Data).Valid, "address not programmed");
   Data := Ready_Config;
   Data (MSIX_At + 3) := 16#80#;
   Check (not M.Plan_Enable (Data).Valid, "MSI-X on");
   Data := Ready_Config;
   Data (M.Command_Register + 1) := 0;
   Check (not M.Plan_Enable (Data).Valid, "INTx not disabled");
   Data := Ready_Config;
   Data (M.Capability_Pointer) := MSIX_At;
   Check (not M.Plan_Enable (Data).Valid, "no MSI capability");
   Data := Ready_Config;
   Data (MSIX_At) := M.MSI_Capability_ID;
   Check (not M.Plan_Enable (Data).Valid, "two MSI capabilities");
   Data := Ready_Config;
   Data (MSI_At + 1) := MSI_At;
   Check (not M.Plan_Enable (Data).Valid, "capability loop");
   Data := Ready_Config;
   Data (M.Capability_Pointer) := 16#21#;
   Check (not M.Plan_Enable (Data).Valid, "bad capability pointer");

   --  Admission: every refusal, and the one way through.
   P := M.Plan_Enable (Ready_Config);
   Check (M.Decide (True, False, True, M.Sources_Masked, P) = M.Enable, "owner, first, after disable, masked");
   Check (M.Decide (False, False, True, M.Sources_Masked, P) = M.Refused_Not_Owner, "not intel-gpu");
   Check (M.Decide (True, True, True, M.Sources_Masked, P) = M.Refused_Already_Attempted, "once");
   Check (M.Decide (True, False, False, M.Sources_Masked, P) = M.Refused_Interrupts_Not_Disabled,
          "before the disable executor");
   Check (M.Decide (True, False, True, 0, P) = M.Refused_Sources_Not_Masked, "sources not reported masked");
   Check (M.Decide (True, False, True, M.Sources_Masked + 1, P) = M.Refused_Sources_Not_Masked, "wrong report");
   Check (M.Decide (True, False, True, M.Sources_Masked, (Valid => False)) = M.Refused_Configuration,
          "configuration");
   --  The hot-plug page: the display-claim owner, once.
   Check (M.Page_Allowed (True, False) and then not M.Page_Allowed (False, False) and then
          not M.Page_Allowed (True, True), "page grant");
   Put_Line ("PASS MSI enable and hot-plug page admission:" & Checks'Image & " checks");
end MSI_Enable_Tests;
