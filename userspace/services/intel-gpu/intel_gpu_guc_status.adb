package body Intel_GPU_GuC_Status with SPARK_Mode is
   function Decode (Value : Unsigned_32) return State is
      F : constant Fields := Split (Value);
   begin
      if Value = Unsigned_32'Last then return Invalid_MMIO; end if;
      if F.Auth = Auth_Bad or else
        F.Bootrom in Bootrom_No_Key_Found | Bootrom_Prod_Key_Check_Failure |
                     Bootrom_RSA_Failed
      then
         return Authentication_Failed;
      end if;
      if F.Bootrom in Bootrom_PAVPC_Failed | Bootrom_WOPCM_Failed |
                      Bootrom_Loadloc_Failed | Bootrom_Jump_Failed |
                      Bootrom_RC6_Context_Config_Failed |
                      Bootrom_MPU_Map_Incorrect | Bootrom_Exception
      then return Bootrom_Failed; end if;
      if F.Ukernel in Load_DevID_Build_Mismatch | Load_Preprod_Build_Mismatch |
                      Load_DevID_Invalid_GuC_Type | Load_HWConfig_Error |
                      Load_DPC_Error | Load_Exception | Load_Init_Data_Invalid |
                      Load_MPU_Data_Invalid | Load_Init_MMIO_Save_Restore_Invalid |
                      Load_KLV_Workaround_Init_Error
      then return Firmware_Failed; end if;
      if F.Ukernel = Load_Ready and then F.Auth = Auth_Good and then
        not F.MIA_In_Reset
      then return Ready; end if;
      return Pending;
   end Decode;

   function Bootrom_Name (Code : Bootrom_Code) return String is
     (case Code is
        when Bootrom_No_Key_Found => "no-key-found",
        when Bootrom_AES_Prod_Key_Found => "aes-prod-key-found",
        when Bootrom_Prod_Key_Check_Failure => "prod-key-check-failure",
        when Bootrom_RSA_Failed => "rsa-failed",
        when Bootrom_PAVPC_Failed => "pavpc-failed",
        when Bootrom_WOPCM_Failed => "wopcm-failed",
        when Bootrom_Loadloc_Failed => "loadloc-failed",
        when Bootrom_Jump_Passed => "jump-passed",
        when Bootrom_Jump_Failed => "jump-failed",
        when Bootrom_RC6_Context_Config_Failed => "rc6ctxconfig-failed",
        when Bootrom_MPU_Map_Incorrect => "mpumap-incorrect",
        when Bootrom_Exception => "exception",
        when others => "unknown");

   function Ukernel_Name (Code : Ukernel_Code) return String is
     (case Code is
        when Load_Default => "default",
        when Load_Start => "start",
        when Load_DevID_Build_Mismatch => "devid-build-mismatch",
        when Load_Preprod_Build_Mismatch => "preprod-build-mismatch",
        when Load_DevID_Invalid_GuC_Type => "devid-invalid-guctype",
        when Load_HWConfig_Start => "hwconfig-start",
        when Load_HWConfig_Done => "hwconfig-done",
        when Load_HWConfig_Error => "hwconfig-error",
        when Load_GDT_Done => "gdt-done",
        when Load_IDT_Done => "idt-done",
        when Load_LAPIC_Done => "lapic-done",
        when Load_GuCINT_Done => "gucint-done",
        when Load_DPC_Ready => "dpc-ready",
        when Load_DPC_Error => "dpc-error",
        when Load_Exception => "exception",
        when Load_Init_Data_Invalid => "init-data-invalid",
        when Load_PXP_Teardown_Ctrl_Enabled => "pxp-teardown-ctrl-enabled",
        when Load_MPU_Data_Invalid => "mpu-data-invalid",
        when Load_Init_MMIO_Save_Restore_Invalid => "init-mmio-save-restore-invalid",
        when Load_KLV_Workaround_Init_Error => "klv-workaround-init-error",
        when Load_Ready => "ready",
        when others => "unknown");

   Digits_16 : constant String (1 .. 16) := "0123456789ABCDEF";
   Nibble_Bits : constant := 4;
   Nibble_Mask : constant Unsigned_32 := 16#F#;

   --  Fixed-width "0x" & Width hex digits.
   function Hex (Value : Unsigned_32; Width : Positive) return String
     with Pre => Width <= 8, Post => Hex'Result'Length = Width + 2
   is
      Result : String (1 .. Width + 2) := [others => '0'];
      V : Unsigned_32 := Value;
   begin
      Result (2) := 'x';
      for I in reverse 3 .. Width + 2 loop
         Result (I) := Digits_16 (Natural (V and Nibble_Mask) + 1);
         V := Shift_Right (V, Nibble_Bits);
      end loop;
      return Result;
   end Hex;

   Bootrom_Digits : constant := 2;
   Ukernel_Digits : constant := 2;
   MIA_Digits : constant := 1;
   Raw_Digits : constant := 8;

   function Describe (Value : Unsigned_32) return String is
      F : constant Fields := Split (Value);
   begin
      if Value = Unsigned_32'Last then
         return "raw=" & Hex (Value, Raw_Digits) & " invalid-mmio";
      end if;
      return "raw=" & Hex (Value, Raw_Digits) &
        " mia-reset=" & (if F.MIA_In_Reset then "1" else "0") &
        " bootrom=" & Hex (F.Bootrom, Bootrom_Digits) & "(" & Bootrom_Name (F.Bootrom) & ")" &
        " ukernel=" & Hex (F.Ukernel, Ukernel_Digits) & "(" & Ukernel_Name (F.Ukernel) & ")" &
        " mia=" & Hex (F.MIA, MIA_Digits) &
        " auth=" & Auth_Name (F.Auth) &
        " state=" & State_Name (Decode (Value));
   end Describe;
end Intel_GPU_GuC_Status;
