package body Intel_GPU_MSI_Enable with SPARK_Mode is
   function Plan_Enable (Data : Intel_GPU_PCI_Power.Configuration) return Plan is
      function Word (Address : Config_Offset) return Unsigned_16 is
        (Unsigned_16 (Data (Address)) or Shift_Left (Unsigned_16 (Data (Address + 1)), 8));
      function Dword (Address : Natural) return Unsigned_32 is
        (Unsigned_32 (Data (Address)) or Shift_Left (Unsigned_32 (Data (Address + 1)), 8) or
         Shift_Left (Unsigned_32 (Data (Address + 2)), 16) or Shift_Left (Unsigned_32 (Data (Address + 3)), 24))
        with Pre => Address <= 252;
      Pointer : Natural := Natural (Data (Capability_Pointer));
      --  The MSI capability's offset (zero: none yet).
      MSI_At : Natural range 0 .. Last_Capability := 0;
      Last_Dword : constant := 252;
      MSIX_On : Boolean := False;
   begin
      if (Data (Status_Register) and Status_Capabilities) = 0 or else
        (Word (Command_Register) and Command_INTx_Disable) = 0
      then
         return (Valid => False);
      end if;
      for Step in 1 .. Capability_Steps loop
         pragma Loop_Invariant (MSI_At mod 4 = 0 and then Pointer <= Natural (Unsigned_8'Last));
         exit when Pointer = 0;
         if Pointer < First_Capability or else Pointer > Last_Capability or else Pointer mod 4 /= 0 then
            return (Valid => False);
         end if;
         if Data (Pointer) = MSI_Capability_ID then
            if MSI_At /= 0 then
               return (Valid => False);
            end if;
            MSI_At := Pointer;
         elsif Data (Pointer) = MSIX_Capability_ID then
            MSIX_On := MSIX_On or else (Word (Pointer + MSIX_Control_Offset) and MSIX_Enable_Bit) /= 0;
         end if;
         Pointer := Natural (Data (Pointer + 1));
      end loop;
      if Pointer /= 0 or else MSI_At = 0 or else MSIX_On or else MSI_At > Last_Dword - MSI_Address_Offset then
         return (Valid => False);
      end if;
      declare
         Control : constant Unsigned_16 := Word (MSI_At + MSI_Control_Offset);
         Address : constant Unsigned_32 := Dword (MSI_At + MSI_Address_Offset);
      begin
         if (Control and MSI_Enable_Bit) /= 0 or else
           (Address and not Message_Address_Window) /= Message_Address_Base
         then
            return (Valid => False);
         end if;
         return (Valid => True, Offset => MSI_At + MSI_Control_Offset, Before => Control,
                 After => (Control and not MSI_Multiple_Enable) or MSI_Enable_Bit);
      end;
   end Plan_Enable;
end Intel_GPU_MSI_Enable;
