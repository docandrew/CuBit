with ACPI_One_Service;
package body ACPI_SCI_Events is
   procedure Mask_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome) is
   begin
      if GPE/=16#6E# then raise Program_Error with "wrong GPE mask";end if;
      Masks:=Masks+1;Result:=(if Mask_Refused then Refused else Completed);
      if Result=Completed then Enabled:=False;end if;
   end Mask_GPE;
   procedure Clear_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome) is
      use type ACPI_One_Service.Runtime.Status.Phase;
   begin
      if GPE/=16#6E# or else Enabled or else not ACPI_One_Service.Root.Fields.Control_Idle
        or else ACPI_One_Service.Runtime.Current/=ACPI_One_Service.Runtime.Status.Idle then
         raise Program_Error with "status clear before quiet runtime/command";end if;
      Clears:=Clears+1;Result:=(if Clear_Refused then Refused else Completed);
      if Result=Completed then Latched:=False;end if;
   end Clear_GPE;
   procedure Enable_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome) is
      use type ACPI_One_Service.Runtime.Status.Phase;
   begin
      if GPE/=16#6E# or else not ACPI_One_Service.Root.Fields.Control_Idle
        or else ACPI_One_Service.Runtime.Current/=ACPI_One_Service.Runtime.Status.Idle then
         raise Program_Error with "enable before quiet runtime/command";end if;
      Enables:=Enables+1;Result:=(if Enable_Refused then Refused else Completed);
      if Result=Completed then Enabled:=True;end if;
   end Enable_GPE;
end ACPI_SCI_Events;
