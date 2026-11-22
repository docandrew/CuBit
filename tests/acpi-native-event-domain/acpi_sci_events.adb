with ACPI_One_Service;
package body ACPI_SCI_Events is
   Owner : Server_Reference;
   procedure Observe_Server(Server : Server_Reference) is
   begin
      if Owner /= null or else Server = null then raise Program_Error with "model owner binding"; end if;
      Owner := Server;
   end Observe_Server;
   procedure Check_Source(GPE : ACPI_SCI_Registers.GPE_Number) is
   begin
      if not GPE_Available(GPE) then raise Program_Error with "unmodeled GPE"; end if;
   end Check_Source;
   procedure Check_Quiet(GPE : ACPI_SCI_Registers.GPE_Number) is
      use type ACPI_One_Service.Runtime.Status.Phase;
   begin
      if not ACPI_One_Service.Root.Fields.Control_Idle or else
         ACPI_One_Service.Runtime.Current /= ACPI_One_Service.Runtime.Status.Idle then
         raise Program_Error with "GPE clear/enable before runtime/command quiet";
      end if;
      if GPE=General_Source and then (Owner=null or else
         ACPI_Requests.Notifications_Pending(Owner.all) or else
         ACPI_Requests.Retained_Results_Pending(Owner.all)) then
         raise Program_Error with "general GPE rearm before Notify/result retirement";
      end if;
   end Check_Quiet;
   procedure Mask_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome) is
   begin
      Check_Source(GPE); Masks:=Masks+1;
      Result:=(if Mask_Refused then Refused else Completed);
      if GPE=General_Source then
         General_Masks:=General_Masks+1;
         if Result=Completed then General_Enabled:=False;end if;
      elsif Result=Completed then Enabled:=False;end if;
   end Mask_GPE;
   procedure Clear_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome) is
   begin
      Check_Source(GPE);Check_Quiet(GPE);
      if (if GPE=General_Source then General_Enabled else Enabled) then
         raise Program_Error with "GPE status clear while enabled";
      end if;
      Clears:=Clears+1;Result:=(if Clear_Refused then Refused else Completed);
      if GPE=General_Source then General_Clears:=General_Clears+1;
      elsif Result=Completed then Latched:=False;end if;
   end Clear_GPE;
   procedure Enable_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome) is
   begin
      Check_Source(GPE);Check_Quiet(GPE);Enables:=Enables+1;
      Result:=(if Enable_Refused then Refused else Completed);
      if GPE=General_Source then
         General_Enables:=General_Enables+1;
         if Result=Completed then General_Enabled:=True;end if;
      elsif Result=Completed then Enabled:=True;end if;
   end Enable_GPE;
end ACPI_SCI_Events;
