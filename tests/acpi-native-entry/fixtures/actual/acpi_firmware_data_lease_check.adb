pragma Ada_2022;
package body ACPI_Firmware_Data_Lease_Check with SPARK_Mode is
   use type ACPI_Firmware_Data_Frames.Word, Requests.Reply_Kind, Owner.Outcome;
   procedure Check (Caller, Domain, Sequence : ACPI_Firmware_Data_Frames.Word;
      Accepted : out Boolean) is
      Reply : Requests.Reply;
      Who : Principal;
   begin
      Accepted := False;
      if Caller = 0 or else Domain = 0 or else Sequence = 0
        or else Authorized_Scope = No_Scope then return; end if;
      Who := Principal_From_Word (Caller);
      if Who = No_Principal then return; end if;
      Requests.Check_Access (Lock_State,Authorized_Scope,Who,(Domain,Sequence),Reply);
      Accepted := Reply.Kind = Requests.Checked and then Reply.Outcome = Owner.Configured;
   end Check;
end ACPI_Firmware_Data_Lease_Check;
