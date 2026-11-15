pragma Ada_2022;
with ACPI_Requests.Power_Preparation.With_Endpoint;
with ACPI_One_Service; with ACPI_Sleep_Entry;
package body ACPI_Native_Power_Preparation is
   package Q renames ACPI_Requests; package One renames ACPI_One_Service;
   package P renames ACPI_One_Service.Power_Preparation;
   use type Q.Phase, One.Runtime.Status.Phase;
   Runtime_Mode : Boolean := False;
   Item : P.Requests.Run;
   function Owner_Ready (Server : Q.State) return Boolean is
     (Runtime_Mode and then Q.Current (Server) = Q.Complete
      and then not Q.Notifications_Pending (Server) and then not Q.Retained_Results_Pending (Server)
      and then One.Runtime.Current = One.Runtime.Status.Idle and then One.Root.Hardware.Admitting
      and then One.Root.Fields.Control_Idle and then One.Root.Fields.EC_Commands.Quiescent);
   function Register_Model return P.Objects.Register_Model is
     (case One.Root.Hardware.Sleep_Register_Model is
        when ACPI_Sleep_Entry.PM1A_Only => P.Objects.PM1A_Only,
        when ACPI_Sleep_Entry.PM1A_And_B => P.Objects.PM1A_And_B,
        when ACPI_Sleep_Entry.Hardware_Reduced => P.Objects.Hardware_Reduced,
        when ACPI_Sleep_Entry.No_Registers => P.Objects.No_Sleep_Registers);
   package Endpoint is new P.Requests.With_Endpoint (P.Read_Clock, Register_Model, Owner_Ready);
   procedure Dispatch (Server : in out Q.State; Request : CuBit.Messages.Message;
      Runtime_Selected : Boolean; Reply : out CuBit.Messages.Message; Handled : out Boolean) is
      Result : Q.Packet;
   begin
      Runtime_Mode := Runtime_Selected;
      Endpoint.Dispatch (Server, Item, Request.authorityTag,
        (Label => Request.tag.label, Length => Request.tag.length, Flags => CuBit.Messages.Flag_Byte (Request.tag),
         Reserved => Request.tag.reserved, Data => Q.Words (Request.words)), Result, Handled);
      Reply := CuBit.Messages.NULL_MESSAGE;
      if not Handled then return; end if;
      Reply.tag := (label => Result.Label, length => Result.Length, protocol => 0, ipc => <>, reserved => Result.Reserved);
      Reply.words := CuBit.Messages.MessageWords (Result.Data);
   end Dispatch;
   procedure Invalidate is begin P.Requests.Invalidate (Item); end Invalidate;
   procedure Retire is begin P.Requests.Retire (Item); end Retire;
   function Current_Types (Server : Q.State) return P.Controller.Copied_Types is
     (if Owner_Ready (Server) then P.Requests.Current_Types (Server, Item) else P.Controller.Empty_Types);
end ACPI_Native_Power_Preparation;
