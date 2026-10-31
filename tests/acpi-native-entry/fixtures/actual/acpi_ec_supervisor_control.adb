pragma Ada_2022;
package body ACPI_EC_Supervisor_Control with SPARK_Mode is
   procedure Enter_Advance (Item : in out State; Accepted : out Boolean) is
   begin
      Accepted := Item.Info.Stage in Fresh | Ready;
      if Accepted then Item.Info.Stage := Advance_Open; end if;
   end Enter_Advance;
   procedure Finish_Advance (Item : in out State; Terminal : Boolean) is
   begin Item.Info.Stage := (if Terminal then Stopped_State else Ready); end Finish_Advance;
   procedure Request_Stop (Item : in out State; Result : out Stop_Result) is
   begin
      Result := Stop_Decision (Item);
      if Result = Stop_Accepted then Item.Info.Stage := Stopped_State; end if;
   end Request_Stop;
   procedure Retain (Item : in out State; Event : E.Notice; Charge, Ceiling : Natural;
      Accepted : out Boolean) is
   begin
      Accepted := Retain_Allowed (Item, Event, Charge, Ceiling);
      if Accepted then Item.Info.Pending := (Present => True, Item => Event);
         Item.Info.Charge := Charge; Item.Info.Ceiling := Ceiling; end if;
   end Retain;
   procedure Confirm (Item : in out State; Expected : E.Notice; Accepted : out Boolean) is
   begin
      Accepted := Confirmation_Allowed (Item, Expected);
      if Accepted then Item.Info.Pending := (Present => False);
         Item.Info.Charge := 0; Item.Info.Ceiling := 0; end if;
   end Confirm;
   procedure Enter_Cleanup (Item : in out State; Accepted : out Boolean) is
   begin
      Accepted := Cleanup_Allowed (Item);
      if Accepted then Item.Info.Stage := Cleanup_Open; end if;
   end Enter_Cleanup;
   procedure Finish_Cleanup (Item : in out State) is
   begin Item.Info.Stage := Stopped_State; end Finish_Cleanup;
end ACPI_EC_Supervisor_Control;
