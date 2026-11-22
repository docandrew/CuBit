pragma Ada_2022;
package body ACPI_EC_Event_Policy with SPARK_Mode is
   procedure Bind (Item : in out State; Controller : E.Controller_ID;
      Epoch : E.Epoch_ID; Sender : P.Process_ID; Result : out Binding_Result) is
   begin
      Result := Binding_Decision (Item, Controller, Epoch, Sender);
      if Result = Bound then
         Item.Data := (Used => True, Enabled => True, Controller => Controller,
                       Epoch => Epoch, Sender => Sender, others => <>);
      end if;
   end Bind;
   procedure Revoke (Item : in out State; Controller : E.Controller_ID;
      Epoch : E.Epoch_ID; Accepted : out Boolean) is
   begin
      Accepted := Item.Data.Used and then Item.Data.Enabled
        and then Item.Data.Controller = Controller and then Item.Data.Epoch = Epoch;
      if Accepted then Item.Data.Enabled := False; end if;
   end Revoke;
   procedure Claim (Item : in out State; Sender : P.Process_ID;
      Event : E.Notice; Scope_Current : Boolean; Result : out Admission) is
   begin
      Result := Decision (Item, Sender, Event, Scope_Current);
      if Result = Admitted then
         Item.Data.Last := Event.Token; Item.Data.Last_Number := Event.Number;
         Item.Data.Held := True;
      end if;
   end Claim;
   procedure Guard (Item : in out State; Event : E.Notice; Accepted : out Boolean) is
   begin
      Accepted := Matches (Item, Event) and then Item.Data.Held
        and then not Item.Data.Uncertain and then not Item.Data.Guarding;
      if Accepted then Item.Data.Guarding := True; end if;
   end Guard;
   procedure Settle (Item : in out State; Event : E.Notice;
      Pending, Uncertain : Boolean; Accepted : out Boolean) is
   begin
      Accepted := Matches (Item, Event) and then Item.Data.Held;
      if Accepted then
         Item.Data.Held := Pending; Item.Data.Uncertain := Item.Data.Uncertain or Uncertain;
         Item.Data.Guarding := False;
      end if;
   end Settle;
end ACPI_EC_Event_Policy;
