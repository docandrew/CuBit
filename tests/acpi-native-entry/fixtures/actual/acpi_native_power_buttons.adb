pragma Ada_2022;
with ACPI_Native_Power_Watches;
package body ACPI_Native_Power_Buttons is
   package W renames ACPI_Native_Power_Watches;
   use type W.Step_Outcome;
   Item : W.State;
   Configured, Fixed_Available, Method_Configured, Method_Available, Stopping : Boolean := False;
   procedure Configure_Fixed (Available : Boolean) is
   begin
      if Configured or Stopping then return;end if;
      Configured := True; Fixed_Available := Available;
      if Available then W.Mark_Present (Item);
      else W.Mark_Unavailable (Item);end if;
      -- No fixed source does not prove the absence of an AML/HID button.
   end Configure_Fixed;
   procedure Fixed_Press is
   begin
      if Configured and then Fixed_Available and then not Stopping then
         W.Record_Press (Item, W.E.Fixed_Event);
      end if;
   end Fixed_Press;
   procedure Configure_Control_Method (Available : Boolean) is
   begin
      if not Configured or Fixed_Available or Method_Configured or Stopping then return;end if;
      Method_Configured := True;Method_Available := Available;
      if Available then W.Mark_Present (Item);else W.Mark_Unavailable (Item);end if;
   end Configure_Control_Method;
   procedure Withdraw_Control_Method is
   begin
      Method_Available := False;
      if not Fixed_Available then W.Mark_Unavailable (Item);end if;
   end Withdraw_Control_Method;
   procedure Control_Method_Press is
   begin
      if Method_Available and then not Fixed_Available and then not Stopping then W.Record_Press (Item, W.E.Control_Method);end if;
   end Control_Method_Press;
   procedure Dispatch (Request : CuBit.Messages.Message; Owner_Quiet : Boolean;
      Reply : out CuBit.Messages.Message; Handled, Reply_Now : out Boolean) is
   begin W.Dispatch (Item, Request, Owner_Quiet, Reply, Handled, Reply_Now);end Dispatch;
   procedure Advance (Owner_Quiet : Boolean; Did_Work : out Boolean) is
      Result : W.Step_Outcome;
   begin W.Advance (Item, Owner_Quiet, Result);Did_Work := Result /= W.Idle;end Advance;
   procedure Begin_Stop is
   begin Stopping := True;W.Begin_Stop (Item);end Begin_Stop;
   function Quiet return Boolean is (W.Quiet (Item));
   function Retirement_Requested return Boolean is
     (not Stopping and then W.Retirement_Requested (Item));
end ACPI_Native_Power_Buttons;
