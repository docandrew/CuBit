with Endpoint_Test_Boundary;
package body ACPI_Native_Power_Buttons is
   procedure Configure_Control_Method(Available:Boolean) is begin Control_Configured:=Available;end;
   procedure Control_Method_Press is begin Offered_Presses:=Offered_Presses+1;end;
   procedure Withdraw_Control_Method is begin Control_Configured:=False;end;
   procedure Configure_Fixed (Available : Boolean) is
   begin Configured := Available; end Configure_Fixed;
   procedure Fixed_Press is
   begin Offered_Presses := Offered_Presses + 1; end Fixed_Press;
   procedure Dispatch (Request : CuBit.Messages.Message; Owner_Quiet : Boolean;
      Reply : out CuBit.Messages.Message; Handled, Reply_Now : out Boolean) is
      pragma Unreferenced(Request,Owner_Quiet);
   begin Endpoint_Test_Boundary.Pass_Through_Calls:=Endpoint_Test_Boundary.Pass_Through_Calls+1;
      Reply:=CuBit.Messages.NULL_MESSAGE;Handled:=False;Reply_Now:=True;end Dispatch;
   procedure Begin_Stop is begin null;end Begin_Stop;
   procedure Withdraw_Sources is begin Withdrawals := Withdrawals + 1; end Withdraw_Sources;
end ACPI_Native_Power_Buttons;
