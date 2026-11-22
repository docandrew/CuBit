with CuBit.Messages;
package ACPI_Native_Power_Buttons is
   Configured : Boolean := False;
   Offered_Presses : Natural := 0;
   procedure Configure_Fixed (Available : Boolean);
   procedure Fixed_Press;
   procedure Dispatch (Request : CuBit.Messages.Message; Owner_Quiet : Boolean;
      Reply : out CuBit.Messages.Message; Handled, Reply_Now : out Boolean);
   procedure Begin_Stop;
   --  The event domain withdraws the button sources when event start fails
   --  or the SCI is unavailable; the stand-in only counts it.
   Withdrawals : Natural := 0;
   procedure Withdraw_Sources;
end ACPI_Native_Power_Buttons;
