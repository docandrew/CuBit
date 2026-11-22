pragma Ada_2022;
with Interfaces; with CuBit.Power_Event_Exchange;
with CuBit.Messages;
-- Process-lived serialized owner. Discovery/callback operations copy facts only.
package ACPI_Native_Power_Buttons is
   procedure Configure_Fixed (Available : Boolean);
   procedure Fixed_Press;
   procedure Configure_Control_Method (Available : Boolean);
   procedure Withdraw_Control_Method;
   -- Terminal event-enrollment failure; settle any saved startup watch.
   procedure Withdraw_Sources;
   procedure Control_Method_Press;
   procedure Dispatch (Request : CuBit.Messages.Message; Owner_Quiet : Boolean;
      Reply : out CuBit.Messages.Message; Handled, Reply_Now : out Boolean);
   procedure Advance (Owner_Quiet : Boolean; Did_Work : out Boolean);
   procedure Begin_Stop;
   procedure Observe_Presses (Count : out Interfaces.Unsigned_64;
      Source : out CuBit.Power_Event_Exchange.Press_Source);
   function Quiet return Boolean;
   function Retirement_Requested return Boolean;
end ACPI_Native_Power_Buttons;
