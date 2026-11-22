pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Handles;
with CuBit.Power_Event_Exchange;
with ACPI_Power_Button_Watches;
package ACPI_Native_Power_Watches is
   package E renames CuBit.Power_Event_Exchange;
   subtype Actual_Source is E.Press_Source range E.Fixed_Event .. E.Intel_HID;
   type Step_Outcome is (Idle, Replied, Caller_Gone);
   type State is limited private;
   -- Trusted discovery facts: absent only after a complete successful scan.
   -- No transport or AML is performed by these fact/callback operations.
   procedure Mark_Present (Item : in out State);
   procedure Mark_Absent (Item : in out State);
   procedure Mark_Unavailable (Item : in out State);
   procedure Record_Press (Item : in out State; Source : Actual_Source);
   procedure Begin_Stop (Item : in out State);
   function Retirement_Requested (Item : State) return Boolean;
   function Pending (Item : State) return Boolean;
   function Quiet (Item : State) return Boolean;
   function Count (Item : State) return Interfaces.Unsigned_64;
   procedure Observe_Presses (Item : State; Count : out Interfaces.Unsigned_64;
      Source : out E.Press_Source);
   procedure Advance (Item : in out State; Owner_Quiet : Boolean; Result : out Step_Outcome);
   -- Request must be the kernel-received message. Caller identity/words are
   -- never authority. The native dispatcher must invoke this on every call
   -- before observer dispatch so Reply_Now is reset per request.
   procedure Dispatch (Item : in out State; Request : CuBit.Messages.Message;
      Owner_Quiet : Boolean; Reply : out CuBit.Messages.Message;
      Handled, Reply_Now : out Boolean);
private
   procedure Save_Current (Token : out CuBit.Handles.Reply; Saved : out Boolean);
   procedure Answer_Saved
     (Token : in out CuBit.Handles.Reply; ID : Interfaces.Unsigned_64;
      Status : E.Watch_Status; Press_Count : Interfaces.Unsigned_64;
      Source : E.Press_Source; Delivered : out Boolean);
   package Policy is new ACPI_Power_Button_Watches
     (Reply_Token => CuBit.Handles.Reply, No_Reply => CuBit.Handles.No_Reply,
      Initially_Discovering => True,
      Save_Current => Save_Current, Answer_Saved => Answer_Saved);
   type State is limited record Watches : Policy.State; end record;
end ACPI_Native_Power_Watches;
