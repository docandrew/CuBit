pragma Ada_2022;
with Interfaces;
with Pointer_Pending;
generic
   Maximum_Contacts : Positive;
   type Contact is private;
   with function Identity (Item : Contact) return Interfaces.Unsigned_32;
   with function Active (Item : Contact) return Boolean;
   with function Positioned (Item : Contact) return Boolean;
   with function Confidence_Known (Item : Contact) return Boolean;
   with function Confident (Item : Contact) return Boolean;
   with function X_Value (Item : Contact) return Long_Long_Integer;
   with function Y_Value (Item : Contact) return Long_Long_Integer;
package HID_Touchpad_Pointer with SPARK_Mode is
   type Coordinate is range -(2 ** 31) .. 2 ** 32 - 1;
   type Axis_Range is record
      Minimum, Maximum : Coordinate := 0;
   end record;
   type Gain_Numerator is range 1 .. 2 ** 16 - 1;
   type Gain_Denominator is range 1 .. 2 ** 31 - 1;
   type Gain is record
      Numerator : Gain_Numerator := 1;
      Denominator : Gain_Denominator := 1;
   end record;
   type Direction is (As_Reported, Reversed);
   type Confidence_Policy is (Require_Confident, Accept_Unknown);
   type Contact_Array is array (Positive range <>) of Contact;
   type Status is (Policy_Configured, Invalid_Configuration, Scale_Refused,
      Pointer_Changed, Pointer_Unchanged, Policy_Not_Configured, Invalid_Frame,
      Reset_Done);
   type Decision is record
      Outcome : Status := Policy_Not_Configured;
      Emit : Boolean := False;
      Data : Pointer_Pending.Report;
   end record;
   type State is limited private;
   function Ready (S : State) return Boolean;
   function Tracking (S : State) return Boolean;
   function Primary (S : State) return Interfaces.Unsigned_32 with Pre => Tracking (S);
   function Admissible (Range_Value : Axis_Range; Scale : Gain) return Boolean;
   procedure Configure
     (S : in out State; X_Range, Y_Range : Axis_Range;
      X_Gain, Y_Gain : Gain; X_Direction, Y_Direction : Direction;
      Confidence : Confidence_Policy; Result : out Decision);
   -- Explicit policy; no hardware range, orientation, sensitivity or confidence
   -- default is admitted implicitly. The worst full-axis displacement, including
   -- fractional carry, must fit one relative-pointer report; other scales are
   -- refused rather than clipped. Reconfiguration releases held buttons.
   procedure Rebase (S : in out State);
   function Current_Buttons (S : State) return Pointer_Pending.Button_State;
   procedure Reset (S : in out State; Result : out Decision);
   -- Lose contact baseline/fractional carry and release buttons; retain policy.
   procedure Update
     (S : in out State; Samples : Contact_Array;
      Known_Buttons, Pressed_Buttons : Pointer_Pending.Button_State;
      Result : out Decision);
   -- Completed frames only. Keep the eligible primary ID while present; new
   -- primary/liftoff rebase without a cursor jump. Unknown confidence follows
   -- explicit policy; known non-confident contacts never move the pointer.
   -- Unknown button bits retain their state. Native owner maps HID usages to
   -- button bits explicitly. Enqueue every Emit result even on reset/error.
   -- Getter functions must be pure/deterministic. No IPC/publication occurs.
private
   type Motion_Math is range -(2 ** 62) .. 2 ** 62 - 1;
   type State is limited record
      Configured, Has_Primary : Boolean := False;
      ID : Interfaces.Unsigned_32 := 0;
      Last_X, Last_Y : Coordinate := 0;
      Remainder_X, Remainder_Y : Motion_Math := 0;
      X_Range, Y_Range : Axis_Range;
      X_Gain, Y_Gain : Gain;
      X_Direction, Y_Direction : Direction := As_Reported;
      Confidence : Confidence_Policy := Require_Confident;
      Buttons : Pointer_Pending.Button_State := 0;
   end record;
   function Ready (S : State) return Boolean is (S.Configured);
   function Tracking (S : State) return Boolean is (S.Has_Primary);
   function Primary (S : State) return Interfaces.Unsigned_32 is (S.ID);
   function Current_Buttons (S : State) return Pointer_Pending.Button_State is (S.Buttons);
end HID_Touchpad_Pointer;
