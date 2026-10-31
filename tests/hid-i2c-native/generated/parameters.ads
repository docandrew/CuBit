--  Generated with the ELF manifest; do not edit.
--  Typed parameters of com.cubit.hid-i2c (docs/ccl-launch-parameters.md): one
--  HID_Parameters, given by its launcher and fixed for the process's lifetime.
--  Signature (.cubit.parameters), SHA-256
--  41B4BF30D05D9A2D77BB660330FD7815ACF97FF6C056C0B870D254FA35548B23:
--    (type HID_Register (range 0 65535))
--    (type HID_Gain_Numerator (range 1 65535))
--    (type HID_Gain_Denominator (range 1 2147483647))
--    (type HID_Direction (enum As_Reported Reversed))
--    (type HID_Axis (record (numerator HID_Gain_Numerator) (denominator HID_Gain_Denominator) (direction
--      HID_Direction)))
--    (type HID_Confidence (enum Require_Confident Accept_Unknown))
--    (type HID_Button (enum Ignore Primary_Button Secondary_Button Middle_Button Side_Button Extra_Button))
--    (type HID_Buttons (record (button_1 HID_Button) (button_2 HID_Button) (button_3 HID_Button)))
--    (type HID_Contact_Limit (range 1 32))
--    (type HID_Binding_Budget (range 1 4294967295))
--    (type HID_Milliseconds (range 1 9223372036854775807))
--    (type HID_Timing (record (startup HID_Milliseconds) (control HID_Milliseconds) (read HID_Milliseconds)
--      (cleanup HID_Milliseconds) (idle_backoff HID_Milliseconds)))
--    (type HID_Parameters (record (descriptor_register HID_Register) (x HID_Axis) (y HID_Axis) (confidence
--      HID_Confidence) (buttons HID_Buttons) (contact_limit HID_Contact_Limit) (binding_budget HID_Binding_Budget)
--      (timing HID_Timing)))
pragma Ada_2022;
with CuBit.Canonical_Readers;
with CuBit.Parameter_Signatures;

package Parameters with SPARK_Mode is
   subtype Text is CuBit.Canonical_Readers.Text;
   type HID_Register_Value is range 0 .. 65535;
   type HID_Gain_Numerator_Value is range 1 .. 65535;
   type HID_Gain_Denominator_Value is range 1 .. 2147483647;
   type HID_Direction_Value is (As_Reported, Reversed);
   type HID_Axis_Value is record
      numerator : HID_Gain_Numerator_Value := HID_Gain_Numerator_Value'First;
      denominator : HID_Gain_Denominator_Value := HID_Gain_Denominator_Value'First;
      direction : HID_Direction_Value := HID_Direction_Value'First;
   end record;
   type HID_Confidence_Value is (Require_Confident, Accept_Unknown);
   type HID_Button_Value is (Ignore, Primary_Button, Secondary_Button, Middle_Button, Side_Button, Extra_Button);
   type HID_Buttons_Value is record
      button_1 : HID_Button_Value := HID_Button_Value'First;
      button_2 : HID_Button_Value := HID_Button_Value'First;
      button_3 : HID_Button_Value := HID_Button_Value'First;
   end record;
   type HID_Contact_Limit_Value is range 1 .. 32;
   type HID_Binding_Budget_Value is range 1 .. 4294967295;
   type HID_Milliseconds_Value is range 1 .. 9223372036854775807;
   type HID_Timing_Value is record
      startup : HID_Milliseconds_Value := HID_Milliseconds_Value'First;
      control : HID_Milliseconds_Value := HID_Milliseconds_Value'First;
      read : HID_Milliseconds_Value := HID_Milliseconds_Value'First;
      cleanup : HID_Milliseconds_Value := HID_Milliseconds_Value'First;
      idle_backoff : HID_Milliseconds_Value := HID_Milliseconds_Value'First;
   end record;
   type HID_Parameters_Value is record
      descriptor_register : HID_Register_Value := HID_Register_Value'First;
      x : HID_Axis_Value := (others => <>);
      y : HID_Axis_Value := (others => <>);
      confidence : HID_Confidence_Value := HID_Confidence_Value'First;
      buttons : HID_Buttons_Value := (others => <>);
      contact_limit : HID_Contact_Limit_Value := HID_Contact_Limit_Value'First;
      binding_budget : HID_Binding_Budget_Value := HID_Binding_Budget_Value'First;
      timing : HID_Timing_Value := (others => <>);
   end record;

   --  The signature's identity: the SHA-256 of its text. procmgr hands over
   --  only a value checked against the signature with this identity.
   Identity : constant CuBit.Parameter_Signatures.Identity :=
     [16#41#, 16#B4#, 16#BF#, 16#30#, 16#D0#, 16#5D#, 16#9A#, 16#2D#,
      16#77#, 16#BB#, 16#66#, 16#03#, 16#30#, 16#FD#, 16#78#, 16#15#,
      16#AC#, 16#F9#, 16#7F#, 16#F6#, 16#C0#, 16#56#, 16#C0#, 16#B8#,
      16#70#, 16#D2#, 16#54#, 16#FA#, 16#35#, 16#54#, 16#8B#, 16#23#];

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a HID_Parameters. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out HID_Parameters_Value; Success : out Boolean)
   with Global => null, Always_Terminates;

   --  The value this process was launched with: read from its launch block
   --  (CuBit.Launch_Parameters) and decoded. The block never changes, so
   --  every call returns the same value; one without a valid value stops
   --  the program, saying why.
   function Value return HID_Parameters_Value
   with SPARK_Mode => Off;

   --  Value, for its check only: the Startup package calls it while it
   --  elaborates, so a program without valid parameters stops before main.
   procedure Require
   with SPARK_Mode => Off;
end Parameters;

