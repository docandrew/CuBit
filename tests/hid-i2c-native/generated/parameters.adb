--  Generated with the ELF manifest; do not edit.
pragma Ada_2022;
with Interfaces;
with CuBit.Launch_Parameters;
with CuBit.Parameter_Sections;

package body Parameters with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_HID_Register
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Register_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (HID_Register_Value'First), Interfaces.Integer_64 (HID_Register_Value'Last), Value);
      Item := HID_Register_Value (Value);
   end Read_HID_Register;

   procedure Read_HID_Gain_Numerator
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Gain_Numerator_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (HID_Gain_Numerator_Value'First), Interfaces.Integer_64 (HID_Gain_Numerator_Value'Last), Value);
      Item := HID_Gain_Numerator_Value (Value);
   end Read_HID_Gain_Numerator;

   procedure Read_HID_Gain_Denominator
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Gain_Denominator_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (HID_Gain_Denominator_Value'First), Interfaces.Integer_64 (HID_Gain_Denominator_Value'Last), Value);
      Item := HID_Gain_Denominator_Value (Value);
   end Read_HID_Gain_Denominator;

   procedure Read_HID_Direction
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Direction_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := HID_Direction_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "HID_Direction.As_Reported") then
         R.Expect_Word (Source, Position, OK, "HID_Direction.As_Reported");
         Item := As_Reported;
      elsif R.Next_Is_Word (Source, Position, "HID_Direction.Reversed") then
         R.Expect_Word (Source, Position, OK, "HID_Direction.Reversed");
         Item := Reversed;
      else
         OK := False;
      end if;
   end Read_HID_Direction;

   procedure Read_HID_Axis
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Axis_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(HID_Axis");
      R.Expect (Source, Position, OK, " numerator => ");
      Read_HID_Gain_Numerator (Source, Position, OK, Item.numerator);
      R.Expect (Source, Position, OK, " denominator => ");
      Read_HID_Gain_Denominator (Source, Position, OK, Item.denominator);
      R.Expect (Source, Position, OK, " direction => ");
      Read_HID_Direction (Source, Position, OK, Item.direction);
      R.Expect (Source, Position, OK, ")");
   end Read_HID_Axis;

   procedure Read_HID_Confidence
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Confidence_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := HID_Confidence_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "HID_Confidence.Require_Confident") then
         R.Expect_Word (Source, Position, OK, "HID_Confidence.Require_Confident");
         Item := Require_Confident;
      elsif R.Next_Is_Word (Source, Position, "HID_Confidence.Accept_Unknown") then
         R.Expect_Word (Source, Position, OK, "HID_Confidence.Accept_Unknown");
         Item := Accept_Unknown;
      else
         OK := False;
      end if;
   end Read_HID_Confidence;

   procedure Read_HID_Button
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Button_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := HID_Button_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "HID_Button.Ignore") then
         R.Expect_Word (Source, Position, OK, "HID_Button.Ignore");
         Item := Ignore;
      elsif R.Next_Is_Word (Source, Position, "HID_Button.Primary_Button") then
         R.Expect_Word (Source, Position, OK, "HID_Button.Primary_Button");
         Item := Primary_Button;
      elsif R.Next_Is_Word (Source, Position, "HID_Button.Secondary_Button") then
         R.Expect_Word (Source, Position, OK, "HID_Button.Secondary_Button");
         Item := Secondary_Button;
      elsif R.Next_Is_Word (Source, Position, "HID_Button.Middle_Button") then
         R.Expect_Word (Source, Position, OK, "HID_Button.Middle_Button");
         Item := Middle_Button;
      elsif R.Next_Is_Word (Source, Position, "HID_Button.Side_Button") then
         R.Expect_Word (Source, Position, OK, "HID_Button.Side_Button");
         Item := Side_Button;
      elsif R.Next_Is_Word (Source, Position, "HID_Button.Extra_Button") then
         R.Expect_Word (Source, Position, OK, "HID_Button.Extra_Button");
         Item := Extra_Button;
      else
         OK := False;
      end if;
   end Read_HID_Button;

   procedure Read_HID_Buttons
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Buttons_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(HID_Buttons");
      R.Expect (Source, Position, OK, " button_1 => ");
      Read_HID_Button (Source, Position, OK, Item.button_1);
      R.Expect (Source, Position, OK, " button_2 => ");
      Read_HID_Button (Source, Position, OK, Item.button_2);
      R.Expect (Source, Position, OK, " button_3 => ");
      Read_HID_Button (Source, Position, OK, Item.button_3);
      R.Expect (Source, Position, OK, ")");
   end Read_HID_Buttons;

   procedure Read_HID_Contact_Limit
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Contact_Limit_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (HID_Contact_Limit_Value'First), Interfaces.Integer_64 (HID_Contact_Limit_Value'Last), Value);
      Item := HID_Contact_Limit_Value (Value);
   end Read_HID_Contact_Limit;

   procedure Read_HID_Binding_Budget
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Binding_Budget_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (HID_Binding_Budget_Value'First), Interfaces.Integer_64 (HID_Binding_Budget_Value'Last), Value);
      Item := HID_Binding_Budget_Value (Value);
   end Read_HID_Binding_Budget;

   procedure Read_HID_Milliseconds
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Milliseconds_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (HID_Milliseconds_Value'First), Interfaces.Integer_64 (HID_Milliseconds_Value'Last), Value);
      Item := HID_Milliseconds_Value (Value);
   end Read_HID_Milliseconds;

   procedure Read_HID_Timing
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Timing_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(HID_Timing");
      R.Expect (Source, Position, OK, " startup => ");
      Read_HID_Milliseconds (Source, Position, OK, Item.startup);
      R.Expect (Source, Position, OK, " control => ");
      Read_HID_Milliseconds (Source, Position, OK, Item.control);
      R.Expect (Source, Position, OK, " read => ");
      Read_HID_Milliseconds (Source, Position, OK, Item.read);
      R.Expect (Source, Position, OK, " cleanup => ");
      Read_HID_Milliseconds (Source, Position, OK, Item.cleanup);
      R.Expect (Source, Position, OK, " idle_backoff => ");
      Read_HID_Milliseconds (Source, Position, OK, Item.idle_backoff);
      R.Expect (Source, Position, OK, ")");
   end Read_HID_Timing;

   procedure Read_HID_Parameters
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out HID_Parameters_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(HID_Parameters");
      R.Expect (Source, Position, OK, " descriptor_register => ");
      Read_HID_Register (Source, Position, OK, Item.descriptor_register);
      R.Expect (Source, Position, OK, " x => ");
      Read_HID_Axis (Source, Position, OK, Item.x);
      R.Expect (Source, Position, OK, " y => ");
      Read_HID_Axis (Source, Position, OK, Item.y);
      R.Expect (Source, Position, OK, " confidence => ");
      Read_HID_Confidence (Source, Position, OK, Item.confidence);
      R.Expect (Source, Position, OK, " buttons => ");
      Read_HID_Buttons (Source, Position, OK, Item.buttons);
      R.Expect (Source, Position, OK, " contact_limit => ");
      Read_HID_Contact_Limit (Source, Position, OK, Item.contact_limit);
      R.Expect (Source, Position, OK, " binding_budget => ");
      Read_HID_Binding_Budget (Source, Position, OK, Item.binding_budget);
      R.Expect (Source, Position, OK, " timing => ");
      Read_HID_Timing (Source, Position, OK, Item.timing);
      R.Expect (Source, Position, OK, ")");
   end Read_HID_Parameters;

   procedure Decode (Source : String; Item : out HID_Parameters_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_HID_Parameters (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;

   function Value return HID_Parameters_Value
   with SPARK_Mode => Off
   is
      use type CuBit.Launch_Parameters.Outcome;
      Source : String (1 .. CuBit.Parameter_Sections.Maximum_Value_Bytes);
      Length : Natural;
      Found : CuBit.Launch_Parameters.Outcome;
      Item : HID_Parameters_Value;
      Decoded : Boolean;
   begin
      CuBit.Launch_Parameters.Get (Identity, Source, Length, Found);
      if Found /= CuBit.Launch_Parameters.Found then
         CuBit.Launch_Parameters.Refuse ("com.cubit.hid-i2c", Found);
      end if;
      Decode (Source (1 .. Length), Item, Decoded);
      if not Decoded then
         CuBit.Launch_Parameters.Refuse ("com.cubit.hid-i2c", CuBit.Launch_Parameters.Undecodable_Value);
      end if;
      return Item;
   end Value;

   procedure Require
   with SPARK_Mode => Off
   is
      Checked : constant HID_Parameters_Value := Value;
      pragma Unreferenced (Checked);
   begin
      null;
   end Require;
end Parameters;

