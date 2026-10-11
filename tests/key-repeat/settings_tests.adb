--  Hosted tests of the typed `desktop.input.key-repeat` setting: the CCL
--  schema's bounds and defaults agree with Key_Repeat, values outside them
--  are refused when the configuration is compiled, and the desktop decodes
--  only the canonical form.
with Ada.Text_IO;
with Interfaces;
with CCL.Interfaces.Desktop_Input;
with CCL.Typed_Settings;
with Desktop_Key_Repeat_Settings;
with Key_Repeat;

procedure Settings_Tests is
   use type Interfaces.Unsigned_64;
   use type CCL.Typed_Settings.Setting_Kind;
   use type Key_Repeat.Settings;
   package TS renames CCL.Typed_Settings;
   package DI renames CCL.Interfaces.Desktop_Input;

   Failures : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   function Image (Value : Integer) return String is
      Text : constant String := Integer'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Accepts (Source : String) return Boolean is
      Result : TS.Check_Result;
   begin
      TS.Check (TS.Key_Repeat_Setting, Source, Result);
      return Result.Success;
   end Accepts;

   function Canonical (Source : String) return String is
      Result : TS.Check_Result;
   begin
      TS.Check (TS.Key_Repeat_Setting, Source, Result);
      return (if Result.Success then Result.Canonical (1 .. Result.Length) else "");
   end Canonical;

   function Value (Enabled : String; Initial_Delay, Rate : Integer) return String is
     ("(Key_Repeat enabled => " & Enabled & " delay_ms => " & Image (Initial_Delay) &
      " rate_hz => " & Image (Rate) & ")");

   Item : Key_Repeat.Settings;
   OK : Boolean;
begin
   Check (TS.Kind_Of (DI.KEY_REPEAT_KEY) = TS.Key_Repeat_Setting, "the key is typed");
   Check (TS.Kind_Of ("desktop.input.other") = TS.Untyped, "only that key");

   --  Defaults: the schema's equal Key_Repeat's.
   Check (Canonical ("(Key_Repeat)") =
            Value ("true", DI.DEFAULT_DELAY_MS, DI.DEFAULT_RATE_HZ),
          "schema defaults are the named defaults: " & Canonical ("(Key_Repeat)"));
   Desktop_Key_Repeat_Settings.Decode (Canonical ("(Key_Repeat)"), Item, OK);
   Check (OK and then Item = Key_Repeat.DEFAULT_SETTINGS, "defaults decode to DEFAULT_SETTINGS");

   --  Bounds: the schema's equal Key_Repeat's.
   Check (Accepts (Value ("true", DI.MINIMUM_DELAY_MS, DI.MINIMUM_RATE_HZ)), "minimum bounds accepted");
   Check (Accepts (Value ("true", DI.MAXIMUM_DELAY_MS, DI.MAXIMUM_RATE_HZ)), "maximum bounds accepted");
   Check (not Accepts (Value ("true", DI.MINIMUM_DELAY_MS - 1, DI.DEFAULT_RATE_HZ)), "delay below range refused");
   Check (not Accepts (Value ("true", DI.MAXIMUM_DELAY_MS + 1, DI.DEFAULT_RATE_HZ)), "delay above range refused");
   Check (not Accepts (Value ("true", DI.DEFAULT_DELAY_MS, DI.MINIMUM_RATE_HZ - 1)), "rate below range refused");
   Check (not Accepts (Value ("true", DI.DEFAULT_DELAY_MS, DI.MAXIMUM_RATE_HZ + 1)), "rate above range refused");
   Check (Key_Repeat.Delay_Ms'First = DI.MINIMUM_DELAY_MS and Key_Repeat.Delay_Ms'Last = DI.MAXIMUM_DELAY_MS and
          Key_Repeat.Rate_Hz'First = DI.MINIMUM_RATE_HZ and Key_Repeat.Rate_Hz'Last = DI.MAXIMUM_RATE_HZ,
          "Key_Repeat uses the schema's bounds");
   Check (not Accepts ("(Key_Repeat delay_ms => ""fast"")"), "wrong field type refused");
   Check (not Accepts ("(Key_Repeat speed => 3)"), "unknown field refused");
   Check (not Accepts ("500"), "not a Key_Repeat refused");

   --  A configured value round-trips; partial values take field defaults.
   Desktop_Key_Repeat_Settings.Decode (Canonical (Value ("false", 250, 50)), Item, OK);
   Check (OK and then Item = (False, 250, 50), "configured value decodes");
   Check (Canonical ("(Key_Repeat rate_hz => 40)") = Value ("true", DI.DEFAULT_DELAY_MS, 40),
          "missing fields take defaults");

   --  The desktop reads only the canonical spelling.
   Desktop_Key_Repeat_Settings.Decode ("(Key_Repeat rate_hz => 40)", Item, OK);
   Check (not OK and then Item = Key_Repeat.DEFAULT_SETTINGS, "non-canonical stored value refused");
   Desktop_Key_Repeat_Settings.Decode (Value ("true", 50, 30), Item, OK);
   Check (not OK, "out-of-range stored value refused");
   Desktop_Key_Repeat_Settings.Decode ("", Item, OK);
   Check (not OK, "empty refused");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("key-repeat settings: PASS" & Checks'Image & " checks");
   else
      Ada.Text_IO.Put_Line ("key-repeat settings: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Settings_Tests;
