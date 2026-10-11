--  The desktop's keyboard settings as CCL types: `desktop.input.key-repeat`
--  is one Key_Repeat, checked against these declarations when the
--  configuration is compiled (CCL.Typed_Settings) and read back the same way
--  by the desktop (Desktop_Key_Repeat_Settings). The bounds are the desktop's
--  typematic limits (Key_Repeat): the type system refuses a value outside
--  them, so the desktop never clamps or guesses.
package CCL.Interfaces.Desktop_Input with SPARK_Mode, Pure is
   --  Delay before the first repeat, in milliseconds.
   MINIMUM_DELAY_MS : constant := 100;
   MAXIMUM_DELAY_MS : constant := 2_000;
   DEFAULT_DELAY_MS : constant := 500;
   --  Repeats per second once repeating.
   MINIMUM_RATE_HZ : constant := 1;
   MAXIMUM_RATE_HZ : constant := 100;
   DEFAULT_RATE_HZ : constant := 30;

   --  The same bounds and defaults, spelled for the CCL analyser
   --  (tests/key-repeat checks the two agree).
   TYPE_SOURCE : constant String :=
     "(type Repeat_Delay (range 100 2000)) " &
     "(type Repeat_Rate (range 1 100)) " &
     "(type Key_Repeat (record (enabled Boolean true) " &
     "(delay_ms Repeat_Delay 500) (rate_hz Repeat_Rate 30)))";
   ROOT_TYPE_NAME : constant String := "Key_Repeat";
   KEY_REPEAT_KEY : constant String := "desktop.input.key-repeat";
end CCL.Interfaces.Desktop_Input;
