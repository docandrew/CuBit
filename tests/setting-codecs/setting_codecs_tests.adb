--  Hosted tests of the generated typed-setting decoders
--  (CCL.Setting_Codecs.*, ccl-manifest --setting-codecs) against the CCL
--  path they replace (CCL.Typed_Settings.Check: analyse, evaluate, capture,
--  render; a value is read only if it is its own rendering).
--
--  For a corpus of values of every catalog type, written as authors write
--  them, CCL.Typed_Settings.Check gives the canonical source. Then:
--  - the generated decoder accepts the canonical source, with the fields
--    the test expects;
--  - every truncation (each proper prefix) is refused by both;
--  - every single-character deletion, replacement (by each of a set of
--    characters that matter to the grammar) and insertion is accepted by
--    the decoder exactly when the CCL path does.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with CuBit.Canonical_Readers;
with CCL.Setting_Kinds;
with CCL.Typed_Settings;
with CCL.Setting_Codecs.Calc_Options;
with CCL.Setting_Codecs.Display_Mode;
with CCL.Setting_Codecs.Display_Modeset;
with CCL.Setting_Codecs.Display_Scale;
with CCL.Setting_Codecs.GPU_Rendering;
with CCL.Setting_Codecs.GPU_Waitboost;
with CCL.Setting_Codecs.Key_Repeat;
with CCL.Setting_Codecs.Launch_Entry;
with CCL.Setting_Codecs.Net_Interface;
with CCL.Interfaces.Desktop_Launch.Icons;

procedure Setting_Codecs_Tests is
   package TS renames CCL.Typed_Settings;
   package SK renames CCL.Setting_Kinds;
   package CR renames CuBit.Canonical_Readers;
   use all type SK.Setting_Kind;

   Failures : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   --  The decoder generated for Kind: whether it accepts Source.
   function Decoded (Kind : SK.Setting_Kind; Source : String) return Boolean is
      OK : Boolean := False;
   begin
      case Kind is
         when Calc_Options_Setting =>
            declare
               Item : CCL.Setting_Codecs.Calc_Options.Calc_Options_Value;
            begin
               CCL.Setting_Codecs.Calc_Options.Decode (Source, Item, OK);
            end;
         when Launch_Entry_Setting =>
            declare
               Item : CCL.Setting_Codecs.Launch_Entry.Launch_Entry_Value;
            begin
               CCL.Setting_Codecs.Launch_Entry.Decode (Source, Item, OK);
               --  What the type cannot say, which CCL.Typed_Settings and
               --  the desktop both check: the icon is in the atlas.
               OK := OK and then CCL.Interfaces.Desktop_Launch.Icons.Known (CR.Image (Item.icon.name));
            end;
         when Key_Repeat_Setting =>
            declare
               Item : CCL.Setting_Codecs.Key_Repeat.Key_Repeat_Value;
            begin
               CCL.Setting_Codecs.Key_Repeat.Decode (Source, Item, OK);
            end;
         when Display_Scale_Setting =>
            declare
               Item : CCL.Setting_Codecs.Display_Scale.Display_Scale_Value;
            begin
               CCL.Setting_Codecs.Display_Scale.Decode (Source, Item, OK);
            end;
         when Display_Mode_Setting =>
            declare
               Item : CCL.Setting_Codecs.Display_Mode.Display_Mode_Value;
            begin
               CCL.Setting_Codecs.Display_Mode.Decode (Source, Item, OK);
            end;
         when Display_Modeset_Setting =>
            declare
               Item : CCL.Setting_Codecs.Display_Modeset.Display_Modeset_Value;
            begin
               CCL.Setting_Codecs.Display_Modeset.Decode (Source, Item, OK);
            end;
         when GPU_Waitboost_Setting =>
            declare
               Item : CCL.Setting_Codecs.GPU_Waitboost.GPU_Waitboost_Value;
            begin
               CCL.Setting_Codecs.GPU_Waitboost.Decode (Source, Item, OK);
            end;
         when GPU_Rendering_Setting =>
            declare
               Item : CCL.Setting_Codecs.GPU_Rendering.GPU_Rendering_Value;
            begin
               CCL.Setting_Codecs.GPU_Rendering.Decode (Source, Item, OK);
            end;
         when Net_Interface_Setting =>
            declare
               Item : CCL.Setting_Codecs.Net_Interface.Net_Interface_Value;
            begin
               CCL.Setting_Codecs.Net_Interface.Decode (Source, Item, OK);
            end;
         when Untyped =>
            null;
      end case;
      return OK;
   end Decoded;

   --  The CCL path's verdict (the reference): a stored value is read back
   --  only if checking it gives that same value, its own canonical source.
   function Referenced (Kind : SK.Setting_Kind; Source : String) return Boolean is
      Result : TS.Check_Result;
   begin
      if Source'Length = 0 then
         return False;
      end if;
      TS.Check (Kind, Source, Result);
      return Result.Success and then Result.Canonical (1 .. Result.Length) = Source;
   end Referenced;

   function Canonical (Kind : SK.Setting_Kind; Source : String) return String is
      Result : TS.Check_Result;
   begin
      TS.Check (Kind, Source, Result);
      return (if Result.Success then Result.Canonical (1 .. Result.Length) else "");
   end Canonical;

   --  Characters that matter to the canonical grammar, and a few that do not.
   Probes : constant String := "\"" ()-09azAZ._=>" & ASCII.LF & Character'Val (16#E9#);

   --  Control and non-ASCII characters as [code], for the log.
   function Visible (Text : String) return String is
     (if Text'Length = 0 then ""
      elsif Text (Text'First) in ' ' .. '~' then Text (Text'First) & Visible (Text (Text'First + 1 .. Text'Last))
      else "[" & Natural'Image (Character'Pos (Text (Text'First))) & "]"
           & Visible (Text (Text'First + 1 .. Text'Last)));

   procedure Differential (Kind : SK.Setting_Kind; Author : String) is
      Text : constant String := Canonical (Kind, Author);
      Mismatches : Natural := 0;
      procedure Agree (Variant : String; What : String) is
         Mine : constant Boolean := Decoded (Kind, Variant);
         Theirs : constant Boolean := Referenced (Kind, Variant);
      begin
         Checks := Checks + 1;
         if Mine /= Theirs then
            Mismatches := Mismatches + 1;
            if Mismatches <= 5 then
               Put_Line ("FAIL: " & SK.Setting_Kind'Image (Kind) & " " & What & ": decoder " &
                         (if Mine then "accepts" else "refuses") & " but CCL " &
                         (if Theirs then "accepts" else "refuses") & ": " & Visible (Variant));
            end if;
         end if;
      end Agree;
   begin
      Check (Text'Length > 0, SK.Setting_Kind'Image (Kind) & " corpus value is valid: " & Author);
      if Text'Length = 0 then
         return;
      end if;
      Check (Decoded (Kind, Text), SK.Setting_Kind'Image (Kind) & " decodes its canonical source: " & Text);
      for Last in Text'First - 1 .. Text'Last - 1 loop
         Check (not Decoded (Kind, Text (Text'First .. Last)),
                SK.Setting_Kind'Image (Kind) & " refuses truncation to" & Natural'Image (Last - Text'First + 1));
         Check (not Referenced (Kind, Text (Text'First .. Last)),
                SK.Setting_Kind'Image (Kind) & " CCL refuses truncation to" & Natural'Image (Last - Text'First + 1));
      end loop;
      for I in Text'Range loop
         Agree (Text (Text'First .. I - 1) & Text (I + 1 .. Text'Last), "deleting" & Natural'Image (I));
         for P of Probes loop
            if P /= Text (I) then
               Agree (Text (Text'First .. I - 1) & P & Text (I + 1 .. Text'Last), "replacing" & Natural'Image (I));
            end if;
            Agree (Text (Text'First .. I - 1) & P & Text (I .. Text'Last), "inserting at" & Natural'Image (I));
         end loop;
      end loop;
      Agree (Text & " ", "trailing space");
      Agree (" " & Text, "leading space");
      Failures := Failures + Mismatches;
   end Differential;

   procedure Field_Checks is
      package NI renames CCL.Setting_Codecs.Net_Interface;
      package KR renames CCL.Setting_Codecs.Key_Repeat;
      package LE renames CCL.Setting_Codecs.Launch_Entry;
      package DM renames CCL.Setting_Codecs.Display_Mode;
      package CO renames CCL.Setting_Codecs.Calc_Options;
      Net : NI.Net_Interface_Value;
      Repeat : KR.Key_Repeat_Value;
      Launch : LE.Launch_Entry_Value;
      Mode : DM.Display_Mode_Value;
      Calc : CO.Calc_Options_Value;
      OK : Boolean;
      use type NI.Net_Interface_Choice, NI.Gateway_Choice, NI.Prefix_Length_Value, NI.Route_Metric_Value;
      use type LE.Launch_Action_Choice, LE.App_Category_Value, LE.App_Color_Choice, LE.Color_Channel_Value;
      use type DM.Display_Mode_Choice, DM.Refresh_Millihertz_Value, DM.Mode_Extent_Value;
      use type KR.Repeat_Delay_Value, KR.Repeat_Rate_Value;
      use type CO.Calc_Mode_Value, CO.Calc_Word_Value, CO.Calc_Base_Value, CO.Calc_Angle_Value;
   begin
      NI.Decode (Canonical (Net_Interface_Setting,
                 "(Net_Interface.Static (Static_Interface address => ""::ffff:10.0.2.15"" prefix => 120 " &
                 "gateway => (Gateway.Via ""::ffff:10.0.2.2"")))"), Net, OK);
      Check (OK and then Net.Choice = NI.Static
             and then CR.Image (Net.Static_Payload.address) = "::ffff:10.0.2.15"
             and then Net.Static_Payload.prefix = 120
             and then Net.Static_Payload.gateway.Choice = NI.Via
             and then CR.Image (Net.Static_Payload.gateway.Via_Payload) = "::ffff:10.0.2.2"
             and then Net.Static_Payload.metric = 100,
             "a static interface's fields, its default metric included");
      NI.Decode ("Net_Interface.Disabled", Net, OK);
      Check (OK and then Net.Choice = NI.Disabled, "Disabled");
      NI.Decode ("Net_Interface.Disable", Net, OK);
      Check (not OK and then Net.Choice = NI.Automatic, "a refused value leaves the default");

      KR.Decode (Canonical (Key_Repeat_Setting, "(Key_Repeat delay_ms => 2000 rate_hz => 1)"), Repeat, OK);
      Check (OK and then Repeat.enabled and then Repeat.delay_ms = 2000 and then Repeat.rate_hz = 1,
             "key repeat at its bounds");
      KR.Decode ("(Key_Repeat enabled => true delay_ms => 99 rate_hz => 1)", Repeat, OK);
      Check (not OK, "key repeat delay below its range");
      KR.Decode ("(Key_Repeat enabled => true delay_ms => 0500 rate_hz => 1)", Repeat, OK);
      Check (not OK, "a leading zero is not canonical");

      LE.Decode (Canonical (Launch_Entry_Setting,
                 "(Launch_Entry label => ""Say \""hi\"" \\ bye"" action => (Launch_Action.Program ""a.app"") " &
                 "icon => (Launch_Icon ""Doom"") category => App_Category.Games single_instance => true " &
                 "color => (App_Color.RGB (RGB_Color red => 255 green => 0 blue => 7)))"), Launch, OK);
      Check (OK and then CR.Image (Launch.label) = "Say ""hi"" \ bye"
             and then Launch.action.Choice = LE.Program and then CR.Image (Launch.action.Program_Payload) = "a.app"
             and then CR.Image (Launch.icon.name) = "Doom" and then Launch.category = LE.Games
             and then Launch.single_instance and then Launch.color.Choice = LE.RGB
             and then Launch.color.RGB_Payload.red = 255 and then Launch.color.RGB_Payload.blue = 7,
             "a launch entry with escaped text");
      LE.Decode (Canonical (Launch_Entry_Setting,
                 "(Launch_Entry label => ""1\n2\r"" action => Launch_Action.Settings icon => (Launch_Icon ""Logs""))"),
                 Launch, OK);
      Check (OK and then CR.Image (Launch.label) = "1" & ASCII.LF & "2" & ASCII.CR, "line breaks as escapes");

      DM.Decode (Canonical (Display_Mode_Setting,
                 "(Display_Mode.Fixed (Display_Mode_Choice width => 65535 height => 1 " &
                 "refresh_millihertz => 4294967295))"), Mode, OK);
      Check (OK and then Mode.Choice = DM.Fixed and then Mode.Fixed_Payload.width = 65535
             and then Mode.Fixed_Payload.refresh_millihertz = 4_294_967_295,
             "a display mode at its bounds");

      CO.Decode (Canonical (Calc_Options_Setting, "(Calc_Options mode => Calc_Mode.Programmer word => Calc_Word.Bits_8)"),
                 Calc, OK);
      Check (OK and then Calc.mode = CO.Programmer and then Calc.word = CO.Bits_8 and then Calc.signed
             and then Calc.base = CO.Decimal and then Calc.angle = CO.Radians,
             "calculator options with defaults");

      Check (not Decoded (Net_Interface_Setting, [1 .. CR.Maximum_Canonical + 1 => 'x']), "an over-long source");
      Check (not Decoded (Net_Interface_Setting, ""), "an empty source");
      declare
         Offset : constant String (1000 .. 1021) := "Net_Interface.Disabled";
      begin
         Check (Decoded (Net_Interface_Setting, Offset), "a source that does not start at 1");
      end;
   end Field_Checks;
begin
   Field_Checks;
   Differential (Calc_Options_Setting, "(Calc_Options)");
   Differential (Calc_Options_Setting,
                 "(Calc_Options mode => Calc_Mode.Scientific angle => Calc_Angle.Gradians word => Calc_Word.Unbounded " &
                 "signed => false base => Calc_Base.Binary)");
   Differential (Launch_Entry_Setting,
                 "(Launch_Entry label => ""DOOM"" action => (Launch_Action.Program ""doom.elf"") " &
                 "icon => (Launch_Icon ""Doom"") category => App_Category.Games single_instance => true " &
                 "color => (App_Color.RGB (RGB_Color red => 179 green => 38 blue => 26)))");
   Differential (Launch_Entry_Setting,
                 "(Launch_Entry label => ""a\""b\\"" action => Launch_Action.Settings icon => (Launch_Icon ""Logs""))");
   --  \n and \r are canonical escapes; a tab stays raw.
   Differential (Launch_Entry_Setting,
                 "(Launch_Entry label => ""1\n2\r3\t4"" action => Launch_Action.Settings icon => (Launch_Icon ""Logs""))");
   Differential (Key_Repeat_Setting, "(Key_Repeat)");
   Differential (Key_Repeat_Setting, "(Key_Repeat enabled => false delay_ms => 2000 rate_hz => 100)");
   Differential (Display_Scale_Setting, "Display_Scale.Auto");
   Differential (Display_Scale_Setting, "(Display_Scale.Fixed (UI_Scale numerator => 16 denominator => 1))");
   Differential (Display_Mode_Setting, "Display_Mode.Auto");
   Differential (Display_Mode_Setting,
                 "(Display_Mode.Fixed (Display_Mode_Choice width => 3840 height => 2160 refresh_millihertz => 60000))");
   Differential (Display_Modeset_Setting, "(Display_Modeset enabled => true)");
   Differential (GPU_Waitboost_Setting, "(GPU_Waitboost)");
   Differential (GPU_Rendering_Setting, "(GPU_Rendering enabled => false)");
   Differential (Net_Interface_Setting, "Net_Interface.Automatic");
   Differential (Net_Interface_Setting, "Net_Interface.Disabled");
   Differential (Net_Interface_Setting,
                 "(Net_Interface.Static (Static_Interface address => ""::ffff:10.0.2.15"" prefix => 120 " &
                 "gateway => (Gateway.Via ""::ffff:10.0.2.2"") metric => 65535))");
   Differential (Net_Interface_Setting,
                 "(Net_Interface.Static (Static_Interface address => ""fe80::1"" prefix => 0))");
   if Failures = 0 then
      Put_Line ("setting-codecs: PASS" & Natural'Image (Checks) & " checks");
   else
      Put_Line ("setting-codecs: FAIL" & Natural'Image (Failures) & " of" & Natural'Image (Checks));
      raise Program_Error;
   end if;
end Setting_Codecs_Tests;
