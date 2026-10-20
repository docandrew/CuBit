--  Hosted tests of the typed desktop.display.mode.<monitor> setting
--  (DISP-016): the CCL schema accepts Auto and Fixed modes within the
--  output protocol's bounds and nothing else, Encode writes exactly the
--  checker's canonical form, Decode reads it back, and each monitor's EDID
--  identity gives its own key.
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with CCL.Interfaces.Desktop_Display;
with CCL.Typed_Settings;
with CuBit.Output_Modes;
with Interfaces;
with Desktop_Display_Mode_Settings;

procedure Mode_Settings_Tests is
   package TS renames CCL.Typed_Settings;
   package DD renames CCL.Interfaces.Desktop_Display;
   package S renames Desktop_Display_Mode_Settings;
   package OM renames CuBit.Output_Modes;
   use type TS.Setting_Kind, OM.Mode, OM.Mode_Request, Interfaces.Unsigned_32;
   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;
   function Canonical (Source : String) return String is
      Result : TS.Check_Result;
   begin
      TS.Check (TS.Display_Mode_Setting, Source, Result);
      return (if Result.Success then Result.Canonical (1 .. Result.Length) else "");
   end Canonical;
   function Accepted (Source : String) return Boolean is (Canonical (Source)'Length > 0);
   LG : constant OM.Monitor_Identity :=
     (Vendor => ['G', 'S', 'M'], Product => 16#5B9A#, Serial => 16#0001_E0E7#, Name_Last => 0,
      Name => [others => ' ']);
   Item : S.Preference;
   OK : Boolean;
   Sizes : constant array (1 .. 4) of OM.Mode := [(1, 1), (720, 576), (3840, 2160), (65_535, 65_535)];
   Rates : constant array (1 .. 4) of OM.Millihertz := [1, 59_940, 60_000, OM.Millihertz'Last];
begin
   Check (S.Key_For (LG) = "desktop.display.mode.GSM-5B9A-0001E0E7", "key " & S.Key_For (LG));
   Check (S.Key_For ((LG with delta Serial => 0, Vendor => ['?', 'A', 'Z'])) =
          "desktop.display.mode.?AZ-5B9A-00000000", "key of a bad vendor letter, serial zero");
   Check (S.Key_For (LG) /= S.Key_For ((LG with delta Serial => 1)), "serials give their own keys");
   Check (TS.Kind_Of (S.Key_For (LG)) = TS.Display_Mode_Setting, "the monitor key is typed");
   Check (TS.Kind_Of (DD.MODESET_KEY) = TS.Display_Modeset_Setting, "desktop.display.modeset stays its own");
   Check (TS.Kind_Of (DD.MODE_PREFIX) = TS.Untyped, "the bare prefix names no monitor");
   Check (Canonical (S.Encode (S.Default)) = S.Encode (S.Default), "Auto is canonical: " & S.Encode (S.Default));
   S.Decode (S.Encode (S.Default), Item, OK);
   Check (OK and then Item.Auto, "Auto round trip");
   for Size of Sizes loop
      for Rate of Rates loop
         declare
            Value : constant S.Preference := (Auto => False, Size => Size, Refresh => Rate);
         begin
            Check (Canonical (S.Encode (Value)) = S.Encode (Value), "canonical " & S.Encode (Value));
            S.Decode (S.Encode (Value), Item, OK);
            Check (OK and then not Item.Auto and then Item.Size = Size and then Item.Refresh = Rate,
                   "round trip " & S.Encode (Value));
            Check (S.Request_Of (Value) = (OM.Exact_Rate, Size, Rate), "request of " & S.Encode (Value));
         end;
      end loop;
   end loop;
   --  The schema's bounds are the protocol's; other shapes are refused.
   Check (not Accepted ("(Display_Mode.Fixed (Display_Mode_Choice width => 0 height => 1080 " &
                        "refresh_millihertz => 60000))"), "width 0");
   Check (not Accepted ("(Display_Mode.Fixed (Display_Mode_Choice width => 65536 height => 1080 " &
                        "refresh_millihertz => 60000))"), "width past 16 bits");
   Check (not Accepted ("(Display_Mode.Fixed (Display_Mode_Choice width => 1920 height => 1080 " &
                        "refresh_millihertz => 0))"), "refresh 0");
   Check (not Accepted ("(Display_Mode.Fixed (Display_Mode_Choice width => 1920 height => 1080 " &
                        "refresh_millihertz => 4294967296))"), "refresh past 32 bits");
   Check (not Accepted ("(Display_Mode.Fixed (Display_Mode_Choice width => 1920 height => 1080))"),
          "missing refresh");
   Check (not Accepted ("Display_Mode.Native"), "no such member");
   --  A non-canonical but valid spelling is refused by Decode.
   S.Decode ("(Display_Mode.Fixed (Display_Mode_Choice height => 1080 width => 1920 " &
             "refresh_millihertz => 60000))", Item, OK);
   Check (not OK and then Item.Auto, "non-canonical spelling refused");
   S.Decode ("garbage", Item, OK);
   Check (not OK and then Item.Auto, "garbage refused, Auto kept");
   Put_Line ("display mode settings:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Mode_Settings_Tests;
