--  Hosted tests of the typed desktop.display.scale setting: the CCL schema
--  accepts Auto and Fixed scales within the display geometry's bounds and
--  nothing else, Encode writes exactly the checker's canonical form, and
--  Decode reads it back.
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with CCL.Interfaces.Desktop_Display;
with CCL.Typed_Settings;
with CCL.Setting_Kinds;
with CuBit.Display_Geometry;
with Desktop_Display_Scale_Settings;

procedure Settings_Tests is
   package TS renames CCL.Typed_Settings;
   package DD renames CCL.Interfaces.Desktop_Display;
   package S renames Desktop_Display_Scale_Settings;
   package G renames CuBit.Display_Geometry;
   use type TS.Setting_Kind, G.UI_Scale;
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
      TS.Check (CCL.Setting_Kinds.Display_Scale_Setting, Source, Result);
      return (if Result.Success then Result.Canonical (1 .. Result.Length) else "");
   end Canonical;
   function Accepted (Source : String) return Boolean is (Canonical (Source)'Length > 0);
   Item : S.Preference;
   OK : Boolean;
begin
   Check (TS.Kind_Of ("desktop.display.scale") = CCL.Setting_Kinds.Display_Scale_Setting, "the key is typed");
   Check (TS.Kind_Of ("desktop.display.other") = CCL.Setting_Kinds.Untyped, "only that key");
   --  Encode is the canonical spelling.
   Check (Canonical (S.Encode (S.Default)) = S.Encode (S.Default), "Auto is canonical: " & S.Encode (S.Default));
   for N in G.Scale_Component loop
      for D in G.Scale_Component loop
         declare
            Value : constant S.Preference := (Auto => False, Scale => (N, D));
         begin
            Check (Canonical (S.Encode (Value)) = S.Encode (Value), "canonical " & S.Encode (Value));
            S.Decode (S.Encode (Value), Item, OK);
            Check (OK and then not Item.Auto and then Item.Scale = (N, D), "round trip " & S.Encode (Value));
         end;
      end loop;
   end loop;
   S.Decode (S.Encode (S.Default), Item, OK);
   Check (OK and then Item.Auto, "Auto round trip");
   --  The schema's bounds are the geometry's; other shapes are refused.
   Check (not Accepted ("(Display_Scale.Fixed (UI_Scale numerator => 0 denominator => 1))"), "numerator 0");
   Check (not Accepted ("(Display_Scale.Fixed (UI_Scale numerator => 17 denominator => 1))"), "numerator 17");
   Check (not Accepted ("(Display_Scale.Fixed (UI_Scale numerator => 2))"), "missing denominator");
   Check (not Accepted ("Display_Scale.Large"), "no such member");
   Check (not Accepted ("""200%"""), "a string");
   --  A non-canonical but valid spelling is refused by Decode.
   S.Decode ("(Display_Scale.Fixed (UI_Scale denominator => 4 numerator => 5))", Item, OK);
   Check (not OK and then Item.Auto, "non-canonical spelling refused");
   S.Decode ("garbage", Item, OK);
   Check (not OK and then Item.Auto, "garbage refused, Auto kept");
   Put_Line ("display scale settings:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Settings_Tests;
