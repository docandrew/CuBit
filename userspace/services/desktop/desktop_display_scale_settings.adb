------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Interfaces.Desktop_Display;
with CCL.Typed_Settings;
with CCL.Setting_Kinds;
with Interfaces;
with CCL.Objects;
with CCL.Objects.Views;

package body Desktop_Display_Scale_Settings is
   package Views renames CCL.Objects.Views;
   package TS renames CCL.Typed_Settings;
   package DD renames CCL.Interfaces.Desktop_Display;
   package G renames CuBit.Display_Geometry;

   --  The schema's member and field names (DD.TYPE_SOURCE).
   Auto_Member : constant String := "Auto";
   Fixed_Member : constant String := "Fixed";
   Numerator_Field : constant String := "numerator";
   Denominator_Field : constant String := "denominator";

   pragma Compile_Time_Error
     (DD.MINIMUM_SCALE_PART /= Natural (G.Scale_Component'First) or else
      DD.MAXIMUM_SCALE_PART /= Natural (G.Scale_Component'Last),
      "the CCL scale part bounds must be the display geometry's");

   type Snapshot_Access is access Views.Snapshot;
   Work : Snapshot_Access;

   procedure Decode (Source : String; Item : out Preference; Success : out Boolean) is
      Read : Boolean;
   begin
      Item := Default;
      Success := False;
      if Work = null then
         Work := new Views.Snapshot;
      end if;
      TS.Read (CCL.Setting_Kinds.Display_Scale_Setting, Source, Work.all, Read);
      if not Read then
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Work.all);
         Member : constant String := TS.Alternative_Name (Work.all, Root);
      begin
         if Member = Auto_Member then
            Success := True;
         elsif Member = Fixed_Member then
            declare
               Scale : constant Views.Cursor := Views.Payload (Work.all, Root);
               N : constant Interfaces.Integer_64 :=
                 CCL.Objects.Integer_Of (Views.Scalar (Work.all, TS.Named (Work.all, Scale, Numerator_Field)));
               D : constant Interfaces.Integer_64 :=
                 CCL.Objects.Integer_Of (Views.Scalar (Work.all, TS.Named (Work.all, Scale, Denominator_Field)));
               subtype Part is Interfaces.Integer_64 range
                 Interfaces.Integer_64 (G.Scale_Component'First) .. Interfaces.Integer_64 (G.Scale_Component'Last);
            begin
               --  The schema's range type already bounds both.
               if N in Part and then D in Part then
                  Item := (Auto => False, Scale => (G.Scale_Component (N), G.Scale_Component (D)));
                  Success := True;
               end if;
            end;
         end if;
      end;
   end Decode;

   function Image (Value : G.Scale_Component) return String is
      Text : constant String := G.Scale_Component'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Encode (Item : Preference) return String is
     (if Item.Auto then DD.ROOT_TYPE_NAME & "." & Auto_Member
      else "(" & DD.ROOT_TYPE_NAME & "." & Fixed_Member & " (UI_Scale " &
           Numerator_Field & " => " & Image (Item.Scale.Numerator) & " " &
           Denominator_Field & " => " & Image (Item.Scale.Denominator) & "))");
end Desktop_Display_Scale_Settings;
