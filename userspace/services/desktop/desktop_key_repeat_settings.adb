------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces;
with CCL.Objects;
with CCL.Objects.Views;

package body Desktop_Key_Repeat_Settings is
   use type Interfaces.Unsigned_64;
   package Views renames CCL.Objects.Views;
   package TS renames CCL.Typed_Settings;

   type Snapshot_Access is access Views.Snapshot;
   Work : Snapshot_Access;

   procedure Decode (Source : String; Item : out Key_Repeat.Settings; Success : out Boolean) is
      Read : Boolean;
   begin
      Item := Key_Repeat.DEFAULT_SETTINGS;
      Success := False;
      if Work = null then
         Work := new Views.Snapshot;
      end if;
      TS.Read (TS.Key_Repeat_Setting, Source, Work.all, Read);
      if not Read then
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Work.all);
         Enabled : constant Interfaces.Unsigned_64 :=
           Views.Scalar (Work.all, TS.Named (Work.all, Root, "enabled")).First;
         Initial_Delay : constant Interfaces.Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Work.all, TS.Named (Work.all, Root, "delay_ms")));
         Rate : constant Interfaces.Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Work.all, TS.Named (Work.all, Root, "rate_hz")));
      begin
         --  The schema's range types already bound both; a value outside
         --  them would mean the schema and Key_Repeat disagree.
         if Initial_Delay not in Interfaces.Integer_64 (Key_Repeat.Delay_Ms'First) ..
                                 Interfaces.Integer_64 (Key_Repeat.Delay_Ms'Last)
           or else Rate not in Interfaces.Integer_64 (Key_Repeat.Rate_Hz'First) ..
                               Interfaces.Integer_64 (Key_Repeat.Rate_Hz'Last)
         then
            return;
         end if;
         Item := (Enabled       => Enabled = 1,
                  Initial_Delay => Key_Repeat.Delay_Ms (Initial_Delay),
                  Rate          => Key_Repeat.Rate_Hz (Rate));
         Success := True;
      end;
   end Decode;
end Desktop_Key_Repeat_Settings;
