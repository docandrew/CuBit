------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces;
with CCL.Objects.Views;

package body Desktop_Display_Modeset_Settings is
   use type Interfaces.Unsigned_64;
   package Views renames CCL.Objects.Views;
   package TS renames CCL.Typed_Settings;

   type Snapshot_Access is access Views.Snapshot;
   Work : Snapshot_Access;

   procedure Decode (Source : String; Enabled : out Boolean; Success : out Boolean) is
      Read : Boolean;
   begin
      Enabled := Default_Enabled;
      Success := False;
      if Work = null then
         Work := new Views.Snapshot;
      end if;
      TS.Read (TS.Display_Modeset_Setting, Source, Work.all, Read);
      if not Read then
         return;
      end if;
      Enabled := Views.Scalar (Work.all, TS.Named (Work.all, Views.Root (Work.all), "enabled")).First = 1;
      Success := True;
   end Decode;
end Desktop_Display_Modeset_Settings;
