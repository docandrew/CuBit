------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Typed_Settings;
with CCL.Setting_Kinds;
with Interfaces;
with CCL.Objects.Views;

package body Desktop_GPU_Power_Settings is
   use type Interfaces.Unsigned_64;
   package Views renames CCL.Objects.Views;
   package TS renames CCL.Typed_Settings;

   type Snapshot_Access is access Views.Snapshot;
   Work : Snapshot_Access;

   procedure Decode (Source : String; Waitboost : out Boolean; Success : out Boolean) is
      Read : Boolean;
   begin
      Waitboost := Default_Waitboost;
      Success := False;
      if Work = null then
         Work := new Views.Snapshot;
      end if;
      TS.Read (CCL.Setting_Kinds.GPU_Waitboost_Setting, Source, Work.all, Read);
      if not Read then
         return;
      end if;
      Waitboost := Views.Scalar (Work.all, TS.Named (Work.all, Views.Root (Work.all), "enabled")).First = 1;
      Success := True;
   end Decode;
end Desktop_GPU_Power_Settings;
