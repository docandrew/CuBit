with CCL.Objects.Views;

package body Calc_Settings is
   package Views renames CCL.Objects.Views;
   package TS renames CCL.Typed_Settings;

   type Snapshot_Access is access Views.Snapshot;
   Work : Snapshot_Access;

   procedure Decode (Source : String; Item : out CCL_Calc_Options.Choice; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := False;
      if Source'Length = 0 or else Source'Length > Maximum_Source then
         return;
      end if;
      if Work = null then
         Work := new Views.Snapshot;
      end if;
      TS.Read (TS.Calculator_Options_Setting, Source, Work.all, Success);
      if Success then
         Item := CCL_Calc_Options.Read (Work.all, Views.Root (Work.all));
      end if;
   end Decode;
end Calc_Settings;
