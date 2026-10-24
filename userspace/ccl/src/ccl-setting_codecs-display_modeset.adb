--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;

package body CCL.Setting_Codecs.Display_Modeset with SPARK_Mode is
   package R renames CCL.Canonical_Readers;

   procedure Read_Display_Modeset
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Display_Modeset_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Display_Modeset");
      R.Expect (Source, Position, OK, " enabled => ");
      R.Read_Boolean (Source, Position, OK, Item.enabled);
      R.Expect (Source, Position, OK, ")");
   end Read_Display_Modeset;

   procedure Decode (Source : String; Item : out Display_Modeset_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Display_Modeset (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Display_Modeset;

