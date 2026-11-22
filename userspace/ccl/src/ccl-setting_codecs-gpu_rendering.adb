--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;

package body CCL.Setting_Codecs.GPU_Rendering with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_GPU_Rendering
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out GPU_Rendering_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(GPU_Rendering");
      R.Expect (Source, Position, OK, " enabled => ");
      R.Read_Boolean (Source, Position, OK, Item.enabled);
      R.Expect (Source, Position, OK, ")");
   end Read_GPU_Rendering;

   procedure Decode (Source : String; Item : out GPU_Rendering_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_GPU_Rendering (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.GPU_Rendering;

