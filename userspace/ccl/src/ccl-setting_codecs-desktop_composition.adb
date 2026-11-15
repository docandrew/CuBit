--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;

package body CCL.Setting_Codecs.Desktop_Composition with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Composition_Path
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Composition_Path_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Composition_Path_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Composition_Path.CPU") then
         R.Expect_Word (Source, Position, OK, "Composition_Path.CPU");
         Item := CPU;
      elsif R.Next_Is_Word (Source, Position, "Composition_Path.GPU") then
         R.Expect_Word (Source, Position, OK, "Composition_Path.GPU");
         Item := GPU;
      else
         OK := False;
      end if;
   end Read_Composition_Path;

   procedure Read_Desktop_Composition
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Desktop_Composition_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Desktop_Composition");
      R.Expect (Source, Position, OK, " path => ");
      Read_Composition_Path (Source, Position, OK, Item.path);
      R.Expect (Source, Position, OK, ")");
   end Read_Desktop_Composition;

   procedure Decode (Source : String; Item : out Desktop_Composition_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Desktop_Composition (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Desktop_Composition;

