--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with Interfaces;

package body CCL.Setting_Codecs.Display_Scale with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Scale_Part
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Scale_Part_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Scale_Part_Value'First), Interfaces.Integer_64 (Scale_Part_Value'Last), Value);
      Item := Scale_Part_Value (Value);
   end Read_Scale_Part;

   procedure Read_UI_Scale
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out UI_Scale_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(UI_Scale");
      R.Expect (Source, Position, OK, " numerator => ");
      Read_Scale_Part (Source, Position, OK, Item.numerator);
      R.Expect (Source, Position, OK, " denominator => ");
      Read_Scale_Part (Source, Position, OK, Item.denominator);
      R.Expect (Source, Position, OK, ")");
   end Read_UI_Scale;

   procedure Read_Display_Scale
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Display_Scale_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := (others => <>);
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Display_Scale.Auto") then
         R.Expect_Word (Source, Position, OK, "Display_Scale.Auto");
         Item.Choice := Auto;
      elsif R.Next_Is (Source, Position, "(Display_Scale.Fixed ") then
         R.Expect (Source, Position, OK, "(Display_Scale.Fixed ");
         Read_UI_Scale (Source, Position, OK, Item.Fixed_Payload);
         R.Expect (Source, Position, OK, ")");
         Item.Choice := Fixed;
      else
         OK := False;
      end if;
   end Read_Display_Scale;

   procedure Decode (Source : String; Item : out Display_Scale_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Display_Scale (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Display_Scale;

