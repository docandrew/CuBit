--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with Interfaces;

package body CCL.Setting_Codecs.Display_Mode with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Mode_Extent
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Mode_Extent_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Mode_Extent_Value'First), Interfaces.Integer_64 (Mode_Extent_Value'Last), Value);
      Item := Mode_Extent_Value (Value);
   end Read_Mode_Extent;

   procedure Read_Refresh_Millihertz
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Refresh_Millihertz_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Refresh_Millihertz_Value'First), Interfaces.Integer_64 (Refresh_Millihertz_Value'Last), Value);
      Item := Refresh_Millihertz_Value (Value);
   end Read_Refresh_Millihertz;

   procedure Read_Display_Mode_Choice
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Display_Mode_Choice_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Display_Mode_Choice");
      R.Expect (Source, Position, OK, " width => ");
      Read_Mode_Extent (Source, Position, OK, Item.width);
      R.Expect (Source, Position, OK, " height => ");
      Read_Mode_Extent (Source, Position, OK, Item.height);
      R.Expect (Source, Position, OK, " refresh_millihertz => ");
      Read_Refresh_Millihertz (Source, Position, OK, Item.refresh_millihertz);
      R.Expect (Source, Position, OK, ")");
   end Read_Display_Mode_Choice;

   procedure Read_Display_Mode
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Display_Mode_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := (others => <>);
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Display_Mode.Auto") then
         R.Expect_Word (Source, Position, OK, "Display_Mode.Auto");
         Item.Choice := Auto;
      elsif R.Next_Is (Source, Position, "(Display_Mode.Fixed ") then
         R.Expect (Source, Position, OK, "(Display_Mode.Fixed ");
         Read_Display_Mode_Choice (Source, Position, OK, Item.Fixed_Payload);
         R.Expect (Source, Position, OK, ")");
         Item.Choice := Fixed;
      else
         OK := False;
      end if;
   end Read_Display_Mode;

   procedure Decode (Source : String; Item : out Display_Mode_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Display_Mode (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Display_Mode;

