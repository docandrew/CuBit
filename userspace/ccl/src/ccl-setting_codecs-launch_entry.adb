--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with Interfaces;

package body CCL.Setting_Codecs.Launch_Entry with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Launch_Action
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Launch_Action_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := (others => <>);
      if not OK then
         return;
      elsif R.Next_Is (Source, Position, "(Launch_Action.Program ") then
         R.Expect (Source, Position, OK, "(Launch_Action.Program ");
         R.Read_Text (Source, Position, OK, Item.Program_Payload);
         R.Expect (Source, Position, OK, ")");
         Item.Choice := Program;
      elsif R.Next_Is_Word (Source, Position, "Launch_Action.Settings") then
         R.Expect_Word (Source, Position, OK, "Launch_Action.Settings");
         Item.Choice := Settings;
      else
         OK := False;
      end if;
   end Read_Launch_Action;

   procedure Read_Launch_Icon
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Launch_Icon_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Launch_Icon");
      R.Expect (Source, Position, OK, " name => ");
      R.Read_Text (Source, Position, OK, Item.name);
      R.Expect (Source, Position, OK, ")");
   end Read_Launch_Icon;

   procedure Read_App_Category
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out App_Category_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := App_Category_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "App_Category.System") then
         R.Expect_Word (Source, Position, OK, "App_Category.System");
         Item := System;
      elsif R.Next_Is_Word (Source, Position, "App_Category.Development") then
         R.Expect_Word (Source, Position, OK, "App_Category.Development");
         Item := Development;
      elsif R.Next_Is_Word (Source, Position, "App_Category.Web") then
         R.Expect_Word (Source, Position, OK, "App_Category.Web");
         Item := Web;
      elsif R.Next_Is_Word (Source, Position, "App_Category.Games") then
         R.Expect_Word (Source, Position, OK, "App_Category.Games");
         Item := Games;
      elsif R.Next_Is_Word (Source, Position, "App_Category.Media") then
         R.Expect_Word (Source, Position, OK, "App_Category.Media");
         Item := Media;
      elsif R.Next_Is_Word (Source, Position, "App_Category.Tools") then
         R.Expect_Word (Source, Position, OK, "App_Category.Tools");
         Item := Tools;
      else
         OK := False;
      end if;
   end Read_App_Category;

   procedure Read_Color_Channel
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Color_Channel_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Color_Channel_Value'First), Interfaces.Integer_64 (Color_Channel_Value'Last), Value);
      Item := Color_Channel_Value (Value);
   end Read_Color_Channel;

   procedure Read_RGB_Color
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out RGB_Color_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(RGB_Color");
      R.Expect (Source, Position, OK, " red => ");
      Read_Color_Channel (Source, Position, OK, Item.red);
      R.Expect (Source, Position, OK, " green => ");
      Read_Color_Channel (Source, Position, OK, Item.green);
      R.Expect (Source, Position, OK, " blue => ");
      Read_Color_Channel (Source, Position, OK, Item.blue);
      R.Expect (Source, Position, OK, ")");
   end Read_RGB_Color;

   procedure Read_App_Color
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out App_Color_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := (others => <>);
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "App_Color.Theme_Accent") then
         R.Expect_Word (Source, Position, OK, "App_Color.Theme_Accent");
         Item.Choice := Theme_Accent;
      elsif R.Next_Is (Source, Position, "(App_Color.RGB ") then
         R.Expect (Source, Position, OK, "(App_Color.RGB ");
         Read_RGB_Color (Source, Position, OK, Item.RGB_Payload);
         R.Expect (Source, Position, OK, ")");
         Item.Choice := RGB;
      else
         OK := False;
      end if;
   end Read_App_Color;

   procedure Read_Launch_Entry
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Launch_Entry_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Launch_Entry");
      R.Expect (Source, Position, OK, " label => ");
      R.Read_Text (Source, Position, OK, Item.label);
      R.Expect (Source, Position, OK, " action => ");
      Read_Launch_Action (Source, Position, OK, Item.action);
      R.Expect (Source, Position, OK, " icon => ");
      Read_Launch_Icon (Source, Position, OK, Item.icon);
      R.Expect (Source, Position, OK, " category => ");
      Read_App_Category (Source, Position, OK, Item.category);
      R.Expect (Source, Position, OK, " single_instance => ");
      R.Read_Boolean (Source, Position, OK, Item.single_instance);
      R.Expect (Source, Position, OK, " color => ");
      Read_App_Color (Source, Position, OK, Item.color);
      R.Expect (Source, Position, OK, ")");
   end Read_Launch_Entry;

   procedure Decode (Source : String; Item : out Launch_Entry_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Launch_Entry (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Launch_Entry;

