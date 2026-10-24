--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CCL.Canonical_Readers;

--  The typed setting desktop.launch.<member>: a CCL Launch_Entry (CCL.Interfaces.Desktop_Launch.TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CCL.Canonical_Readers).
package CCL.Setting_Codecs.Launch_Entry with SPARK_Mode, Pure is
   subtype Text is CCL.Canonical_Readers.Text;
   type Launch_Action_Choice is (Program, Settings);
   type Launch_Action_Value is record
      Choice : Launch_Action_Choice := Launch_Action_Choice'First;
      Program_Payload : Text := CCL.Canonical_Readers.Empty_Text;
   end record;
   type Launch_Icon_Value is record
      name : Text := CCL.Canonical_Readers.Empty_Text;
   end record;
   type App_Category_Value is (System, Development, Web, Games, Media, Tools);
   type Color_Channel_Value is range 0 .. 255;
   type RGB_Color_Value is record
      red : Color_Channel_Value := Color_Channel_Value'First;
      green : Color_Channel_Value := Color_Channel_Value'First;
      blue : Color_Channel_Value := Color_Channel_Value'First;
   end record;
   type App_Color_Choice is (Theme_Accent, RGB);
   type App_Color_Value is record
      Choice : App_Color_Choice := App_Color_Choice'First;
      RGB_Payload : RGB_Color_Value := (others => <>);
   end record;
   type Launch_Entry_Value is record
      label : Text := CCL.Canonical_Readers.Empty_Text;
      action : Launch_Action_Value := (others => <>);
      icon : Launch_Icon_Value := (others => <>);
      category : App_Category_Value := App_Category_Value'First;
      single_instance : Boolean := False;
      color : App_Color_Value := (others => <>);
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Launch_Entry. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Launch_Entry_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Launch_Entry;

