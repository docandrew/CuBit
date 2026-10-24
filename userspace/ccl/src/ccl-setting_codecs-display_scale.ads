--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CCL.Canonical_Readers;

--  The typed setting desktop.display.scale: a CCL Display_Scale (CCL.Interfaces.Desktop_Display.TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CCL.Canonical_Readers).
package CCL.Setting_Codecs.Display_Scale with SPARK_Mode, Pure is
   subtype Text is CCL.Canonical_Readers.Text;
   type Scale_Part_Value is range 1 .. 16;
   type UI_Scale_Value is record
      numerator : Scale_Part_Value := Scale_Part_Value'First;
      denominator : Scale_Part_Value := Scale_Part_Value'First;
   end record;
   type Display_Scale_Choice is (Auto, Fixed);
   type Display_Scale_Value is record
      Choice : Display_Scale_Choice := Display_Scale_Choice'First;
      Fixed_Payload : UI_Scale_Value := (others => <>);
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Display_Scale. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Display_Scale_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Display_Scale;

