--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CCL.Canonical_Readers;

--  The typed setting desktop.display.modeset: a CCL Display_Modeset (CCL.Interfaces.Desktop_Display.MODESET_TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CCL.Canonical_Readers).
package CCL.Setting_Codecs.Display_Modeset with SPARK_Mode, Pure is
   subtype Text is CCL.Canonical_Readers.Text;
   type Display_Modeset_Value is record
      enabled : Boolean := False;
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Display_Modeset. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Display_Modeset_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Display_Modeset;

