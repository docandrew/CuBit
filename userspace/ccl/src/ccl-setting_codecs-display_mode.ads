--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CuBit.Canonical_Readers;

--  The typed setting desktop.display.mode.<member>: a CCL Display_Mode (CCL.Interfaces.Desktop_Display.MODE_TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CuBit.Canonical_Readers).
package CCL.Setting_Codecs.Display_Mode with SPARK_Mode, Pure is
   subtype Text is CuBit.Canonical_Readers.Text;
   type Mode_Extent_Value is range 1 .. 65535;
   type Refresh_Millihertz_Value is range 1 .. 4294967295;
   type Display_Mode_Choice_Value is record
      width : Mode_Extent_Value := Mode_Extent_Value'First;
      height : Mode_Extent_Value := Mode_Extent_Value'First;
      refresh_millihertz : Refresh_Millihertz_Value := Refresh_Millihertz_Value'First;
   end record;
   type Display_Mode_Choice is (Auto, Fixed);
   type Display_Mode_Value is record
      Choice : Display_Mode_Choice := Display_Mode_Choice'First;
      Fixed_Payload : Display_Mode_Choice_Value := (others => <>);
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Display_Mode. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Display_Mode_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Display_Mode;

