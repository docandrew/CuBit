--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CCL.Canonical_Readers;

--  The typed setting desktop.input.key-repeat: a CCL Key_Repeat (CCL.Interfaces.Desktop_Input.TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CCL.Canonical_Readers).
package CCL.Setting_Codecs.Key_Repeat with SPARK_Mode, Pure is
   subtype Text is CCL.Canonical_Readers.Text;
   type Repeat_Delay_Value is range 100 .. 2000;
   type Repeat_Rate_Value is range 1 .. 100;
   type Key_Repeat_Value is record
      enabled : Boolean := False;
      delay_ms : Repeat_Delay_Value := Repeat_Delay_Value'First;
      rate_hz : Repeat_Rate_Value := Repeat_Rate_Value'First;
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Key_Repeat. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Key_Repeat_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Key_Repeat;

