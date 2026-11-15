--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CuBit.Canonical_Readers;

--  The typed setting desktop.composition: a CCL Desktop_Composition (CCL.Interfaces.GPU_Power.COMPOSITION_TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CuBit.Canonical_Readers).
package CCL.Setting_Codecs.Desktop_Composition with SPARK_Mode, Pure is
   subtype Text is CuBit.Canonical_Readers.Text;
   type Composition_Path_Value is (CPU, GPU);
   type Desktop_Composition_Value is record
      path : Composition_Path_Value := Composition_Path_Value'First;
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Desktop_Composition. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Desktop_Composition_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Desktop_Composition;

