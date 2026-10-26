--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CuBit.Canonical_Readers;

--  The typed setting gpu.power.waitboost: a CCL GPU_Waitboost (CCL.Interfaces.GPU_Power.TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CuBit.Canonical_Readers).
package CCL.Setting_Codecs.GPU_Waitboost with SPARK_Mode, Pure is
   subtype Text is CuBit.Canonical_Readers.Text;
   type GPU_Waitboost_Value is record
      enabled : Boolean := False;
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a GPU_Waitboost. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out GPU_Waitboost_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.GPU_Waitboost;

