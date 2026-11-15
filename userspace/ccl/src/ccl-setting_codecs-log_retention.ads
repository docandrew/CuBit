--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CuBit.Canonical_Readers;

--  The typed setting logs.retention: a CCL Log_Retention (CCL.Interfaces.Log_Retention.TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CuBit.Canonical_Readers).
package CCL.Setting_Codecs.Log_Retention with SPARK_Mode, Pure is
   subtype Text is CuBit.Canonical_Readers.Text;
   type Retained_Per_Source_Value is range 1024 .. 1048576;
   type Retained_Boot_Value is range 64 .. 65536;
   type Retained_Total_Value is range 8192 .. 16777216;
   type Log_Retention_Value is record
      per_source : Retained_Per_Source_Value := Retained_Per_Source_Value'First;
      boot : Retained_Boot_Value := Retained_Boot_Value'First;
      total : Retained_Total_Value := Retained_Total_Value'First;
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Log_Retention. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Log_Retention_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Log_Retention;

