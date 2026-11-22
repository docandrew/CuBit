--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit.
pragma Ada_2022;
with CCL.Setting_Kinds;

--  Config's check of a typed setting's stored value: the generated
--  decoder of the key's type accepts it.
package CCL.Setting_Codecs.Checks with SPARK_Mode, Pure is
   --  Whether Source is the canonical source of a value of Kind's type.
   function Canonical (Kind : CCL.Setting_Kinds.Setting_Kind; Source : String) return Boolean
   with Pre => CCL.Setting_Kinds."/=" (Kind, CCL.Setting_Kinds.Untyped);
end CCL.Setting_Codecs.Checks;

