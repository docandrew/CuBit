--  logstore's retention as a CCL type (LOG-005): `logs.retention` is one
--  Log_Retention, checked when the configuration is compiled
--  (CCL.Typed_Settings) and read back by logstore with the decoder
--  generated from these declarations (CCL.Setting_Codecs.Log_Retention,
--  Logstore_Retention_Settings). Each field is a ceiling in records: what
--  one source keeps, the first records of each source that are never given
--  up for its later ones, and what every source together keeps. The ranges
--  are Log_Fanout's: the least of each is what logstore keeps until it has
--  read the setting, so a plan can only raise them. No field has a default:
--  a plan writes all three.
package CCL.Interfaces.Log_Retention with SPARK_Mode, Pure is
   TYPE_SOURCE : constant String :=
     "(type Retained_Per_Source (range 1024 1048576)) " &
     "(type Retained_Boot (range 64 65536)) " &
     "(type Retained_Total (range 8192 16777216)) " &
     "(type Log_Retention (record (per_source Retained_Per_Source) " &
     "(boot Retained_Boot) (total Retained_Total)))";
   ROOT_TYPE_NAME : constant String := "Log_Retention";
end CCL.Interfaces.Log_Retention;
