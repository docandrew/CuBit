pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Metric_Protocol;
package CuBit.Metric_Raw_Validation with Pure, SPARK_Mode is
   package P renames CuBit.Metric_Protocol;
   function Valid (Page : P.Raw_Page; Cursor, Written, Next, Gap : Unsigned_64)
      return Boolean;
end CuBit.Metric_Raw_Validation;
