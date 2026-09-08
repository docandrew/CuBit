with CCL.Language;

--  Stable human-readable text, independent of GNAT enumeration-name tables.
package CCL.Diagnostics with SPARK_Mode is
   function Message (Code : CCL.Language.Diagnostic_Code) return String;
   function Message (Status : CCL.Language.Interpretation_Status) return String;
end CCL.Diagnostics;
