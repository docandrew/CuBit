with CCL.Language;

--  A REPL or Workbench result as the clipboard takes it (docs/clipboard.md):
--  its type name, its text form for plain text fields, and the literal that
--  reads back as the same value. The console, the REPL view and the
--  Workbench copy results through these, and a click on a console result
--  inserts the same literal.
package CCL_Result_Copy is
   function Type_Name (Outcome : CCL.Language.Interpretation_Result) return String;
   function Text_Form (Outcome : CCL.Language.Interpretation_Result) return String;
   function Literal (Outcome : CCL.Language.Interpretation_Result) return String;
end CCL_Result_Copy;
