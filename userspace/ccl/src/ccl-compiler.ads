with CCL.Language;
with CCL.Catalog;
with CCL.Debug_Maps;
with CCL.VM;

package CCL.Compiler with
   SPARK_Mode => On
is
   type Compilation_Status is
     (Compilation_Succeeded,
      Analysis_Failed,
      Unsupported_Form,
      Malformed_Typed_Tree,
      Too_Many_Locals,
      Too_Many_Imports,
      Debug_Map_Full,
      Program_Full);

   type Compilation_Result is record
      Status          : Compilation_Status := Analysis_Failed;
      Diagnostic_Node : CCL.Language.Node_Reference := CCL.Language.NO_NODE;
      Source_Position : CCL.Language.Source_Position := 0;
      Program         : CCL.VM.Program;
      Linkage         : CCL.Catalog.Linkage_Table;
      Debug           : CCL.Debug_Maps.Debug_Map;
   end record;

   procedure Compile
     (Analysis : CCL.Language.Analysis_Result;
      Result   : out Compilation_Result);
end CCL.Compiler;
