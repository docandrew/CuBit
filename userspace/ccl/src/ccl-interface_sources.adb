with CCL.Language;

package body CCL.Interface_Sources with SPARK_Mode is
   use type CCL.Language.Analysis_Status;
   use type CCL.Types.Import_Result;

   procedure Declare_Types (Source : String; Types : in out CCL.Types.Registry; Accepted : out Boolean) is
      Checked : CCL.Language.Analysis_Result;
      Declared : CCL.Types.Registry;
      Imported : CCL.Types.Type_Reference;
      Result : CCL.Types.Import_Result;
   begin
      --  The declarations, then a trivial expression: one checked program.
      CCL.Language.Analyze (Source & " 0", Checked);
      Accepted := CCL.Language.Analysis_Status_Of (Checked) = CCL.Language.Analysis_Succeeded;
      if not Accepted then return; end if;
      Declared := CCL.Language.Analysis_Types (Checked);
      for Ref in CCL.Types.Declared_Type'First .. CCL.Types.Last (Declared) loop
         CCL.Types.Import_Definition (Declared, Ref, Types, Imported, Result);
         Accepted := Result = CCL.Types.Imported;
         exit when not Accepted;
      end loop;
   end Declare_Types;
end CCL.Interface_Sources;
