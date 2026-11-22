--  Compare evaluated system declarations, without I/O or activation authority.
package CCL.Configurations.Changes with SPARK_Mode => On is
   type Setting_Change is (Unchanged, Added, Replaced);
   type Candidate_Changes is
     array (Positive range 1 .. MAX_SETTINGS) of Setting_Change;
   type Removed_Settings is
     array (Positive range 1 .. MAX_SETTINGS) of Boolean;
   type Review is record
      Accepted : Boolean := False;
      Candidate : Candidate_Changes := [others => Unchanged];
      Removed : Removed_Settings := [others => False];
   end record;

   --  Candidate entries index After.Plan.Settings; Removed entries index
   --  Before.Plan.Settings. Retain those immutable owned plans while displaying
   --  this review. No values, grants, or executable expressions are copied.
   --  Only successful system-config compilations are accepted, not startup
   --  plans. Unused entries remain neutral. Reordering settings is not a change.
   --  v1 stores evaluated strings: this is value comparison, not an AST diff.
   function Compare (Before, After : Compilation_Result) return Review;
end CCL.Configurations.Changes;
