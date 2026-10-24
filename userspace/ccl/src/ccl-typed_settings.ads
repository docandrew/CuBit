with CCL.Canonical_Readers;
with CCL.Setting_Kinds;

--  Settings whose values are typed CCL values (docs/ccl-boot-configuration.md,
--  "Typed settings"): a key prefix names the declared type its value must
--  have. CCL.Configurations checks the value's expression against that type
--  with the CCL analyser and stores the value's canonical source (one
--  spelling per value: every field named, in declaration order). Readers
--  never run CCL: each reads that source with the decoder ccl-manifest
--  generated from the same type at build time (CCL.Setting_Codecs.<Root>,
--  over CCL.Canonical_Readers), which accepts only the canonical spelling.
--  Which keys are typed, and with which type, is the setting catalog's
--  (userspace/ccl/interfaces/settings/, CAP-003): CCL.Setting_Kinds is
--  generated from it.
package CCL.Typed_Settings is
   subtype Setting_Kind is CCL.Setting_Kinds.Setting_Kind;
   use all type CCL.Setting_Kinds.Setting_Kind;
   function Kind_Of (Key : String) return Setting_Kind renames CCL.Setting_Kinds.Kind_Of;

   MAXIMUM_CANONICAL : constant := CCL.Canonical_Readers.Maximum_Canonical;
   MAXIMUM_MESSAGE : constant := 160;
   type Check_Result is record
      Success : Boolean := False;
      Canonical : String (1 .. MAXIMUM_CANONICAL) := [others => ' '];
      Length : Natural range 0 .. MAXIMUM_CANONICAL := 0;
      --  Where in the value's source the checker stopped, from 1.
      Position : Natural := 0;
      Message : String (1 .. MAXIMUM_MESSAGE) := [others => ' '];
      Message_Length : Natural range 0 .. MAXIMUM_MESSAGE := 0;
   end record;

   --  Check Source (one expression) against Kind's declared type.
   procedure Check (Kind : Setting_Kind; Source : String; Result : out Check_Result)
     with Pre => Kind /= Untyped;

end CCL.Typed_Settings;
