with CCL.Objects.Views;

--  Settings whose values are typed CCL values (docs/ccl-boot-configuration.md,
--  "Typed settings"): a key prefix names the declared type its value must
--  have. CCL.Configurations checks the value's expression against that type
--  with the CCL analyser and stores the value's canonical source (one
--  spelling per value: every field named, in declaration order). A reader
--  (the desktop) reads that source back through the same typed path and
--  accepts it only if it is that canonical spelling, so a stored value is
--  never parsed by hand and never in any other form.
--  First user: desktop.launch.* (CCL.Interfaces.Desktop_Launch).
package CCL.Typed_Settings is
   type Setting_Kind is (Untyped, Launch_Entry_Setting);
   function Kind_Of (Key : String) return Setting_Kind;

   MAXIMUM_CANONICAL : constant := 1_024;
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

   --  Read a stored value: Object holds it (its root is Kind's type) when
   --  Success, which needs Canonical to be exactly the value's canonical
   --  source.
   procedure Read
     (Kind : Setting_Kind; Canonical : String; Object : in out CCL.Objects.Views.Snapshot; Success : out Boolean)
     with Pre => Kind /= Untyped;

   --  Readers' helpers over a captured value.
   function Named
     (Object : CCL.Objects.Views.Snapshot; At_Cursor : CCL.Objects.Views.Cursor; Name : String)
      return CCL.Objects.Views.Cursor;
   --  An enum or variant's alternative name ("Web").
   function Alternative_Name
     (Object : CCL.Objects.Views.Snapshot; At_Cursor : CCL.Objects.Views.Cursor) return String;
end CCL.Typed_Settings;
