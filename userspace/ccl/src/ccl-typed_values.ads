with CuBit.Canonical_Readers;

--  Typed CCL values checked against declared types (docs/ccl-boot-configuration.md,
--  "Typed settings"; docs/ccl-launch-parameters.md): a value's expression
--  is analysed and evaluated with the CCL declarations of its type and
--  rendered as its canonical source (every record field named, in
--  declaration order; sums as Type.Member or (Type.Member payload)), the one
--  spelling the decoders ccl-manifest generates (over
--  CuBit.Canonical_Readers) accept. Typed settings (CCL.Typed_Settings) and
--  typed program parameters (a launcher's arguments against a program's
--  .cubit.parameters signature) are both checked here.
package CCL.Typed_Values is
   MAXIMUM_CANONICAL : constant := CuBit.Canonical_Readers.Maximum_Canonical;
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

   --  Check Source (one expression) against Root_Name, declared (with the
   --  types it reaches) by Schema, a sequence of CCL (type ...) forms.
   procedure Check (Schema, Root_Name, Source : String; Result : out Check_Result);
end CCL.Typed_Values;
