with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  calc.* (docs/calculator.md, "One core, three faces"): the calculator's
--  evaluation as a CCL operation. (calc.evaluate "2/3 + 1/6"), or the
--  command form (calc "2/3 + 1/6"), runs the same core (userspace/lib/calc,
--  Calc_Engine) as the Calculator app, so a script and the app give the
--  same answer: an exact rational, or one marked approximate (sqrt of a
--  non-square, pi, e), as a typed record.
--  Pure computation: no authority, no service, no state.
package CCL.Interfaces.Calc with SPARK_Mode is
   use Standard.Interfaces;

   --  The types, as CCL source: checked by the CCL type checker when the
   --  interface is published. The digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   --  numerator: the signed numerator in decimal; denominator: positive,
   --  "1" for an integer; decimal: the value at PLACES places
   --  (Calc_Format: 0.8(3) repeats, 1.5e-30, rounded when form says so).
   TYPE_SOURCE : constant String :=
     "(type Calc_Form (enum Integer_Form Terminating Repeating Scientific_Exact Rounded " &
     "Scientific_Rounded)) (type Calc_Number (record (exact Boolean) (numerator String) " &
     "(denominator String) (decimal String) (form Calc_Form)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#DB4C_A826_C821_F8E6#,
      16#0E26_A58C_0E8D_CC71#,
      16#0CB7_B858_9C90_8A71#,
      16#1BDC_4A80_C8BC_D6DF#];
   FORM_KEY : constant CCL.Objects.Schema_Key :=
     [16#4E2C_ED02_AAD9_6AEE#,
      16#90F1_AC65_3467_D71A#,
      16#25EE_A01C_2F30_A4AC#,
      16#1698_F763_C250_3F35#];
   NUMBER_KEY : constant CCL.Objects.Schema_Key :=
     [16#460E_5E85_168C_F7A8#,
      16#AC93_44D9_9FDA_0F81#,
      16#6EB9_6747_9BBF_244E#,
      16#E08E_B8DC_89F0_7A51#];
   NUMBER_FIELDS : constant := 5;

   --  The interface's declared limits (part of its contract, listed in
   --  docs/calculator.md): the most limbs (32 bits) a numerator or
   --  denominator may reach at any step, about 39,000 decimal digits; the
   --  digits of an approximation; the places of the decimal field. The
   --  whole result must also fit one CCL value's text
   --  (CCL.Objects.Maximum_Text_Bytes).
   BUDGET : constant := 4_096;
   PRECISION : constant := 50;
   PLACES : constant := 20;

   --  Calc is the REPL's short form, (calc "2/3 + 1/6"): the same
   --  evaluation as calc.evaluate, callable by its bare name.
   type Operation is (Evaluate, Calc);
   function Name (Item : Operation) return String is
     (case Item is when Evaluate => "evaluate", when Calc => "calc");
   --  The operations callable by their bare names (CCL.Catalog.Mark_Command).
   function Is_Command (Item : Operation) return Boolean is (Item = Calc);
   FIRST_BINDING : constant Unsigned_32 := 16#000C_0001#;
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Form, Number : CCL.Objects.Binding;
   end record;
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Calc;
