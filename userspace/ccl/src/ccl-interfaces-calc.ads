with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;
with CCL.Interfaces.Calc_Types;

--  calc.* (docs/calculator.md, "One core, three faces"): the calculator's
--  evaluation as CCL operations. (calc.evaluate "2/3 + 1/6"), or the
--  command form (calc "2/3 + 1/6"), runs the same core (userspace/lib/calc,
--  Calc_Engine) as the Calculator app in its standard mode, so a script and
--  the app give the same answer: an exact rational, or an approximation
--  with its stated error bound, as a typed record. calc.evaluate-with
--  takes a Calc_Request: the expression and the app's options (scientific
--  angle unit, programmer word size and sign). Pure computation: no
--  authority, no service, no state.
package CCL.Interfaces.Calc with SPARK_Mode is

   --  What Publish adds to a catalog: one interface and this many data
   --  schemas. CCL_Host_Environment sums these to check the catalog's
   --  capacity at compile time; tests/ccl-install's probe checks a real run.
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 4;
   use Standard.Interfaces;

   --  The types, as CCL source (CCL.Interfaces.Calc_Types, which the
   --  typed setting reads too): checked by the CCL type checker when the
   --  interface is published. The digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   TYPE_SOURCE : constant String := Calc_Types.TYPE_SOURCE;
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#EE50_7739_C9ED_1A5F#,
      16#A2F1_652A_9317_83BF#,
      16#0E49_B51E_454B_B48E#,
      16#D69E_7C9E_23FC_BD4B#];
   FORM_KEY : constant CCL.Objects.Schema_Key :=
     [16#839F_693F_93E3_13C0#,
      16#8E47_4F5F_484B_D6DB#,
      16#38D2_7F98_0A56_5E61#,
      16#5462_81C5_0F87_5132#];
   NUMBER_KEY : constant CCL.Objects.Schema_Key :=
     [16#79C6_142B_E584_9936#,
      16#9FD6_F19C_58D5_25FA#,
      16#AC7F_B6AE_0B39_689C#,
      16#D815_B20B_DB32_E02D#];
   OPTIONS_KEY : constant CCL.Objects.Schema_Key :=
     [16#C794_A1A3_158A_515C#,
      16#FBF2_FF24_1356_4A44#,
      16#12F3_7EB4_A0A4_7D2C#,
      16#9A4D_7909_B83E_0280#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#D45C_AEC7_37A1_858B#,
      16#E010_3640_AB05_3B7D#,
      16#9496_AA8E_5C18_C9F5#,
      16#B963_345F_6F6F_78AE#];
   NUMBER_FIELDS : constant := 10;
   ROOT_OPTIONS_NAME : constant String := Calc_Types.ROOT_OPTIONS_NAME;

   --  The members, in declaration order (the app's Ada side).
   type Mode_Member is (Standard, Scientific, Programmer);
   type Angle_Member is (Radians, Degrees, Gradians);
   type Word_Member is (Unbounded, Bits_8, Bits_16, Bits_32, Bits_64);
   type Base_Member is (Decimal, Hexadecimal, Octal, Binary);

   --  The interface's declared limits (part of its contract, listed in
   --  docs/calculator.md): the most limbs (32 bits) a numerator or
   --  denominator may reach at any step, about 39,000 decimal digits; the
   --  digits approximations are computed to; the places of the decimal
   --  field. The whole result must also fit one CCL value's text
   --  (CCL.Objects.Maximum_Text_Bytes).
   BUDGET : constant := 4_096;
   PRECISION : constant := 50;
   PLACES : constant := 20;

   --  Calc is the REPL's short form, (calc "2/3 + 1/6"): the same
   --  evaluation as calc.evaluate, callable by its bare name.
   type Operation is (Evaluate, Calc, Evaluate_With);
   function Name (Item : Operation) return String is
     (case Item is when Evaluate => "evaluate", when Calc => "calc", when Evaluate_With => "evaluate-with");
   --  The operations callable by their bare names (CCL.Catalog.Mark_Command).
   function Is_Command (Item : Operation) return Boolean is (Item = Calc);
   FIRST_BINDING : constant Unsigned_32 := 16#000C_0001#;
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Form, Number, Options, Request : CCL.Objects.Binding;
   end record;
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Calc;
