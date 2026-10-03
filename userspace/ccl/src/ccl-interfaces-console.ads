with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  The console as an object cells can program:
--  its window title, its notation (Lisp or BASIC), its theme and its own
--  statistics, from CCL, like any other typed interface. Only the console
--  process publishes and grants it; it reaches nothing outside the console.
package CCL.Interfaces.Console with SPARK_Mode is
   use Standard.Interfaces;

   --  The types, as CCL source: checked by the CCL type checker when the
   --  interface is published. The digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   TYPE_SOURCE : constant String :=
     "(type Notation (enum Lisp Basic)) (type Theme (enum Midnight Daylight)) (type " &
     "Milliseconds (range 0 9223372036854775807)) (type Console_Stats (record (cells " &
     "Integer) (live Integer) (live_runs Integer) (last_ms Milliseconds) (slowest_ms " &
     "Milliseconds) (streams Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#0C41_3473_843A_11AA#,
      16#E46F_6565_5A14_FF2C#,
      16#3FC5_B572_074D_F91A#,
      16#E4DC_415B_FCFE_8080#];
   NOTATION_KEY : constant CCL.Objects.Schema_Key :=
     [16#0BB9_1AEE_C3FE_84CD#,
      16#ED8E_598E_7CC2_6F44#,
      16#1669_3D67_BA8B_894D#,
      16#43D8_3E85_CBC6_5B6C#];
   THEME_KEY : constant CCL.Objects.Schema_Key :=
     [16#0210_DD65_8B75_B0CB#,
      16#0384_023D_1D6D_0C24#,
      16#D580_EE76_AA36_F284#,
      16#1B7D_92FA_EC74_F3B1#];
   STATS_KEY : constant CCL.Objects.Schema_Key :=
     [16#6A36_9801_361D_8A79#,
      16#3321_F88A_2F94_4E51#,
      16#7386_933D_E885_70AF#,
      16#756F_F69B_2AC0_6A22#];

   type Notation is (Lisp, Basic);
   type Theme is (Midnight, Daylight);

   --  What console.stats reports.
   STATS_FIELDS : constant := 6;
   type Statistics is record
      Cells, Live, Live_Runs : Natural := 0;
      Last_Ms, Slowest_Ms : Natural := 0;
      Streams : Natural := 0;
   end record;

   type Operation is (Title, Set_Notation, Set_Theme, Stats);
   function Name (Item : Operation) return String is
     (case Item is
         when Title => "title", when Set_Notation => "notation",
         when Set_Theme => "theme", when Stats => "stats");
   FIRST_BINDING : constant Unsigned_32 := 16#0007_0001#;
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));
   MAX_TITLE : constant := 64;

   type Contracts is record
      Notation, Theme, Stats : CCL.Objects.Binding;
   end record;
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);

   --  Values as images under their contracts.
   procedure Notation_Value
     (Contract : CCL.Objects.Binding; Value : Notation;
      Result : out CCL.Objects.Image; Built : out Boolean);
   procedure Theme_Value
     (Contract : CCL.Objects.Binding; Value : Theme;
      Result : out CCL.Objects.Image; Built : out Boolean);
   procedure Stats_Value
     (Contract : CCL.Objects.Binding; Value : Statistics;
      Result : out CCL.Objects.Image; Built : out Boolean);
   --  The member an enumeration image holds (Found False if it is not one
   --  of Count members under Contract).
   procedure Member_Of
     (Contract : CCL.Objects.Binding; Value : CCL.Objects.Image; Count : Positive;
      Member : out Positive; Found : out Boolean);
end CCL.Interfaces.Console;
