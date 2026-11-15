pragma Ada_2022;
with CuBit.Child_Specs;

--  The fact/policy split of a device-class launch (SUP-001). The startup
--  plan's class launch, (driver-class "<ID>" <value>), is one typed CCL
--  value (CCL.Configurations.Class_Block):
--
--    (Device_Class_Launch facts => ["<field>" ...] policy => (<Record> ...))
--
--  policy writes the policy fields of the child's parameter record and
--  facts names the fields the supervisor (devmgr) fills with what it read
--  from the device. Its type involves the child's own record, so it is
--  checked when the child is started (procmgr's Class_Launch_Checks):
--
--  1. Policy_Schema: the child's signature with its root record WITHOUT the
--     supervisor's fact fields, plus the Device_Class_Launch record whose
--     policy field has that root type. CCL's type checker then refuses a
--     policy naming a fact (no such field) or leaving a policy field out
--     (a missing field), and renders the value canonical.
--  2. Read_Launch: the canonical value's facts are exactly the
--     supervisor's, and where its policy is.
--  3. Merge (below) appends the fact values; procmgr checks the merged
--     record against the child's full signature as any typed parameters.
--
--  Merge checks that the facts given are exactly the facts declared (each
--  once), then appends them to the policy value as named associations:
--
--     (Rec x => 1 y => 2)  +  d => 32   =  (Rec x => 1 y => 2 d => 32)
--
--  The merged source is then checked against the child's declared record
--  type by CCL's type checker (CCL.Typed_Values, the same check as a plan
--  entry's (arguments ...)): a policy value that also names a fact field
--  is a repeated field there, and a policy field nobody wrote is missing.
--  So neither side can set or fill the other's fields. Fact values are
--  integers rendered here, never source text from the supervisor.
--  No I/O; procmgr calls it.
package Child_Parameters with SPARK_Mode is
   package CS renames CuBit.Child_Specs;

   --  A merged value fits one startup-plan value (CCL.Configurations
   --  Value_Text).
   Maximum_Merged_Bytes : constant := CS.Maximum_Value_Bytes;
   --  The facts a class block may declare.
   Maximum_Declared : constant := CS.Maximum_Facts;

   subtype Declared_Count is Natural range 0 .. Maximum_Declared;
   type Declared_Array is array (1 .. Maximum_Declared) of CS.Field_Text;

   type Merge_Status is
     (Merged,
      Policy_Not_Record,     --  the policy is not one parenthesized form
      Declaration_Invalid,   --  a declared fact is not a field name, or is declared twice
      Fact_Not_Declared,     --  the supervisor gave a fact the block does not declare
      Fact_Missing,          --  a declared fact was not given
      Too_Long);             --  the merged value does not fit Maximum_Merged_Bytes

   function Declares (Declared : Declared_Array; Total : Declared_Count; Field : CS.Field_Text)
     return Boolean is
     (for some I in 1 .. Total => CS.Same (Declared (I), Field));

   function Given (Facts : CS.Fact_Array; Total : CS.Fact_Count; Field : CS.Field_Text)
     return Boolean is
     (for some I in 1 .. Total => CS.Same (Facts (I).Field, Field));

   --  The two sets are equal: every fact given is declared, every declared
   --  fact is given.
   function Same_Facts
     (Declared : Declared_Array; Declared_Total : Declared_Count;
      Facts : CS.Fact_Array; Fact_Total : CS.Fact_Count) return Boolean is
     ((for all I in 1 .. Fact_Total => Declares (Declared, Declared_Total, Facts (I).Field))
      and then (for all I in 1 .. Declared_Total => Given (Facts, Fact_Total, Declared (I))));

   procedure Merge
     (Policy : String;
      Declared : Declared_Array; Declared_Total : Declared_Count;
      Facts : CS.Fact_Array; Fact_Total : CS.Fact_Count;
      Into : out String; Length : out Natural; Status : out Merge_Status)
     with Pre  => Into'First = 1 and then Into'Length = Maximum_Merged_Bytes
                  and then Policy'Length <= Maximum_Merged_Bytes,
          Post => Length <= Maximum_Merged_Bytes
                  and then (if Status = Merged then
                              Length >= 2 and then Into (1) = '(' and then Into (Length) = ')'
                              and then Same_Facts (Declared, Declared_Total, Facts, Fact_Total)
                            else Length = 0);

   ---------------------------------------------------------------------------
   --  The class launch's schema and its canonical value.
   ---------------------------------------------------------------------------
   --  A child's signature (CuBit.Parameter_Signatures.Maximum_Signature_Bytes)
   --  and the schema built from it: the signature less the fact fields,
   --  plus the Device_Class_Launch declaration.
   Maximum_Signature_Bytes : constant := 2_048;
   Launch_Type : constant String := "Device_Class_Launch";
   Maximum_Root_Bytes : constant := 64;
   Launch_Declaration_Bytes : constant := 96 + Maximum_Root_Bytes;
   Maximum_Schema_Bytes : constant := Maximum_Signature_Bytes + Launch_Declaration_Bytes;

   type Schema_Status is
     (Built,
      Root_Not_Record,   --  the signature declares no (type <Root> (record ...))
      Fact_Not_Field,    --  a fact names no field of the root record, or one twice
      Schema_Too_Long);

   --  Signature with Root's record declaration less the fields Facts name,
   --  then (type Device_Class_Launch (record (facts (List String))
   --  (policy <Root>))). Whitespace between forms is kept as written; a
   --  field's default may be a string with parentheses inside.
   procedure Policy_Schema
     (Signature, Root : String;
      Facts : CS.Fact_Array; Fact_Total : CS.Fact_Count;
      Into : out String; Length : out Natural; Status : out Schema_Status)
     with Pre  => Into'First = 1 and then Into'Length = Maximum_Schema_Bytes
                  and then Signature'Length <= Maximum_Signature_Bytes
                  and then Signature'Last < Positive'Last
                  and then Root'Length in 1 .. Maximum_Root_Bytes,
          Post => Length <= Maximum_Schema_Bytes
                  and then (if Status /= Built then Length = 0);

   type Read_Status is
     (Read,
      Not_Canonical,     --  not the canonical form of a Device_Class_Launch
      Facts_Differ);     --  its facts are not exactly the supervisor's

   --  Canonical: the checker's rendering of a Device_Class_Launch,
   --    (Device_Class_Launch facts => ["a" "b"] policy => (<Record> ...))
   --  Policy_First .. Policy_Last: where its policy value is.
   procedure Read_Launch
     (Canonical : String; Facts : CS.Fact_Array; Fact_Total : CS.Fact_Count;
      Policy_First, Policy_Last : out Natural; Status : out Read_Status)
     with Pre  => Canonical'Last < Positive'Last,
          Post => (if Status = Read then
                     Policy_First in Canonical'Range and then Policy_Last in Canonical'Range
                     and then Policy_First < Policy_Last
                     and then Canonical (Policy_First) = '(' and then Canonical (Policy_Last) = ')');

   function Schema_Status_Name (Status : Schema_Status) return String is
     (case Status is
        when Built           => "built",
        when Root_Not_Record => "its parameter signature declares no record for its root",
        when Fact_Not_Field  => "a fact the supervisor gives is no field of its parameter record",
        when Schema_Too_Long => "its parameter signature is too long for a class launch's schema");
   function Read_Status_Name (Status : Read_Status) return String is
     (case Status is
        when Read          => "read",
        when Not_Canonical => "the class launch is not a canonical Device_Class_Launch",
        when Facts_Differ  => "the class launch's facts are not exactly the supervisor's");

   function Status_Name (Status : Merge_Status) return String is
     (case Status is
        when Merged              => "merged",
        when Policy_Not_Record   => "the class block's parameters are not one record value",
        when Declaration_Invalid => "the class block declares a fact that is no field name, or one twice",
        when Fact_Not_Declared   => "a fact given is not one the class block declares",
        when Fact_Missing        => "a fact the class block declares was not given",
        when Too_Long            => "policy and facts together are too long for one value");
end Child_Parameters;
